{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Rotas HTTP (o "garçom" do app).
--
-- Cada bloco do tipo @get "/caminho" $ ...@ ou @post "/caminho" $ ...@ diz:
-- "quando alguém acessar este endereço, faça isto". As funções pegam dados
-- do banco (via "Models") e mandam a View montar a página (via "Views").
--
-- get  = a pessoa está apenas vendo uma página.
-- post = a pessoa está enviando dados (um formulário).
module Routes where

import Control.Exception       (SomeException)
import Control.Monad.IO.Class  (liftIO)
import Data.Int                (Int64)
import Data.Text               (Text)
import qualified Data.Text     as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import Data.Time               (getCurrentTime, parseTimeM, defaultTimeLocale, utctDay, Day)
import qualified Database.Persist as P
import Database.Persist        ((==.), (=.), (<-.))
import Database.Persist.Sql    (fromSqlKey, toSqlKey, SqlPersistT, runSqlPool)
import Database.Persist.Sqlite (ConnectionPool)
import Models
import Auth                    (hashPassword, verifyPassword, generateToken)
import Text.Read               (readMaybe)
import Views hiding (tshow)
import Web.Cookie              (parseCookies)
import Web.Scotty

-- ─── DB helper ──────────────────────────────────────────────────────────────

runDB :: ConnectionPool -> SqlPersistT IO a -> IO a
runDB pool action = runSqlPool action pool

-- ─── Sessão / usuário atual ──────────────────────────────────────────────────
--
-- Como o login se mantém entre páginas: ao entrar, gravamos uma Session com um
-- token aleatório e enviamos esse token num cookie. A cada novo pedido, lemos o
-- cookie, achamos a sessão no banco e descobrimos QUEM é o usuário logado.

-- Lê o valor de um cookie específico enviado pelo navegador.
getCookie :: Text -> ActionM (Maybe Text)
getCookie name = do
  mh <- header "Cookie"
  case mh of
    Nothing -> return Nothing
    Just h  ->
      let cookies = parseCookies (encodeUtf8 (TL.toStrict h))
      in return (decodeUtf8 <$> lookup (encodeUtf8 name) cookies)

setSessionCookie :: Text -> ActionM ()
setSessionCookie tok =
  setHeader "Set-Cookie"
    (TL.fromStrict ("session=" <> tok <> "; Path=/; HttpOnly; SameSite=Lax; Max-Age=2592000"))

clearSessionCookie :: ActionM ()
clearSessionCookie =
  setHeader "Set-Cookie" "session=; Path=/; HttpOnly; Max-Age=0"

-- Descobre o usuário logado a partir do cookie de sessão (ou Nothing se não há).
currentUser :: ConnectionPool -> ActionM (Maybe (P.Entity User))
currentUser pool = do
  mTok <- getCookie "session"
  case mTok of
    Nothing  -> return Nothing
    Just tok -> liftIO $ runDB pool $ do
      mSess <- P.getBy (UniqueToken tok)
      case mSess of
        Nothing               -> return Nothing
        Just (P.Entity _ sess) ->
          let uid = sessionUserId sess
          in fmap (P.Entity uid) <$> P.get uid

-- Executa a ação apenas se houver usuário logado; senão manda para /login.
withUser :: ConnectionPool -> (P.Entity User -> ActionM ()) -> ActionM ()
withUser pool action = do
  mu <- currentUser pool
  case mu of
    Just u  -> action u
    Nothing -> redirect "/login"

-- Busca uma planta SOMENTE se ela pertence ao usuário informado.
-- É isso que impede um usuário de ver ou mexer nas plantas de outro.
getOwnedPlant :: ConnectionPool -> UserId -> Int64 -> IO (Maybe (P.Entity Plant))
getOwnedPlant pool uid pid = runDB pool $ do
  mp <- P.get (toSqlKey pid :: PlantId)
  return $ case mp of
    Just p | plantUserId p == uid -> Just (P.Entity (toSqlKey pid) p)
    _                             -> Nothing

-- ─── Rotas ──────────────────────────────────────────────────────────────────

appRoutes :: ConnectionPool -> ScottyM ()
appRoutes pool = do
  let db = runDB pool

  -- ── Autenticação ──────────────────────────────────────────────────────────

  get "/login" $ do
    mu <- currentUser pool
    case mu of
      Just _  -> redirect "/"
      Nothing -> html $ renderAuthPage $ loginView Nothing

  post "/login" $ do
    email <- (T.toLower . T.strip) <$> param "email" :: ActionM Text
    pw    <- param "password" :: ActionM Text
    now   <- liftIO getCurrentTime
    mUser <- liftIO $ db $ P.getBy (UniqueEmail email)
    case mUser of
      Just (P.Entity uid u) | verifyPassword pw (userPasswordHash u) -> do
        tok <- liftIO generateToken
        _   <- liftIO $ db $ P.insert $ Session tok uid now
        setSessionCookie tok
        redirect "/"
      _ -> html $ renderAuthPage $ loginView (Just "E-mail ou senha incorretos.")

  get "/cadastro" $ do
    mu <- currentUser pool
    case mu of
      Just _  -> redirect "/"
      Nothing -> html $ renderAuthPage $ signupView Nothing

  post "/cadastro" $ do
    name  <- T.strip <$> param "name" :: ActionM Text
    email <- (T.toLower . T.strip) <$> param "email" :: ActionM Text
    pw    <- param "password" :: ActionM Text
    now   <- liftIO getCurrentTime
    if T.null name || not ("@" `T.isInfixOf` email) || T.length pw < 6
      then html $ renderAuthPage $ signupView
             (Just "Preencha nome, um e-mail válido e uma senha com ao menos 6 caracteres.")
      else do
        existing <- liftIO $ db $ P.getBy (UniqueEmail email)
        case existing of
          Just _  -> html $ renderAuthPage $ signupView
                       (Just "Já existe uma conta com esse e-mail.")
          Nothing -> do
            ph  <- liftIO $ hashPassword pw
            uid <- liftIO $ db $ P.insert $ User email ph name now
            tok <- liftIO generateToken
            _   <- liftIO $ db $ P.insert $ Session tok uid now
            setSessionCookie tok
            redirect "/"

  post "/logout" $ do
    mTok <- getCookie "session"
    case mTok of
      Just tok -> liftIO $ db $ P.deleteWhere [SessionToken ==. tok]
      Nothing  -> return ()
    clearSessionCookie
    redirect "/login"

  -- ── Dashboard ─────────────────────────────────────────────────────────────

  get "/" $ withUser pool $ \user@(P.Entity uid _) -> do
    today <- liftIO $ utctDay <$> getCurrentTime
    (withWater, logs) <- liftIO $ db $ do
      plants <- P.selectList [PlantUserId ==. uid] [P.Asc PlantCreatedAt]
      ww     <- mapM attachLastWatering plants
      ls     <- P.selectList [CareLogPlantId <-. map P.entityKey plants]
                             [P.Desc CareLogCreatedAt, P.LimitTo 6]
      return (ww, ls)
    html $ renderPage (Just user) $ dashboardView today withWater logs

  -- ── Lista de plantas ──────────────────────────────────────────────────────

  get "/plantas" $ withUser pool $ \user@(P.Entity uid _) -> do
    today    <- liftIO $ utctDay <$> getCurrentTime
    withWater <- liftIO $ db $ do
      plants <- P.selectList [PlantUserId ==. uid] [P.Asc PlantName]
      mapM attachLastWatering plants
    html $ renderPage (Just user) $ plantListView today withWater

  -- ── Estatísticas ──────────────────────────────────────────────────────────

  get "/estatisticas" $ withUser pool $ \user@(P.Entity uid _) -> do
    (plants, logs) <- liftIO $ db $ do
      ps <- P.selectList [PlantUserId ==. uid] [P.Asc PlantName]
      ls <- P.selectList [CareLogPlantId <-. map P.entityKey ps] [P.Desc CareLogDate]
      return (ps, ls)
    html $ renderPage (Just user) $ statsView plants logs

  -- ── Formulário nova planta ────────────────────────────────────────────────

  get "/plantas/nova" $ withUser pool $ \user ->
    html $ renderPage (Just user) $ plantFormView Nothing

  post "/plantas/nova" $ withUser pool $ \user@(P.Entity uid _) -> do
    name      <- param "name"     :: ActionM Text
    species   <- optParam "species"
    location  <- param "location" :: ActionM Text
    acquired  <- param "acquired" :: ActionM Text
    heightTxt <- optParam "height"
    intvlTxt  <- optParam "water_interval"
    notes     <- optParam "notes"
    now       <- liftIO getCurrentTime
    let height = heightTxt >>= readMaybe . T.unpack
        intvl  = intvlTxt  >>= readMaybe . T.unpack
    case parseDate acquired of
      Nothing   -> html $ renderPage (Just user) $ errorView "Data de aquisição inválida."
      Just date -> do
        pid <- liftIO $ db $ P.insert $
                 Plant uid name species date location height intvl notes now
        redirect $ "/plantas/" <> tshow (fromSqlKey pid)

  -- ── Detalhe da planta ─────────────────────────────────────────────────────

  get "/plantas/:id" $ withUser pool $ \user@(P.Entity uid _) -> do
    pid    <- param "id" :: ActionM Int64
    today  <- liftIO $ utctDay <$> getCurrentTime
    mPlant <- liftIO $ getOwnedPlant pool uid pid
    case mPlant of
      Nothing    -> html $ renderPage (Just user) $ errorView "Planta não encontrada."
      Just plant -> do
        logs <- liftIO $ db $ P.selectList
                  [CareLogPlantId ==. toSqlKey pid] [P.Desc CareLogDate]
        html $ renderPage (Just user) $ plantDetailView today plant logs

  -- ── Editar planta ─────────────────────────────────────────────────────────

  get "/plantas/:id/editar" $ withUser pool $ \user@(P.Entity uid _) -> do
    pid    <- param "id" :: ActionM Int64
    mPlant <- liftIO $ getOwnedPlant pool uid pid
    case mPlant of
      Nothing    -> html $ renderPage (Just user) $ errorView "Planta não encontrada."
      Just plant -> html $ renderPage (Just user) $ plantFormView (Just plant)

  post "/plantas/:id/editar" $ withUser pool $ \user@(P.Entity uid _) -> do
    pid       <- param "id"       :: ActionM Int64
    mPlant    <- liftIO $ getOwnedPlant pool uid pid
    case mPlant of
      Nothing -> redirect "/plantas"
      Just _  -> do
        name      <- param "name"     :: ActionM Text
        species   <- optParam "species"
        location  <- param "location" :: ActionM Text
        acquired  <- param "acquired" :: ActionM Text
        heightTxt <- optParam "height"
        intvlTxt  <- optParam "water_interval"
        notes     <- optParam "notes"
        let height = heightTxt >>= readMaybe . T.unpack
            intvl  = intvlTxt  >>= readMaybe . T.unpack
        case parseDate acquired of
          Nothing   -> html $ renderPage (Just user) $ errorView "Data de aquisição inválida."
          Just date -> do
            liftIO $ db $ P.update (toSqlKey pid :: PlantId)
              [ PlantName              =. name
              , PlantSpecies           =. species
              , PlantAcquiredDate      =. date
              , PlantLocation          =. location
              , PlantHeightCm          =. height
              , PlantWaterIntervalDays =. intvl
              , PlantNotes             =. notes
              ]
            redirect $ "/plantas/" <> tshow pid

  -- ── Excluir planta ────────────────────────────────────────────────────────

  post "/plantas/:id/excluir" $ withUser pool $ \(P.Entity uid _) -> do
    pid    <- param "id" :: ActionM Int64
    mPlant <- liftIO $ getOwnedPlant pool uid pid
    case mPlant of
      Nothing -> redirect "/plantas"
      Just _  -> do
        liftIO $ db $ do
          P.deleteWhere [CareLogPlantId ==. (toSqlKey pid :: PlantId)]
          P.delete      (toSqlKey pid  :: PlantId)
        redirect "/plantas"

  -- ── Registro de cuidado ───────────────────────────────────────────────────

  post "/plantas/:id/registros" $ withUser pool $ \user@(P.Entity uid _) -> do
    pid    <- param "id" :: ActionM Int64
    mPlant <- liftIO $ getOwnedPlant pool uid pid
    case mPlant of
      Nothing -> redirect "/plantas"
      Just _  -> do
        date     <- param "date"      :: ActionM Text
        careType <- param "care_type" :: ActionM Text
        notes    <- optParam "notes"
        now      <- liftIO getCurrentTime
        case parseDate date of
          Nothing -> html $ renderPage (Just user) $ errorView "Data do registro inválida."
          Just d  -> do
            -- Regra biológica: no máximo UMA rega por dia por planta.
            -- Regar demais encharca o substrato e apodrece as raízes.
            -- Por isso contamos quantas regas a planta já teve nesta data.
            sameDayWaterings <-
              if careType == "rega"
                then liftIO $ db $ P.count
                       [ CareLogPlantId  ==. (toSqlKey pid :: PlantId)
                       , CareLogCareType ==. ("rega" :: Text)
                       , CareLogDate     ==. d ]
                else return 0
            if sameDayWaterings > 0
              then html $ renderPage (Just user) $ wateringLimitView pid
              else do
                _ <- liftIO $ db $ P.insert $ CareLog (toSqlKey pid) d careType notes now
                redirect $ "/plantas/" <> tshow pid

  post "/plantas/:id/registros/:lid/excluir" $ withUser pool $ \(P.Entity uid _) -> do
    pid    <- param "id"  :: ActionM Int64
    lid    <- param "lid" :: ActionM Int64
    mPlant <- liftIO $ getOwnedPlant pool uid pid
    case mPlant of
      Nothing -> redirect "/plantas"
      Just _  -> do
        liftIO $ db $ P.delete (toSqlKey lid :: CareLogId)
        redirect $ "/plantas/" <> tshow pid

-- ─── Helpers ────────────────────────────────────────────────────────────────

attachLastWatering :: P.Entity Plant -> SqlPersistT IO (P.Entity Plant, Maybe Day)
attachLastWatering e@(P.Entity pid _) = do
  mLog <- P.selectFirst
            [ CareLogPlantId  ==. pid
            , CareLogCareType ==. ("rega" :: Text) ]
            [ P.Desc CareLogDate ]
  return (e, careLogDate . P.entityVal <$> mLog)

parseDate :: Text -> Maybe Day
parseDate t = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t)

optParam :: TL.Text -> ActionM (Maybe Text)
optParam name = do
  result <- (Just <$> param name) `rescue` \(_ :: SomeException) -> return Nothing
  return $ case result of
    Just t | T.null (T.strip t) -> Nothing
    other                        -> other

tshow :: Show a => a -> TL.Text
tshow = TL.pack . show
