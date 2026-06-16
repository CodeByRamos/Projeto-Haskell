{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes where

import Control.Exception       (SomeException)
import Control.Monad.IO.Class  (liftIO)
import Data.Int                (Int64)
import Data.Text               (Text)
import qualified Data.Text     as T
import qualified Data.Text.Lazy as TL
import Data.Time               (getCurrentTime, parseTimeM, defaultTimeLocale, utctDay, Day)
import qualified Database.Persist as P
import Database.Persist        ((==.), (=.))
import Database.Persist.Sql    (fromSqlKey, toSqlKey, SqlPersistT, runSqlPool)
import Database.Persist.Sqlite (ConnectionPool)
import Models
import Text.Read               (readMaybe)
import Views hiding (tshow)
import Web.Scotty

-- ─── DB helper ──────────────────────────────────────────────────────────────

runDB :: ConnectionPool -> SqlPersistT IO a -> IO a
runDB pool action = runSqlPool action pool

-- ─── Routes ─────────────────────────────────────────────────────────────────

appRoutes :: ConnectionPool -> ScottyM ()
appRoutes pool = do
  let db = runDB pool

  -- Dashboard
  get "/" $ do
    today    <- liftIO $ utctDay <$> getCurrentTime
    withWater <- liftIO $ db $ do
      plants <- P.selectList [] [P.Asc PlantCreatedAt]
      mapM attachLastWatering plants
    logs   <- liftIO $ db $ P.selectList [] [P.Desc CareLogCreatedAt, P.LimitTo 6]
    html $ renderPage $ dashboardView today withWater logs

  -- Lista de plantas
  get "/plantas" $ do
    today    <- liftIO $ utctDay <$> getCurrentTime
    withWater <- liftIO $ db $ do
      plants <- P.selectList [] [P.Asc PlantName]
      mapM attachLastWatering plants
    html $ renderPage $ plantListView today withWater

  -- Estatísticas do jardim
  get "/estatisticas" $ do
    plants <- liftIO $ db $ P.selectList [] [P.Asc PlantName]
    logs   <- liftIO $ db $ P.selectList [] [P.Desc CareLogDate]
    html $ renderPage $ statsView plants logs

  -- Formulário nova planta (antes de /:id para não conflitar)
  get "/plantas/nova" $
    html $ renderPage $ plantFormView Nothing

  -- Criar planta
  post "/plantas/nova" $ do
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
      Nothing   -> html $ renderPage $ errorView "Data de aquisição inválida."
      Just date -> do
        pid <- liftIO $ db $ P.insert $ Plant name species date location height intvl notes now
        redirect $ "/plantas/" <> tshow (fromSqlKey pid)

  -- Detalhe da planta
  get "/plantas/:id" $ do
    pid    <- param "id" :: ActionM Int64
    today  <- liftIO $ utctDay <$> getCurrentTime
    mPlant <- liftIO $ db $ P.get (toSqlKey pid :: PlantId)
    case mPlant of
      Nothing    -> html $ renderPage $ errorView "Planta não encontrada."
      Just plant -> do
        logs <- liftIO $ db $ P.selectList
                  [CareLogPlantId ==. toSqlKey pid] [P.Desc CareLogDate]
        html $ renderPage $ plantDetailView today (P.Entity (toSqlKey pid) plant) logs

  -- Formulário editar planta
  get "/plantas/:id/editar" $ do
    pid    <- param "id" :: ActionM Int64
    mPlant <- liftIO $ db $ P.get (toSqlKey pid :: PlantId)
    case mPlant of
      Nothing    -> html $ renderPage $ errorView "Planta não encontrada."
      Just plant -> html $ renderPage $ plantFormView (Just (P.Entity (toSqlKey pid) plant))

  -- Atualizar planta
  post "/plantas/:id/editar" $ do
    pid       <- param "id"       :: ActionM Int64
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
      Nothing   -> html $ renderPage $ errorView "Data de aquisição inválida."
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

  -- Excluir planta
  post "/plantas/:id/excluir" $ do
    pid <- param "id" :: ActionM Int64
    liftIO $ db $ do
      P.deleteWhere [CareLogPlantId ==. (toSqlKey pid :: PlantId)]
      P.delete      (toSqlKey pid  :: PlantId)
    redirect "/plantas"

  -- Adicionar registro de cuidado
  post "/plantas/:id/registros" $ do
    pid      <- param "id"        :: ActionM Int64
    date     <- param "date"      :: ActionM Text
    careType <- param "care_type" :: ActionM Text
    notes    <- optParam "notes"
    now      <- liftIO getCurrentTime
    case parseDate date of
      Nothing -> html $ renderPage $ errorView "Data do registro inválida."
      Just d  -> do
        -- Limite biológico: no máximo uma rega por dia (evita apodrecimento de raízes)
        sameDayWaterings <-
          if careType == "rega"
            then liftIO $ db $ P.count
                   [ CareLogPlantId  ==. (toSqlKey pid :: PlantId)
                   , CareLogCareType ==. ("rega" :: Text)
                   , CareLogDate     ==. d ]
            else return 0
        if sameDayWaterings > 0
          then html $ renderPage $ wateringLimitView pid
          else do
            _ <- liftIO $ db $ P.insert $ CareLog (toSqlKey pid) d careType notes now
            redirect $ "/plantas/" <> tshow pid

  -- Excluir registro de cuidado
  post "/plantas/:id/registros/:lid/excluir" $ do
    pid <- param "id"  :: ActionM Int64
    lid <- param "lid" :: ActionM Int64
    liftIO $ db $ P.delete (toSqlKey lid :: CareLogId)
    redirect $ "/plantas/" <> tshow pid

-- ─── Helpers ────────────────────────────────────────────────────────────────

-- Anexa a data da última rega de cada planta (para a agenda de rega).
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
