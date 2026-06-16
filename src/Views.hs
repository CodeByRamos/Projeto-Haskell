{-# LANGUAGE OverloadedStrings #-}

-- | Views (o "chef que monta o prato bonito").
--
-- Aqui transformamos os dados em páginas HTML. Usamos a biblioteca Lucid: cada
-- função como @div_@, @p_@, @h1_@ representa uma tag HTML, e o texto entre
-- aspas vira o conteúdo. As classes (@class_ "..."@) são do Tailwind CSS, que
-- cuida das cores, espaçamentos e responsividade.
--
-- O ponto de entrada é 'renderPage' (páginas do app, com a barra de navegação)
-- e 'renderAuthPage' (telas de login/cadastro, sem a barra).
module Views where

import Data.Int             (Int64)
import Data.List            (sortOn)
import Data.Maybe           (fromMaybe, mapMaybe)
import Data.Ord             (Down(..))
import Data.Text            (Text)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import Data.Time            (Day, addDays, diffDays, formatTime, defaultTimeLocale)
import Database.Persist     (Entity(..))
import Database.Persist.Sql (fromSqlKey)
import Lucid
import Lucid.Base            (makeAttribute)
import Models

-- ─── Entry point ────────────────────────────────────────────────────────────

renderPage :: Maybe (Entity User) -> Html () -> TL.Text
renderPage mUser = renderText . layout mUser

layout :: Maybe (Entity User) -> Html () -> Html ()
layout mUser content = do
  doctype_
  html_ [lang_ "pt-BR"] $ do
    head_ pageHead
    body_ [class_ "relative min-h-screen flex flex-col text-[#3D3A33] antialiased"] $ do
      decorativeBackground
      siteNav mUser
      main_ [class_ "relative flex-1 w-full max-w-6xl mx-auto px-4 sm:px-6 py-8 sm:py-10"] content
      siteFooter

decorativeBackground :: Html ()
decorativeBackground =
  div_ [class_ "fixed inset-0 -z-10 overflow-hidden pointer-events-none"] $ do
    div_ [class_ "absolute -top-32 -left-24 w-[30rem] h-[30rem] rounded-full bg-emerald-900/5 blur-3xl"] emptyHtml
    div_ [class_ "absolute bottom-0 right-0 w-[26rem] h-[26rem] rounded-full bg-[#B98A2E]/5 blur-3xl"] emptyHtml

-- ─── <head> ─────────────────────────────────────────────────────────────────

pageHead :: Html ()
pageHead = do
  meta_ [charset_ "utf-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
  title_ "PlantDiary"
  link_ [ rel_ "stylesheet"
        , href_ "https://fonts.googleapis.com/css2?family=Fraunces:ital,opsz,wght@0,9..144,400;0,9..144,500;0,9..144,600;1,9..144,500&family=Inter:wght@400;500;600;700&display=swap"
        ]
  link_ [ rel_ "stylesheet"
        , href_ "https://cdn.jsdelivr.net/npm/@tabler/icons-webfont@2.47.0/tabler-icons.min.css"
        ]
  script_ [src_ "https://cdn.tailwindcss.com"] emptyHtml
  style_ appStyles

emptyHtml :: Html ()
emptyHtml = ""

appStyles :: Text
appStyles = T.unlines
  [ "body { font-family: 'Inter', sans-serif; background-color: #FBF9F4;"
  , "  background-image: linear-gradient(rgba(20,54,31,.025) 1px, transparent 1px), linear-gradient(90deg, rgba(20,54,31,.025) 1px, transparent 1px);"
  , "  background-size: 34px 34px; }"
  , ".font-serif { font-family: 'Fraunces', serif; font-optical-sizing: auto; letter-spacing: -0.01em; }"
  , "* { -webkit-tap-highlight-color: transparent; }"
  , ".card { transition: transform .2s ease, box-shadow .2s ease, border-color .2s ease; }"
  , ".card:hover { transform: translateY(-3px); box-shadow: 0 16px 36px rgba(20,54,31,.12); border-color: #B98A2E66; }"
  , ".btn { transition: background-color .15s ease, opacity .15s ease, transform .1s ease, box-shadow .15s ease; }"
  , ".btn:active { transform: scale(0.97); }"
  , ".link-fade { transition: color .15s ease; }"
  , ".gold-thread { height: 1px; background: linear-gradient(to right, transparent, rgba(185,138,46,.55), transparent); }"
  , "input, select, textarea { outline: none; transition: border-color .15s ease, box-shadow .15s ease; }"
  , "input:focus, select:focus, textarea:focus { border-color: #1E5B38 !important; box-shadow: 0 0 0 3px rgba(185,138,46,.18); }"
  , "::selection { background: #F0E6CE; }"
  , "@keyframes fadeUp { from { opacity: 0; transform: translateY(6px); } to { opacity: 1; transform: translateY(0); } }"
  , ".fade-up { animation: fadeUp .4s ease both; }"
  , ".icon-circle { display: inline-flex; align-items: center; justify-content: center; border-radius: 9999px; flex-shrink: 0; }"
  , ".toggle-box { transition: all .15s ease; }"
  , ".toggle-input:checked + .toggle-box { background: #14361F; color: #FBF9F4; border-color: #14361F; box-shadow: 0 6px 16px rgba(20,54,31,.28); }"
  , ".toggle-input:focus-visible + .toggle-box { box-shadow: 0 0 0 3px rgba(185,138,46,.3); }"
  ]

-- ─── Small helpers ──────────────────────────────────────────────────────────

icon :: Text -> Html ()
icon name = i_ [class_ $ "ti " <> name] emptyHtml

iconSz :: Text -> Text -> Html ()
iconSz name extraCls = i_ [class_ $ "ti " <> name <> " " <> extraCls] emptyHtml

checkedAttr :: Attribute
checkedAttr = makeAttribute "checked" "checked"

requiredAttr :: Attribute
requiredAttr = makeAttribute "required" "required"

autofocusAttr :: Attribute
autofocusAttr = makeAttribute "autofocus" "autofocus"

minlenAttr :: Text -> Attribute
minlenAttr = makeAttribute "minlength"

-- ─── Nav & Footer ───────────────────────────────────────────────────────────

siteNav :: Maybe (Entity User) -> Html ()
siteNav mUser =
  nav_ [class_ "relative bg-[#14361F] text-[#FBF9F4] sticky top-0 z-50 border-b border-[#B98A2E]/30"] $ do
    div_ [class_ "max-w-6xl mx-auto px-4 sm:px-6 flex items-center justify-between h-16"] $ do
      a_ [href_ "/", class_ "flex items-center gap-2.5 text-base sm:text-lg font-serif font-semibold link-fade hover:text-[#E7C77A]"] $ do
        iconSz "ti-leaf" "text-xl text-[#B98A2E]"
        span_ "PlantDiary"
      case mUser of
        Just (Entity _ u) ->
          div_ [class_ "flex items-center gap-3 sm:gap-6 text-sm font-medium"] $ do
            a_ [href_ "/", class_ "hidden sm:inline link-fade text-[#FBF9F4]/70 hover:text-[#FBF9F4]"] "Início"
            a_ [href_ "/plantas", class_ "hidden sm:inline link-fade text-[#FBF9F4]/70 hover:text-[#FBF9F4]"] "Minhas Plantas"
            a_ [href_ "/estatisticas", class_ "hidden sm:inline link-fade text-[#FBF9F4]/70 hover:text-[#FBF9F4]"] "Estatísticas"
            a_ [ href_ "/plantas/nova"
               , class_ "flex items-center gap-1.5 border border-[#FBF9F4]/25 hover:bg-[#FBF9F4]/10 text-[#FBF9F4] btn px-3.5 sm:px-4 py-2 rounded-lg font-medium text-xs sm:text-sm whitespace-nowrap"
               ] $ do
                 iconSz "ti-plus" "text-base"
                 span_ "Nova Planta"
            div_ [class_ "flex items-center gap-2.5 pl-1 sm:pl-4 sm:border-l border-[#FBF9F4]/15"] $ do
              div_ [class_ "icon-circle w-8 h-8 bg-[#B98A2E]/20 text-[#E7C77A] text-xs font-serif font-semibold"] $
                toHtml (userInitials u)
              span_ [class_ "hidden sm:inline text-[#FBF9F4]/80 text-sm max-w-[8rem] truncate"] $
                toHtml (userName u)
              form_ [action_ "/logout", method_ "post", class_ "flex"] $
                button_ [type_ "submit", class_ "btn text-[#FBF9F4]/50 hover:text-[#FBF9F4]", title_ "Sair"] $
                  iconSz "ti-logout" "text-lg"
        Nothing ->
          div_ [class_ "flex items-center gap-3 sm:gap-5 text-sm font-medium"] $ do
            a_ [href_ "/login", class_ "link-fade text-[#FBF9F4]/70 hover:text-[#FBF9F4]"] "Entrar"
            a_ [ href_ "/cadastro"
               , class_ "bg-[#B98A2E] hover:bg-[#A2761F] text-[#14361F] btn px-4 py-2 rounded-lg font-semibold text-xs sm:text-sm whitespace-nowrap shadow-sm"
               ] "Criar conta"

userInitials :: User -> Text
userInitials u =
  let parts = filter (not . T.null) (T.words (userName u))
  in case parts of
       []      -> "?"
       [a]     -> T.toUpper (T.take 1 a)
       (a:b:_) -> T.toUpper (T.take 1 a) <> T.toUpper (T.take 1 b)

siteFooter :: Html ()
siteFooter =
  footer_ [class_ "relative bg-[#14361F] text-[#FBF9F4]/55 text-center py-6 text-xs sm:text-sm mt-12 border-t border-[#B98A2E]/30"] $
    p_ [class_ "flex items-center justify-center gap-1.5"] $ do
      iconSz "ti-leaf" "text-[#B98A2E]"
      span_ "PlantDiary — cuide das suas plantas com carinho"

-- ─── Autenticação (login / cadastro) ──────────────────────────────────────────

renderAuthPage :: Html () -> TL.Text
renderAuthPage = renderText . authLayout

authLayout :: Html () -> Html ()
authLayout content = do
  doctype_
  html_ [lang_ "pt-BR"] $ do
    head_ pageHead
    body_ [class_ "antialiased text-[#3D3A33]", style_ "background-color:#FBF9F4;"] content

-- SVG botânico decorativo (sem imagens externas).
botanicalArt :: Html ()
botanicalArt = div_ [class_ "absolute inset-0 pointer-events-none"] $ toHtmlRaw botanicalSvg

botanicalSvg :: Text
botanicalSvg = T.concat
  [ "<svg viewBox='0 0 400 600' preserveAspectRatio='xMidYMid slice' xmlns='http://www.w3.org/2000/svg' style='width:100%;height:100%'>"
  , "<g fill='none' stroke='#FBF9F4' stroke-opacity='0.12' stroke-width='1.5'>"
  , "<path d='M60 600 C60 460 60 360 120 280 C150 240 150 180 130 120'/>"
  , "<path d='M120 300 C60 290 30 250 20 200 M120 300 C180 300 215 270 230 225'/>"
  , "<path d='M120 380 C60 372 35 338 25 300 M120 380 C180 372 205 345 220 308'/>"
  , "<path d='M120 460 C70 452 45 420 38 388 M120 460 C172 452 196 428 210 398'/>"
  , "</g>"
  , "<g fill='none' stroke='#B98A2E' stroke-opacity='0.18' stroke-width='1.5'>"
  , "<path d='M330 0 C300 70 300 130 340 180 C360 205 370 250 360 300'/>"
  , "<path d='M340 90 C300 86 280 60 274 28 M340 90 C384 86 405 64 414 30'/>"
  , "<ellipse cx='300' cy='420' rx='46' ry='80' transform='rotate(28 300 420)'/>"
  , "<path d='M300 350 L300 490' transform='rotate(28 300 420)'/>"
  , "</g>"
  , "</svg>"
  ]

-- Painel ilustrado lateral (escondido no mobile).
authSidePanel :: Html ()
authSidePanel =
  div_ [class_ "hidden lg:flex relative bg-[#14361F] text-[#FBF9F4] p-12 flex-col justify-between overflow-hidden"] $ do
    botanicalArt
    -- cantos de moldura dourados
    div_ [class_ "absolute top-6 left-6 w-8 h-8 border-t border-l border-[#B98A2E]/50"] emptyHtml
    div_ [class_ "absolute bottom-6 right-6 w-8 h-8 border-b border-r border-[#B98A2E]/50"] emptyHtml
    a_ [href_ "/", class_ "relative flex items-center gap-2.5 text-lg font-serif font-semibold"] $ do
      iconSz "ti-leaf" "text-xl text-[#B98A2E]"
      span_ "PlantDiary"
    div_ [class_ "relative"] $ do
      p_ [class_ "flex items-center gap-2 text-[11px] uppercase tracking-[0.22em] font-semibold text-[#E7C77A] mb-4"] $ do
        span_ "——"
        span_ "Seu jardim, organizado"
      h2_ [class_ "font-serif text-4xl font-semibold leading-[1.1] mb-4 text-[#FBF9F4]"] "Cada planta merece um diário."
      p_ [class_ "text-[#FBF9F4]/70 leading-relaxed max-w-sm"] "Acompanhe regas, crescimento e cuidados — e veja seu jardim prosperar ao longo do tempo."
    div_ [class_ "relative grid grid-cols-3 gap-4 pt-6 border-t border-[#FBF9F4]/10"] $ do
      authStat "Regas" "no ponto certo"
      authStat "Vitalidade" "que cresce"
      authStat "Histórico" "completo"

authStat :: Text -> Text -> Html ()
authStat title sub = div_ $ do
  p_ [class_ "font-serif text-base text-[#E7C77A]"] $ toHtml title
  p_ [class_ "text-[#FBF9F4]/55 text-xs"] $ toHtml sub

loginView :: Maybe Text -> Html ()
loginView mErr =
  div_ [class_ "min-h-screen grid lg:grid-cols-2"] $ do
    authSidePanel
    div_ [class_ "flex items-center justify-center px-6 py-12 sm:px-12"] $
      div_ [class_ "w-full max-w-sm fade-up"] $ do
        authMobileLogo
        authKicker "Bem-vindo de volta"
        h1_ [class_ "font-serif text-3xl font-semibold text-[#14361F] mb-1.5"] "Entrar na sua estufa"
        p_ [class_ "text-[#7A746A] text-sm mb-6"] "Acesse seu diário de plantas."
        maybe (pure ()) authError mErr
        form_ [action_ "/login", method_ "post", class_ "space-y-4"] $ do
          authField "E-mail" "ti-mail" $
            input_ [type_ "email", name_ "email", requiredAttr, autofocusAttr,
                    class_ authInputCls, placeholder_ "voce@email.com"]
          authField "Senha" "ti-lock" $
            input_ [type_ "password", name_ "password", requiredAttr,
                    class_ authInputCls, placeholder_ "Sua senha"]
          authSubmit "Entrar"
        authTrust
        authSwap "Ainda não tem conta?" "Criar agora" "/cadastro"

signupView :: Maybe Text -> Html ()
signupView mErr =
  div_ [class_ "min-h-screen grid lg:grid-cols-2"] $ do
    authSidePanel
    div_ [class_ "flex items-center justify-center px-6 py-12 sm:px-12"] $
      div_ [class_ "w-full max-w-sm fade-up"] $ do
        authMobileLogo
        authKicker "Comece agora"
        h1_ [class_ "font-serif text-3xl font-semibold text-[#14361F] mb-1.5"] "Criar sua conta"
        p_ [class_ "text-[#7A746A] text-sm mb-6"] "Leva menos de um minuto."
        maybe (pure ()) authError mErr
        form_ [action_ "/cadastro", method_ "post", class_ "space-y-4"] $ do
          authField "Nome" "ti-user" $
            input_ [type_ "text", name_ "name", requiredAttr, autofocusAttr,
                    class_ authInputCls, placeholder_ "Como te chamamos?"]
          authField "E-mail" "ti-mail" $
            input_ [type_ "email", name_ "email", requiredAttr,
                    class_ authInputCls, placeholder_ "voce@email.com"]
          authField "Senha" "ti-lock" $
            input_ [type_ "password", name_ "password", requiredAttr, minlenAttr "6",
                    class_ authInputCls, placeholder_ "Ao menos 6 caracteres"]
          authSubmit "Criar conta"
        authTrust
        authSwap "Já tem uma conta?" "Entrar" "/login"

authMobileLogo :: Html ()
authMobileLogo =
  a_ [href_ "/", class_ "lg:hidden flex items-center gap-2 text-[#14361F] font-serif font-semibold text-lg mb-8"] $ do
    iconSz "ti-leaf" "text-xl text-[#B98A2E]"
    span_ "PlantDiary"

authKicker :: Text -> Html ()
authKicker t =
  p_ [class_ "flex items-center gap-2 text-[11px] uppercase tracking-[0.22em] font-semibold text-[#B98A2E] mb-3"] $ do
    span_ "——"
    toHtml t

authError :: Text -> Html ()
authError msg =
  div_ [class_ "flex items-center gap-2 bg-[#F4E3DE] text-[#A23B2A] border border-[#E6C7BF] rounded-lg px-3.5 py-2.5 text-sm mb-4"] $ do
    iconSz "ti-alert-circle" "text-base flex-shrink-0"
    toHtml msg

authField :: Text -> Text -> Html () -> Html ()
authField label iconName inner =
  div_ $ do
    label_ [class_ "block text-sm font-medium text-[#3D3A33] mb-1.5"] $ toHtml label
    div_ [class_ "relative"] $ do
      div_ [class_ "absolute left-3.5 top-1/2 -translate-y-1/2 text-[#A9A294]"] $
        iconSz iconName "text-base"
      inner

authInputCls :: Text
authInputCls = "w-full rounded-lg border border-[#E7E1D3] bg-white pl-10 pr-4 py-3 text-sm text-[#3D3A33] placeholder-[#A9A294]"

authSubmit :: Text -> Html ()
authSubmit label =
  button_ [type_ "submit", class_ "w-full btn bg-[#14361F] hover:bg-[#1E5B38] text-[#FBF9F4] font-medium py-3 rounded-lg flex items-center justify-center gap-2 mt-2"] $ do
    toHtml label
    iconSz "ti-arrow-right" "text-base"

authTrust :: Html ()
authTrust =
  p_ [class_ "flex items-center justify-center gap-1.5 text-[11px] text-[#A9A294] mt-5"] $ do
    iconSz "ti-shield-check" "text-sm"
    span_ "Sua senha é protegida com criptografia."

authSwap :: Text -> Text -> Text -> Html ()
authSwap prompt linkText href = do
  div_ [class_ "gold-thread my-6"] emptyHtml
  p_ [class_ "text-center text-sm text-[#7A746A]"] $ do
    toHtml prompt
    " "
    a_ [href_ href, class_ "text-[#14361F] font-medium underline decoration-[#B98A2E] underline-offset-2 hover:text-[#1E5B38]"] $
      toHtml linkText

-- ─── Dashboard ──────────────────────────────────────────────────────────────

dashboardView :: Day -> [(Entity Plant, Maybe Day)] -> [Entity CareLog] -> Html ()
dashboardView today plants logs = do
  let needWater = filter (needsAttention today) plants
  div_ [class_ "mb-10 fade-up"] $ do
    p_ [class_ "flex items-center gap-2 text-[#B98A2E] font-semibold mb-2 text-xs uppercase tracking-[0.18em]"] "Painel"
    h1_ [class_ "font-serif text-3xl sm:text-4xl font-semibold text-[#14361F] mb-3 leading-tight"] "Visão geral do jardim"
    p_ [class_ "text-[#7A746A] text-base max-w-xl"] "Acompanhe o crescimento e os cuidados das suas plantas."

  div_ [class_ "grid grid-cols-1 sm:grid-cols-3 gap-4 mb-8 fade-up"] $ do
    statCard "ti-plant-2"   (tpack $ show $ length plants)    "plantas cadastradas" "emerald"
    statCard "ti-droplet"   (tpack $ show $ length needWater) "precisam de água"    "sky"
    statCard "ti-calendar"  (fmtDay today)                     "hoje"                "amber"

  -- Painel de atenção: plantas com rega atrasada ou para hoje
  if null needWater
    then return ()
    else div_ [class_ "mb-10 fade-up"] $ do
           div_ [class_ "bg-amber-50/80 backdrop-blur border border-amber-200 rounded-xl p-5"] $ do
             h2_ [class_ "flex items-center gap-2 font-serif text-lg font-semibold text-amber-800 mb-3"] $ do
               iconSz "ti-alert-circle" "text-xl"
               "Precisam de atenção hoje"
             div_ [class_ "grid grid-cols-1 sm:grid-cols-2 gap-2.5"] $
               mapM_ (attentionRow today) needWater

  div_ [class_ "grid grid-cols-1 lg:grid-cols-5 gap-8 sm:gap-10"] $ do

    div_ [class_ "lg:col-span-3"] $ do
      sectionTitle "Registros Recentes"
      if null logs
        then emptyState "ti-clipboard-list" "Nenhum registro ainda."
                        "Selecione uma planta e adicione o primeiro cuidado."
        else div_ [class_ "space-y-3"] $ mapM_ miniLogCard logs

    div_ [class_ "lg:col-span-2"] $ do
      sectionTitle "Suas Plantas"
      if null plants
        then div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-[#E7E1D3] p-8 text-center"] $ do
               iconSz "ti-plant-2" "text-4xl text-stone-300 mb-3"
               p_ [class_ "text-[#7A746A] mb-5 text-sm"] "Você ainda não tem plantas cadastradas."
               a_ [href_ "/plantas/nova", class_ "btn inline-flex items-center gap-1.5 bg-[#14361F] hover:bg-[#1E5B38] text-[#FBF9F4] px-5 py-2.5 rounded-lg text-sm font-medium"] $ do
                 icon "ti-plus"
                 span_ "Cadastrar primeira planta"
        else div_ [class_ "space-y-2.5"] $ mapM_ (quickPlantRow today) (take 6 plants)

-- ─── Plant List ─────────────────────────────────────────────────────────────

plantListView :: Day -> [(Entity Plant, Maybe Day)] -> Html ()
plantListView today plants = do
  div_ [class_ "flex flex-wrap items-center justify-between gap-4 mb-6 fade-up"] $ do
    h1_ [class_ "font-serif text-3xl sm:text-4xl font-semibold text-[#14361F]"] "Minhas Plantas"
    a_ [href_ "/plantas/nova", class_ "btn flex items-center gap-1.5 bg-[#14361F] hover:bg-[#1E5B38] text-[#FBF9F4] px-4 py-2.5 rounded-lg text-sm font-medium whitespace-nowrap"] $ do
      icon "ti-plus"
      span_ "Nova Planta"

  if null plants
    then emptyState "ti-plant-2" "Nenhuma planta cadastrada."
                    "Comece adicionando sua primeira planta!"
    else do
      -- Busca instantânea (client-side)
      div_ [class_ "relative mb-6 fade-up"] $ do
        div_ [class_ "absolute left-3.5 top-1/2 -translate-y-1/2 text-[#A9A294]"] $
          icon "ti-search"
        input_ [ type_ "text", id_ "plant-search"
               , placeholder_ "Buscar por nome, espécie ou local..."
               , class_ "w-full rounded-lg border border-[#E7E1D3] bg-white/90 backdrop-blur pl-10 pr-4 py-2.5 text-sm text-[#14361F] placeholder-stone-400"
               , makeAttribute "oninput" "filterPlants(this.value)" ]
      p_ [id_ "no-results", class_ "hidden text-center text-[#A9A294] text-sm py-10"] "Nenhuma planta encontrada para a busca."
      div_ [id_ "plant-grid", class_ "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-5 sm:gap-6"] $
        mapM_ (plantCard today) plants
      script_ searchScript

searchScript :: Text
searchScript = T.unlines
  [ "function filterPlants(q) {"
  , "  var term = q.toLowerCase().trim();"
  , "  var cards = document.querySelectorAll('#plant-grid [data-search]');"
  , "  var visible = 0;"
  , "  cards.forEach(function(c) {"
  , "    var match = c.getAttribute('data-search').indexOf(term) !== -1;"
  , "    c.style.display = match ? '' : 'none';"
  , "    if (match) visible++;"
  , "  });"
  , "  document.getElementById('no-results').classList.toggle('hidden', visible !== 0);"
  , "}"
  ]

-- ─── Plant Detail ───────────────────────────────────────────────────────────

plantDetailView :: Day -> Entity Plant -> [Entity CareLog] -> Html ()
plantDetailView today (Entity pid plant) logs = do
  let waterLogs = filter (\(Entity _ l) -> careLogCareType l == "rega") logs
      waterings = length waterLogs
      lastW     = case waterLogs of
                    (Entity _ l : _) -> Just (careLogDate l)
                    []               -> Nothing

  div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-[#E7E1D3] overflow-hidden mb-8 fade-up"] $ do
    div_ [class_ "gold-thread"] emptyHtml
    div_ [class_ "p-5 sm:p-7"] $ do
      div_ [class_ "flex flex-wrap items-start justify-between gap-4"] $ do
        div_ [class_ "flex items-start gap-4"] $ do
          div_ [class_ "icon-circle w-14 h-14 bg-emerald-50 text-emerald-600 text-2xl"] $
            icon (plantIcon (plantLocation plant))
          div_ $ do
            h1_ [class_ "font-serif text-2xl sm:text-3xl font-semibold text-[#14361F]"] $ toHtml $ plantName plant
            mapM_ (\sp -> p_ [class_ "text-[#7A746A] italic mt-0.5"] $ toHtml sp) (plantSpecies plant)
        div_ [class_ "flex gap-2 mt-1"] $ do
          actionBtn (pidUrl pid <> "/editar") "ti-edit" "Editar" "bg-stone-100 hover:bg-stone-200 text-stone-700"
          deleteBtn (pidUrl pid <> "/excluir") "ti-trash" "Excluir"
      div_ [class_ "flex flex-wrap gap-2 mt-4"] $ do
        badge "ti-map-pin" (plantLocation plant) "bg-stone-100 text-stone-600"
        badge "ti-calendar" ("Adquirida em " <> fmtDay (plantAcquiredDate plant)) "bg-emerald-50 text-emerald-700"
        growthBadge (plantHeightCm plant)
        waterStatusBadge (waterStatus today (plantWaterIntervalDays plant) lastW)
      mapM_ (\n -> p_ [class_ "mt-4 text-stone-600 leading-relaxed bg-stone-50 rounded-lg p-4 text-sm border border-stone-100"] $ toHtml n)
            (plantNotes plant)

  div_ [class_ "grid grid-cols-1 lg:grid-cols-5 gap-8"] $ do

    div_ [class_ "lg:col-span-2"] $ do
      vitalityShowcase waterings
      sectionTitle "Registrar Cuidado"
      div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-[#E7E1D3] p-5 sm:p-6"] $
        form_ [action_ (pidUrl pid <> "/registros"), method_ "post"] $ do
          formField "Data" $
            input_ [type_ "date", name_ "date", class_ inputCls]
          formField "Tipo de Cuidado" careTypeToggle
          formField "Observações (opcional)" $
            textarea_ [name_ "notes", rows_ "3", class_ inputCls, placeholder_ "Como a planta está?"] ""
          button_ [type_ "submit", class_ "w-full btn bg-[#14361F] hover:bg-[#1E5B38] text-[#FBF9F4] font-medium py-3 rounded-lg mt-2"] "Registrar cuidado"
      gardenerTip

    div_ [class_ "lg:col-span-3"] $ do
      sectionTitle $ "Histórico (" <> tpack (show $ length logs) <> " registros)"
      if null logs
        then emptyState "ti-calendar" "Nenhum cuidado registrado."
                        "Use o formulário ao lado para adicionar o primeiro."
        else div_ [class_ "relative pl-1"] $ do
               div_ [class_ "absolute left-[19px] top-2 bottom-2 w-px bg-stone-200"] emptyHtml
               div_ [class_ "space-y-4"] $
                 mapM_ (careLogItem (fromSqlKey pid)) logs

-- ─── Plant Form (create / edit) ─────────────────────────────────────────────

plantFormView :: Maybe (Entity Plant) -> Html ()
plantFormView mPlant = do
  let isEdit  = maybe False (const True) mPlant
      formUrl = maybe "/plantas/nova"
                      (\(Entity pid _) -> pidUrl pid <> "/editar") mPlant
      plant   = fmap entityVal mPlant
      val f   = maybe "" f plant
      valOpt f = fromMaybe "" (plant >>= f)
      titleText :: Text
      titleText = if isEdit then "Editar Planta" else "Nova Planta"
      submitText :: Text
      submitText = if isEdit then "Salvar Alterações" else "Cadastrar Planta"

  div_ [class_ "max-w-xl mx-auto fade-up"] $ do
    h1_ [class_ "font-serif text-3xl sm:text-4xl font-semibold text-[#14361F] mb-2"] $ toHtml titleText
    p_ [class_ "text-[#7A746A] mb-8"] $
      if isEdit then "Atualize as informações da sua planta."
                else "Preencha os dados para cadastrar uma nova planta."

    div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-[#E7E1D3] p-5 sm:p-7"] $
      form_ [action_ formUrl, method_ "post", class_ "space-y-5"] $ do
        div_ [class_ "grid grid-cols-1 sm:grid-cols-2 gap-5"] $ do
          formField "Nome da Planta" $
            input_ [type_ "text", name_ "name", class_ inputCls,
                    placeholder_ "Ex: Samambaia", value_ (val plantName)]
          formField "Espécie (opcional)" $
            input_ [type_ "text", name_ "species", class_ inputCls,
                    placeholder_ "Ex: Nephrolepis exaltata", value_ (valOpt plantSpecies)]
        div_ [class_ "grid grid-cols-1 sm:grid-cols-2 gap-5"] $ do
          formField "Data de Aquisição" $
            input_ [type_ "date", name_ "acquired", class_ inputCls,
                    value_ (maybe "" (tpack . show . plantAcquiredDate) plant)]
          formField "Localização" $
            input_ [type_ "text", name_ "location", class_ inputCls,
                    placeholder_ "Ex: Sala, Varanda, Jardim", value_ (val plantLocation)]
        div_ [class_ "grid grid-cols-1 sm:grid-cols-2 gap-5"] $ do
          formField "Altura aproximada em cm (opcional)" $
            input_ [type_ "number", name_ "height", step_ "0.5", min_ "0", class_ inputCls,
                    placeholder_ "Ex: 32", value_ (maybe "" (tpack . show) (plant >>= plantHeightCm))]
          formField "Regar a cada quantos dias? (opcional)" $
            input_ [type_ "number", name_ "water_interval", min_ "1", class_ inputCls,
                    placeholder_ "Ex: 7", value_ (maybe "" (tpack . show) (plant >>= plantWaterIntervalDays))]
        formField "Observações (opcional)" $
          textarea_ [name_ "notes", rows_ "4", class_ inputCls,
                     placeholder_ "Características especiais, origem, dicas..."]
            $ toHtml $ fromMaybe "" (plant >>= plantNotes)
        div_ [class_ "flex gap-3 pt-2"] $ do
          button_ [type_ "submit", class_ "flex-1 btn bg-[#14361F] hover:bg-[#1E5B38] text-[#FBF9F4] font-medium py-3 rounded-lg"] $
            toHtml submitText
          a_ [href_ "/plantas", class_ "btn flex-1 text-center bg-stone-100 hover:bg-stone-200 text-stone-700 font-medium py-3 rounded-lg"] "Cancelar"

-- ─── Error View ─────────────────────────────────────────────────────────────

errorView :: Text -> Html ()
errorView msg =
  div_ [class_ "max-w-md mx-auto text-center py-20 fade-up"] $ do
    iconSz "ti-alert-triangle" "text-5xl text-stone-300 mb-4"
    h1_ [class_ "font-serif text-2xl font-semibold text-stone-700 mb-2"] "Algo não foi encontrado"
    p_ [class_ "text-[#7A746A] mb-6"] $ toHtml msg
    a_ [href_ "/", class_ "btn inline-flex items-center gap-1.5 bg-[#14361F] hover:bg-[#1E5B38] text-[#FBF9F4] px-5 py-2.5 rounded-lg font-medium"] $ do
      icon "ti-arrow-left"
      span_ "Voltar ao início"

-- ─── Watering limit notice (gardener voice) ────────────────────────────────

wateringLimitView :: Int64 -> Html ()
wateringLimitView pid =
  div_ [class_ "max-w-lg mx-auto py-10 fade-up"] $
    div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-sky-200 overflow-hidden"] $ do
      div_ [class_ "gold-thread"] emptyHtml
      div_ [class_ "p-7 text-center"] $ do
        div_ [class_ "icon-circle w-16 h-16 bg-sky-50 text-sky-600 text-3xl mx-auto mb-4"] $
          icon "ti-droplet-off"
        h1_ [class_ "font-serif text-2xl font-semibold text-[#14361F] mb-3"] "Esta planta já foi regada hoje"
        p_ [class_ "text-stone-600 text-sm leading-relaxed mb-2"] $ do
          "O excesso de água é a "
          strong_ [class_ "font-semibold text-stone-700"] "principal causa de morte"
          " de plantas de interior: encharcar o substrato expulsa o oxigênio das raízes e favorece o apodrecimento (root rot) e fungos."
        p_ [class_ "text-[#7A746A] text-sm leading-relaxed mb-6"] $ do
          icon "ti-bulb"
          " A maioria das espécies só deve ser regada quando os 2–3 cm superiores do substrato estiverem secos ao toque — geralmente a cada poucos dias, nunca mais de uma vez por dia."
        a_ [href_ ("/plantas/" <> tshow pid), class_ "btn inline-flex items-center gap-1.5 bg-[#14361F] hover:bg-[#1E5B38] text-[#FBF9F4] px-5 py-2.5 rounded-lg font-medium"] $ do
          icon "ti-arrow-left"
          span_ "Voltar para a planta"

-- ─── Vitality meter (icon grows with consistent watering) ──────────────────

-- Decide o estágio de vitalidade conforme o número de regas (w).
-- Quanto mais cuidados, maior o estágio e o ícone. Devolve:
-- (ícone, nome do estágio, tamanho do ícone, quantas regas faltam p/ o próximo).
vitalityStage :: Int -> (Text, Text, Text, Maybe Int)
vitalityStage w
  | w >= 15   = ("ti-tree",    "Exuberante",     "text-7xl", Nothing)
  | w >= 8    = ("ti-plant",   "Vigorosa",       "text-7xl", Just (15 - w))
  | w >= 4    = ("ti-plant-2", "Crescendo",      "text-6xl", Just (8 - w))
  | w >= 1    = ("ti-seeding", "Brotando",       "text-5xl", Just (4 - w))
  | otherwise = ("ti-seeding", "Recém-plantada", "text-4xl", Just 1)

vitalityShowcase :: Int -> Html ()
vitalityShowcase w = do
  let (ic, label, sizeCls, next) = vitalityStage w
      pct = min 100 (w * 100 `div` 15)
      ringStyle = "background: conic-gradient(#1E5B38 " <> tshow pct <> "%, #E7E1D3 0);"
  div_ [class_ "bg-[#14361F] rounded-xl border border-[#B98A2E]/30 p-6 mb-6 text-center"] $ do
    p_ [class_ "flex items-center justify-center gap-2 text-xs uppercase tracking-[0.18em] text-[#E7C77A] font-semibold mb-4"] $ do
      span_ "——"
      span_ "Vitalidade da planta"
    div_ [class_ "inline-flex items-center justify-center rounded-full p-1.5 mb-4 ring-1 ring-[#B98A2E]/50", style_ ringStyle] $
      div_ [class_ "rounded-full bg-[#FBF9F4] flex items-center justify-center", style_ "width:8.5rem;height:8.5rem;"] $
        iconSz ic (sizeCls <> " text-[#1E5B38]")
    h3_ [class_ "font-serif text-xl font-semibold text-[#FBF9F4]"] $ toHtml label
    p_ [class_ "text-[#FBF9F4]/60 text-sm mt-1"] $
      toHtml (tshow w <> (if w == 1 then " rega registrada" else " regas registradas"))
    case next of
      Just n  -> p_ [class_ "inline-flex items-center gap-1.5 text-[#E7C77A] text-xs mt-3 font-medium bg-[#B98A2E]/15 px-3 py-1 rounded-full"] $ do
                   iconSz "ti-arrow-up-right" "text-sm"
                   toHtml ("Faltam " <> tshow n <> (if n == 1 then " rega" else " regas") <> " para o próximo estágio")
      Nothing -> p_ [class_ "inline-flex items-center gap-1.5 text-[#E7C77A] text-xs mt-3 font-medium bg-[#B98A2E]/15 px-3 py-1 rounded-full"] $ do
                   iconSz "ti-award" "text-sm"
                   "Estágio máximo alcançado"

gardenerTip :: Html ()
gardenerTip =
  div_ [class_ "bg-emerald-50/70 border border-emerald-100 rounded-xl p-4 mt-6 flex gap-3"] $ do
    div_ [class_ "icon-circle w-9 h-9 bg-emerald-100 text-emerald-700 text-base flex-shrink-0"] $
      icon "ti-bulb"
    div_ $ do
      p_ [class_ "text-sm font-medium text-emerald-800 mb-0.5"] "Dica do jardineiro"
      p_ [class_ "text-xs text-emerald-700 leading-relaxed"] "Regue somente quando os 2–3 cm superiores do substrato estiverem secos ao toque. Cada rega consistente faz a vitalidade da sua planta crescer — mas o limite de uma por dia protege as raízes do apodrecimento."

-- ─── Watering schedule (smart reminders) ──────────────────────────────────

data WaterStatus = NoSchedule | NeverWatered | Overdue Int | DueToday | Upcoming Int

-- Calcula a situação da rega de uma planta. A conta é simples:
-- pega a data da última rega, soma o intervalo (ex.: +7 dias) e compara
-- com a data de hoje. Se a data prevista já passou, está atrasada.
waterStatus :: Day -> Maybe Int -> Maybe Day -> WaterStatus
waterStatus _ Nothing _              = NoSchedule    -- sem agenda definida
waterStatus _ (Just _) Nothing       = NeverWatered  -- tem agenda, mas nunca regada
waterStatus today (Just iv) (Just w) =
  let nextDue = addDays (fromIntegral iv) w  -- data prevista da próxima rega
      d       = diffDays nextDue today        -- dias entre hoje e a data prevista
  in if d < 0        then Overdue (fromInteger (negate d))  -- já passou: atrasada
     else if d == 0  then DueToday                          -- é hoje
     else                 Upcoming (fromInteger d)          -- ainda falta

needsAttention :: Day -> (Entity Plant, Maybe Day) -> Bool
needsAttention today (Entity _ p, lastW) =
  case waterStatus today (plantWaterIntervalDays p) lastW of
    Overdue _    -> True
    DueToday     -> True
    NeverWatered -> True
    _            -> False

waterStatusBadge :: WaterStatus -> Html ()
waterStatusBadge st = case st of
  NoSchedule   -> badge "ti-droplet-off"     "Sem agenda"                       "bg-stone-100 text-[#7A746A]"
  NeverWatered -> badge "ti-droplet"         "Regar agora"                      "bg-amber-50 text-amber-700"
  Overdue n    -> badge "ti-alert-triangle"  ("Atrasada " <> tshow n <> "d")    "bg-red-50 text-red-600"
  DueToday     -> badge "ti-droplet"         "Regar hoje"                       "bg-amber-50 text-amber-700"
  Upcoming n   -> badge "ti-clock"           ("Regar em " <> tshow n <> "d")    "bg-emerald-50 text-emerald-600"

waterStatusDot :: WaterStatus -> Html ()
waterStatusDot st = case st of
  NeverWatered -> iconSz "ti-droplet"        "text-amber-500"
  Overdue _    -> iconSz "ti-alert-triangle" "text-red-500"
  DueToday     -> iconSz "ti-droplet"        "text-amber-500"
  _            -> iconSz "ti-chevron-right"  "text-stone-300 group-hover:text-emerald-500 transition-colors"

attentionRow :: Day -> (Entity Plant, Maybe Day) -> Html ()
attentionRow today (Entity pid plant, lastW) =
  a_ [href_ (pidUrl pid), class_ "flex items-center gap-3 bg-white rounded-lg border border-amber-100 p-3 hover:border-amber-300 transition-colors"] $ do
    div_ [class_ "icon-circle w-9 h-9 bg-amber-50 text-amber-600 text-base flex-shrink-0"] $
      icon "ti-droplet"
    div_ [class_ "flex-1 min-w-0"] $ do
      p_ [class_ "font-medium text-stone-700 text-sm truncate"] $ toHtml $ plantName plant
      p_ [class_ "text-xs truncate"] $
        waterStatusText (waterStatus today (plantWaterIntervalDays plant) lastW)
    iconSz "ti-arrow-right" "text-amber-400 flex-shrink-0"

waterStatusText :: WaterStatus -> Html ()
waterStatusText st = case st of
  Overdue n    -> span_ [class_ "text-red-600 font-medium"] $
                    toHtml ("Rega atrasada há " <> tshow n <> (if n == 1 then " dia" else " dias"))
  DueToday     -> span_ [class_ "text-amber-700 font-medium"] "Regar hoje"
  NeverWatered -> span_ [class_ "text-amber-700 font-medium"] "Ainda não foi regada"
  _            -> span_ [class_ "text-[#A9A294]"] "Em dia"

-- ─── Garden statistics ──────────────────────────────────────────────────────

statsView :: [Entity Plant] -> [Entity CareLog] -> Html ()
statsView plants logs = do
  let total      = length logs
      countOf t  = length (filter (\(Entity _ l) -> careLogCareType l == t) logs)
      rows       = [ (label, ic, countOf v) | (v, label, ic) <- careTypeDefs ]
      maxC       = maximum (1 : map (\(_, _, c) -> c) rows)
      waterCount = countOf "rega"
      counts     = [ (plantName p, length (filter (\(Entity _ l) -> careLogPlantId l == pid) logs))
                   | Entity pid p <- plants ]
      topPlants  = take 5 $ sortOn (Down . snd) $ filter ((> 0) . snd) counts

  div_ [class_ "mb-10 fade-up"] $ do
    p_ [class_ "flex items-center gap-2 text-[#B98A2E] font-semibold mb-2 text-xs uppercase tracking-[0.18em]"] "Insights"
    h1_ [class_ "font-serif text-3xl sm:text-4xl font-semibold text-[#14361F] mb-3 leading-tight"] "Estatísticas do jardim"
    p_ [class_ "text-[#7A746A] text-base max-w-xl"] "Um panorama de como você tem cuidado das suas plantas."

  div_ [class_ "grid grid-cols-2 lg:grid-cols-4 gap-4 mb-10 fade-up"] $ do
    statCard "ti-plant-2"        (tpack $ show $ length plants) "plantas"        "emerald"
    statCard "ti-clipboard-list" (tpack $ show total)           "cuidados totais" "sky"
    statCard "ti-droplet"        (tpack $ show waterCount)       "regas"          "amber"
    statCard "ti-map-pin"        (tpack $ show $ distinctLocations plants) "locais" "emerald"

  div_ [class_ "grid grid-cols-1 lg:grid-cols-2 gap-8"] $ do
    div_ $ do
      sectionTitle "Cuidados por tipo"
      div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-[#E7E1D3] p-5 space-y-3"] $
        if total == 0
          then p_ [class_ "text-[#A9A294] text-sm text-center py-6"] "Nenhum cuidado registrado ainda."
          else mapM_ (statBar maxC) rows

    div_ $ do
      sectionTitle "Plantas mais cuidadas"
      div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-[#E7E1D3] p-5"] $
        if null topPlants
          then p_ [class_ "text-[#A9A294] text-sm text-center py-6"] "Nenhum cuidado registrado ainda."
          else div_ [class_ "space-y-3"] $ mapM_ rankRow (zip [1 :: Int ..] topPlants)

statBar :: Int -> (Text, Text, Int) -> Html ()
statBar maxC (label, ic, c) = do
  let pct = c * 100 `div` maxC
  div_ $ do
    div_ [class_ "flex items-center justify-between text-sm mb-1"] $ do
      span_ [class_ "flex items-center gap-1.5 text-stone-600 font-medium"] $ do
        iconSz ic "text-base text-emerald-600"
        toHtml label
      span_ [class_ "text-[#A9A294] text-xs"] $ toHtml (tshow c)
    div_ [class_ "h-2 bg-stone-100 rounded-full overflow-hidden"] $
      div_ [class_ "h-full bg-gradient-to-r from-emerald-500 to-sky-500 rounded-full", style_ ("width:" <> tshow pct <> "%;")] emptyHtml

rankRow :: (Int, (Text, Int)) -> Html ()
rankRow (pos, (name, c)) =
  div_ [class_ "flex items-center gap-3"] $ do
    div_ [class_ "icon-circle w-8 h-8 bg-emerald-50 text-emerald-700 text-sm font-semibold"] $ toHtml (tshow pos)
    span_ [class_ "flex-1 min-w-0 truncate font-medium text-stone-700 text-sm"] $ toHtml name
    span_ [class_ "text-[#A9A294] text-xs"] $ toHtml (tshow c <> (if c == 1 then " cuidado" else " cuidados"))

distinctLocations :: [Entity Plant] -> Int
distinctLocations = length . dedupe . map (T.toLower . plantLocation . entityVal)
  where dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- ─── Components ─────────────────────────────────────────────────────────────

plantCard :: Day -> (Entity Plant, Maybe Day) -> Html ()
plantCard today (Entity pid plant, lastW) =
  div_ [ class_ "card bg-white/90 backdrop-blur rounded-xl border border-[#E7E1D3] overflow-hidden"
       , makeAttribute "data-search" (searchKey plant) ] $ do
    div_ [class_ "gold-thread"] emptyHtml
    div_ [class_ "p-5"] $ do
      div_ [class_ "flex items-start gap-3 mb-3"] $ do
        div_ [class_ "icon-circle w-11 h-11 bg-emerald-50 text-emerald-600 text-lg"] $
          icon (plantIcon (plantLocation plant))
        div_ $ do
          h3_ [class_ "font-serif text-lg font-semibold text-[#14361F] leading-tight"] $ toHtml $ plantName plant
          mapM_ (\sp -> p_ [class_ "text-[#A9A294] italic text-sm"] $ toHtml sp) (plantSpecies plant)
      div_ [class_ "flex flex-wrap gap-2 mb-4"] $ do
        badge "ti-map-pin" (plantLocation plant) "bg-stone-100 text-[#7A746A]"
        growthBadge (plantHeightCm plant)
        waterStatusBadge (waterStatus today (plantWaterIntervalDays plant) lastW)
      a_ [href_ (pidUrl pid), class_ "btn flex items-center justify-center gap-1.5 bg-[#14361F] hover:bg-[#1E5B38] text-[#FBF9F4] rounded-lg py-2.5 text-sm font-medium"] $ do
        span_ "Ver detalhes"
        iconSz "ti-arrow-right" "text-base"

searchKey :: Plant -> Text
searchKey p = T.toLower $ T.intercalate " "
  [ plantName p, fromMaybe "" (plantSpecies p), plantLocation p ]

statCard :: Text -> Text -> Text -> Text -> Html ()
statCard iconName valueText label color =
  div_ [class_ "bg-white/90 backdrop-blur border border-[#E7E1D3] rounded-xl p-5 flex items-center gap-4"] $ do
    div_ [class_ $ "icon-circle w-12 h-12 text-xl " <> colorBg color <> " " <> colorText color] $
      icon iconName
    div_ $ do
      p_ [class_ "text-xl sm:text-2xl font-semibold font-serif text-[#14361F]"] $ toHtml valueText
      p_ [class_ "text-[#7A746A] text-xs sm:text-sm"] $ toHtml label

careLogItem :: Int64 -> Entity CareLog -> Html ()
careLogItem pid (Entity lid log_) =
  div_ [class_ "relative flex gap-4 pb-2"] $ do
    div_ [class_ $ "icon-circle w-10 h-10 text-lg ring-4 ring-stone-50 " <> careTypeBg (careLogCareType log_) <> " " <> careTypeColor (careLogCareType log_)] $
      icon (careTypeIcon (careLogCareType log_))
    div_ [class_ "bg-white rounded-lg p-4 flex-1 border border-[#E7E1D3]"] $ do
      div_ [class_ "flex items-center justify-between gap-2 flex-wrap"] $ do
        span_ [class_ "font-medium text-stone-700"] $ toHtml $ careTypeLabel (careLogCareType log_)
        span_ [class_ "text-[#A9A294] text-sm"] $ toHtml $ fmtDay (careLogDate log_)
      mapM_ (\n -> p_ [class_ "text-[#7A746A] text-sm mt-1"] $ toHtml n)
            (careLogNotes log_)
      form_ [action_ ("/plantas/" <> tshow pid <> "/registros/" <> tshow (fromSqlKey lid) <> "/excluir"), method_ "post", class_ "mt-2"] $
        button_ [type_ "submit", class_ "text-xs text-stone-300 hover:text-red-500 link-fade"] "remover"

miniLogCard :: Entity CareLog -> Html ()
miniLogCard (Entity _ log_) =
  div_ [class_ "flex items-center gap-3 bg-white/90 backdrop-blur rounded-lg border border-[#E7E1D3] p-3.5"] $ do
    div_ [class_ $ "icon-circle w-9 h-9 text-base " <> careTypeBg (careLogCareType log_) <> " " <> careTypeColor (careLogCareType log_)] $
      icon (careTypeIcon (careLogCareType log_))
    div_ [class_ "flex-1 min-w-0"] $ do
      p_ [class_ "font-medium text-stone-700 text-sm truncate"] $ toHtml $ careTypeLabel (careLogCareType log_)
      p_ [class_ "text-[#A9A294] text-xs"] $ toHtml $ fmtDay (careLogDate log_)

quickPlantRow :: Day -> (Entity Plant, Maybe Day) -> Html ()
quickPlantRow today (Entity pid plant, lastW) =
  a_ [href_ (pidUrl pid), class_ "flex items-center gap-3 bg-white/90 backdrop-blur rounded-lg border border-[#E7E1D3] p-3.5 hover:border-emerald-300 transition-colors group"] $ do
    div_ [class_ "icon-circle w-10 h-10 bg-emerald-50 text-emerald-600 text-base"] $
      icon (plantIcon (plantLocation plant))
    div_ [class_ "flex-1 min-w-0"] $ do
      p_ [class_ "font-medium text-stone-700 text-sm truncate group-hover:text-emerald-700 transition-colors"] $ toHtml $ plantName plant
      p_ [class_ "text-[#A9A294] text-xs italic truncate"] $ toHtml $ fromMaybe "Espécie não informada" (plantSpecies plant)
    waterStatusDot (waterStatus today (plantWaterIntervalDays plant) lastW)

-- ─── Care type toggle (responsive button grid, no JS) ──────────────────────

careTypeDefs :: [(Text, Text, Text)]
careTypeDefs =
  [ ("rega",       "Rega",            "ti-droplet")
  , ("adubacao",   "Adubação",        "ti-seeding")
  , ("poda",       "Poda",            "ti-cut")
  , ("repotagem",  "Repotagem",       "ti-arrow-up-circle")
  , ("observacao", "Observação",      "ti-eye")
  , ("solar",      "Exposição solar", "ti-sun")
  , ("tratamento", "Tratamento",      "ti-pill")
  ]

careTypeToggle :: Html ()
careTypeToggle =
  div_ [class_ "grid grid-cols-3 sm:grid-cols-4 gap-2"] $
    mapM_ renderToggle (zip [0 :: Int ..] careTypeDefs)
  where
    renderToggle :: (Int, (Text, Text, Text)) -> Html ()
    renderToggle (i, (v, label, ic)) =
      label_ [class_ "cursor-pointer block"] $ do
        input_ $
          [type_ "radio", name_ "care_type", value_ v, class_ "toggle-input sr-only"]
          ++ [checkedAttr | i == 0]
        div_ [class_ "toggle-box flex flex-col items-center justify-center gap-1 px-1 py-3 rounded-xl border border-[#E7E1D3] bg-white text-[#7A746A] hover:border-emerald-300 hover:text-emerald-600 text-center select-none"] $ do
          iconSz ic "text-xl"
          span_ [class_ "text-[11px] font-medium leading-tight"] $ toHtml label

-- ─── Growth tracking ────────────────────────────────────────────────────────

growthBadge :: Maybe Double -> Html ()
growthBadge h =
  let (ic, label, bg, txt) = growthStage h
  in span_ [class_ $ "inline-flex items-center gap-1.5 px-2.5 py-1 rounded-md text-xs font-medium " <> bg <> " " <> txt] $ do
       iconSz ic "text-sm"
       toHtml label

growthStage :: Maybe Double -> (Text, Text, Text, Text)
growthStage Nothing = ("ti-ruler-2", "Altura não registrada", "bg-stone-100", "text-[#7A746A]")
growthStage (Just h)
  | h < 15    = ("ti-seeding",  "Muda · "             <> fmtHeight h, "bg-lime-50",    "text-lime-700")
  | h < 50    = ("ti-plant-2",  "Em crescimento · "   <> fmtHeight h, "bg-emerald-50", "text-emerald-700")
  | otherwise = ("ti-tree",     "Planta madura · "    <> fmtHeight h, "bg-teal-50",    "text-teal-700")

fmtHeight :: Double -> Text
fmtHeight h = tpack (show (round h :: Int)) <> " cm"

-- ─── Small presentational components ───────────────────────────────────────

emptyState :: Text -> Text -> Text -> Html ()
emptyState iconName titleText desc =
  div_ [class_ "bg-white/80 backdrop-blur rounded-xl border border-dashed border-stone-300 p-10 text-center"] $ do
    iconSz iconName "text-4xl text-stone-300 mb-3"
    p_ [class_ "font-medium text-stone-600 mb-1"] $ toHtml titleText
    p_ [class_ "text-[#A9A294] text-sm"] $ toHtml desc

badge :: Text -> Text -> Text -> Html ()
badge iconName txt cls =
  span_ [class_ $ "inline-flex items-center gap-1.5 px-2.5 py-1 rounded-md text-xs font-medium " <> cls] $ do
    iconSz iconName "text-sm"
    toHtml txt

sectionTitle :: Text -> Html ()
sectionTitle t =
  h2_ [class_ "font-serif text-lg sm:text-xl font-semibold text-[#14361F] mb-4"] $ toHtml t

formField :: Text -> Html () -> Html ()
formField labelText fieldInput =
  div_ $ do
    label_ [class_ "block text-sm font-medium text-stone-600 mb-1.5"] $ toHtml labelText
    fieldInput

actionBtn :: Text -> Text -> Text -> Text -> Html ()
actionBtn url iconName label cls =
  a_ [href_ url, class_ $ "btn flex items-center gap-1.5 px-3.5 py-1.5 rounded-lg text-sm font-medium " <> cls] $ do
    icon iconName
    span_ $ toHtml label

deleteBtn :: Text -> Text -> Text -> Html ()
deleteBtn actionUrl iconName label =
  form_ [action_ actionUrl, method_ "post", class_ "inline"] $
    button_ [type_ "submit", class_ "btn flex items-center gap-1.5 px-3.5 py-1.5 rounded-lg text-sm font-medium bg-red-50 hover:bg-red-100 text-red-600",
             onclick_ "return confirm('Tem certeza? Esta ação não pode ser desfeita.')"] $ do
      icon iconName
      span_ $ toHtml label

-- ─── Helpers ────────────────────────────────────────────────────────────────

inputCls :: Text
inputCls = "w-full rounded-lg border border-[#E7E1D3] bg-stone-50 px-4 py-2.5 text-sm text-[#14361F] placeholder-stone-400"

pidUrl :: PlantId -> Text
pidUrl pid = "/plantas/" <> tpack (show $ fromSqlKey pid)

fmtDay :: Day -> Text
fmtDay = tpack . formatTime defaultTimeLocale "%d/%m/%Y"

tpack :: String -> Text
tpack = T.pack

tshow :: Show a => a -> Text
tshow = T.pack . show

colorBg :: Text -> Text
colorBg c = case c of
  "emerald" -> "bg-emerald-50"
  "sky"     -> "bg-sky-50"
  "amber"   -> "bg-amber-50"
  _         -> "bg-stone-100"

colorText :: Text -> Text
colorText c = case c of
  "emerald" -> "text-emerald-600"
  "sky"     -> "text-sky-600"
  "amber"   -> "text-amber-600"
  _         -> "text-stone-600"

plantIcon :: Text -> Text
plantIcon loc
  | "varanda" `T.isInfixOf` lc = "ti-sun"
  | "jardim"  `T.isInfixOf` lc = "ti-tree"
  | "quarto"  `T.isInfixOf` lc = "ti-bed"
  | otherwise                  = "ti-plant-2"
  where lc = T.toLower loc

careTypeIcon :: Text -> Text
careTypeIcon t = case t of
  "rega"       -> "ti-droplet"
  "adubacao"   -> "ti-seeding"
  "poda"       -> "ti-cut"
  "repotagem"  -> "ti-arrow-up-circle"
  "observacao" -> "ti-eye"
  "solar"      -> "ti-sun"
  "tratamento" -> "ti-pill"
  _            -> "ti-notes"

careTypeLabel :: Text -> Text
careTypeLabel t = case t of
  "rega"       -> "Rega"
  "adubacao"   -> "Adubação"
  "poda"       -> "Poda"
  "repotagem"  -> "Repotagem"
  "observacao" -> "Observação"
  "solar"      -> "Exposição solar"
  "tratamento" -> "Tratamento"
  _            -> t

careTypeBg :: Text -> Text
careTypeBg t = case t of
  "rega"       -> "bg-sky-50"
  "adubacao"   -> "bg-emerald-50"
  "poda"       -> "bg-stone-100"
  "repotagem"  -> "bg-amber-50"
  "observacao" -> "bg-violet-50"
  "solar"      -> "bg-yellow-50"
  "tratamento" -> "bg-pink-50"
  _            -> "bg-stone-100"

careTypeColor :: Text -> Text
careTypeColor t = case t of
  "rega"       -> "text-sky-600"
  "adubacao"   -> "text-emerald-600"
  "poda"       -> "text-stone-600"
  "repotagem"  -> "text-amber-600"
  "observacao" -> "text-violet-600"
  "solar"      -> "text-yellow-600"
  "tratamento" -> "text-pink-600"
  _            -> "text-stone-600"
