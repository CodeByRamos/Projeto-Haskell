{-# LANGUAGE OverloadedStrings #-}

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

renderPage :: Html () -> TL.Text
renderPage = renderText . layout

layout :: Html () -> Html ()
layout content = do
  doctype_
  html_ [lang_ "pt-BR"] $ do
    head_ pageHead
    body_ [class_ "relative min-h-screen flex flex-col text-stone-800 antialiased bg-stone-50"] $ do
      decorativeBackground
      siteNav
      main_ [class_ "relative flex-1 w-full max-w-6xl mx-auto px-4 sm:px-6 py-8 sm:py-10"] content
      siteFooter

decorativeBackground :: Html ()
decorativeBackground =
  div_ [class_ "fixed inset-0 -z-10 overflow-hidden pointer-events-none"] $ do
    div_ [class_ "absolute -top-24 -left-24 w-[28rem] h-[28rem] rounded-full bg-emerald-200/50 blur-3xl"] emptyHtml
    div_ [class_ "absolute top-1/4 -right-32 w-[26rem] h-[26rem] rounded-full bg-sky-200/40 blur-3xl"] emptyHtml
    div_ [class_ "absolute bottom-0 left-1/3 w-[24rem] h-[24rem] rounded-full bg-amber-100/40 blur-3xl"] emptyHtml

-- ─── <head> ─────────────────────────────────────────────────────────────────

pageHead :: Html ()
pageHead = do
  meta_ [charset_ "utf-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
  title_ "PlantDiary"
  link_ [ rel_ "stylesheet"
        , href_ "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@500;600;700&family=Inter:wght@400;500;600;700&display=swap"
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
  [ "body { font-family: 'Inter', sans-serif; }"
  , ".font-serif { font-family: 'Playfair Display', serif; }"
  , "* { -webkit-tap-highlight-color: transparent; }"
  , ".card { transition: transform .2s ease, box-shadow .2s ease, border-color .2s ease; }"
  , ".card:hover { transform: translateY(-3px); box-shadow: 0 14px 32px rgba(15,23,15,.10); border-color: #a7d8b8; }"
  , ".btn { transition: background-color .15s ease, opacity .15s ease, transform .1s ease, box-shadow .15s ease; }"
  , ".btn:active { transform: scale(0.97); }"
  , ".link-fade { transition: color .15s ease; }"
  , "input, select, textarea { outline: none; transition: border-color .15s ease, box-shadow .15s ease; }"
  , "input:focus, select:focus, textarea:focus { border-color: #047857 !important; box-shadow: 0 0 0 3px rgba(4,120,87,.12); }"
  , "::selection { background: #d1fae5; }"
  , "@keyframes fadeUp { from { opacity: 0; transform: translateY(6px); } to { opacity: 1; transform: translateY(0); } }"
  , ".fade-up { animation: fadeUp .35s ease both; }"
  , ".icon-circle { display: inline-flex; align-items: center; justify-content: center; border-radius: 9999px; flex-shrink: 0; }"
  , ".toggle-box { transition: all .15s ease; }"
  , ".toggle-input:checked + .toggle-box { background: #059669; color: #fff; border-color: #059669; box-shadow: 0 6px 16px rgba(5,150,105,.3); }"
  , ".toggle-input:focus-visible + .toggle-box { box-shadow: 0 0 0 3px rgba(5,150,105,.3); }"
  ]

-- ─── Small helpers ──────────────────────────────────────────────────────────

icon :: Text -> Html ()
icon name = i_ [class_ $ "ti " <> name] emptyHtml

iconSz :: Text -> Text -> Html ()
iconSz name extraCls = i_ [class_ $ "ti " <> name <> " " <> extraCls] emptyHtml

checkedAttr :: Attribute
checkedAttr = makeAttribute "checked" "checked"

-- ─── Nav & Footer ───────────────────────────────────────────────────────────

siteNav :: Html ()
siteNav =
  nav_ [class_ "relative bg-stone-900 text-stone-100 sticky top-0 z-50"] $ do
    div_ [class_ "h-[3px] bg-gradient-to-r from-emerald-500 via-sky-400 to-amber-400"] emptyHtml
    div_ [class_ "max-w-6xl mx-auto px-4 sm:px-6 flex items-center justify-between h-16"] $ do
      a_ [href_ "/", class_ "flex items-center gap-2.5 text-base sm:text-lg font-serif font-semibold link-fade hover:text-emerald-400"] $ do
        iconSz "ti-leaf" "text-xl text-emerald-400"
        span_ "PlantDiary"
      div_ [class_ "flex items-center gap-3 sm:gap-7 text-sm font-medium"] $ do
        a_ [href_ "/", class_ "hidden sm:inline link-fade hover:text-emerald-400 text-stone-300"] "Início"
        a_ [href_ "/plantas", class_ "hidden sm:inline link-fade hover:text-emerald-400 text-stone-300"] "Minhas Plantas"
        a_ [href_ "/estatisticas", class_ "hidden sm:inline link-fade hover:text-emerald-400 text-stone-300"] "Estatísticas"
        a_ [ href_ "/plantas/nova"
           , class_ "flex items-center gap-1.5 bg-gradient-to-r from-emerald-600 to-emerald-500 hover:from-emerald-500 hover:to-emerald-400 text-white btn px-3.5 sm:px-4 py-2 rounded-lg font-medium text-xs sm:text-sm whitespace-nowrap shadow-sm"
           ] $ do
             iconSz "ti-plus" "text-base"
             span_ "Nova Planta"

siteFooter :: Html ()
siteFooter =
  footer_ [class_ "relative bg-stone-900 text-stone-500 text-center py-6 text-xs sm:text-sm mt-12"] $
    p_ [class_ "flex items-center justify-center gap-1.5"] $ do
      iconSz "ti-leaf" "text-emerald-500"
      span_ "PlantDiary — cuide das suas plantas com carinho"

-- ─── Dashboard ──────────────────────────────────────────────────────────────

dashboardView :: Day -> [(Entity Plant, Maybe Day)] -> [Entity CareLog] -> Html ()
dashboardView today plants logs = do
  let needWater = filter (needsAttention today) plants
  div_ [class_ "mb-10 fade-up"] $ do
    p_ [class_ "text-emerald-700 font-semibold mb-2 text-xs uppercase tracking-widest"] "Painel"
    h1_ [class_ "font-serif text-3xl sm:text-4xl font-semibold text-stone-800 mb-3 leading-tight"] "Visão geral do jardim"
    p_ [class_ "text-stone-500 text-base max-w-xl"] "Acompanhe o crescimento e os cuidados das suas plantas."

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
        then div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-stone-200 p-8 text-center"] $ do
               iconSz "ti-plant-2" "text-4xl text-stone-300 mb-3"
               p_ [class_ "text-stone-500 mb-5 text-sm"] "Você ainda não tem plantas cadastradas."
               a_ [href_ "/plantas/nova", class_ "btn inline-flex items-center gap-1.5 bg-emerald-600 hover:bg-emerald-500 text-white px-5 py-2.5 rounded-lg text-sm font-medium"] $ do
                 icon "ti-plus"
                 span_ "Cadastrar primeira planta"
        else div_ [class_ "space-y-2.5"] $ mapM_ (quickPlantRow today) (take 6 plants)

-- ─── Plant List ─────────────────────────────────────────────────────────────

plantListView :: Day -> [(Entity Plant, Maybe Day)] -> Html ()
plantListView today plants = do
  div_ [class_ "flex flex-wrap items-center justify-between gap-4 mb-6 fade-up"] $ do
    h1_ [class_ "font-serif text-3xl sm:text-4xl font-semibold text-stone-800"] "Minhas Plantas"
    a_ [href_ "/plantas/nova", class_ "btn flex items-center gap-1.5 bg-emerald-600 hover:bg-emerald-500 text-white px-4 py-2.5 rounded-lg text-sm font-medium whitespace-nowrap"] $ do
      icon "ti-plus"
      span_ "Nova Planta"

  if null plants
    then emptyState "ti-plant-2" "Nenhuma planta cadastrada."
                    "Comece adicionando sua primeira planta!"
    else do
      -- Busca instantânea (client-side)
      div_ [class_ "relative mb-6 fade-up"] $ do
        div_ [class_ "absolute left-3.5 top-1/2 -translate-y-1/2 text-stone-400"] $
          icon "ti-search"
        input_ [ type_ "text", id_ "plant-search"
               , placeholder_ "Buscar por nome, espécie ou local..."
               , class_ "w-full rounded-lg border border-stone-200 bg-white/90 backdrop-blur pl-10 pr-4 py-2.5 text-sm text-stone-800 placeholder-stone-400"
               , makeAttribute "oninput" "filterPlants(this.value)" ]
      p_ [id_ "no-results", class_ "hidden text-center text-stone-400 text-sm py-10"] "Nenhuma planta encontrada para a busca."
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

  div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-stone-200 overflow-hidden mb-8 fade-up"] $ do
    div_ [class_ "h-2 bg-gradient-to-r from-emerald-600 via-emerald-500 to-sky-500"] emptyHtml
    div_ [class_ "p-5 sm:p-7"] $ do
      div_ [class_ "flex flex-wrap items-start justify-between gap-4"] $ do
        div_ [class_ "flex items-start gap-4"] $ do
          div_ [class_ "icon-circle w-14 h-14 bg-emerald-50 text-emerald-600 text-2xl"] $
            icon (plantIcon (plantLocation plant))
          div_ $ do
            h1_ [class_ "font-serif text-2xl sm:text-3xl font-semibold text-stone-800"] $ toHtml $ plantName plant
            mapM_ (\sp -> p_ [class_ "text-stone-500 italic mt-0.5"] $ toHtml sp) (plantSpecies plant)
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
      div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-stone-200 p-5 sm:p-6"] $
        form_ [action_ (pidUrl pid <> "/registros"), method_ "post"] $ do
          formField "Data" $
            input_ [type_ "date", name_ "date", class_ inputCls]
          formField "Tipo de Cuidado" careTypeToggle
          formField "Observações (opcional)" $
            textarea_ [name_ "notes", rows_ "3", class_ inputCls, placeholder_ "Como a planta está?"] ""
          button_ [type_ "submit", class_ "w-full btn bg-emerald-600 hover:bg-emerald-500 text-white font-medium py-3 rounded-lg mt-2"] "Registrar cuidado"
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
    h1_ [class_ "font-serif text-3xl sm:text-4xl font-semibold text-stone-800 mb-2"] $ toHtml titleText
    p_ [class_ "text-stone-500 mb-8"] $
      if isEdit then "Atualize as informações da sua planta."
                else "Preencha os dados para cadastrar uma nova planta."

    div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-stone-200 p-5 sm:p-7"] $
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
          button_ [type_ "submit", class_ "flex-1 btn bg-emerald-600 hover:bg-emerald-500 text-white font-medium py-3 rounded-lg"] $
            toHtml submitText
          a_ [href_ "/plantas", class_ "btn flex-1 text-center bg-stone-100 hover:bg-stone-200 text-stone-700 font-medium py-3 rounded-lg"] "Cancelar"

-- ─── Error View ─────────────────────────────────────────────────────────────

errorView :: Text -> Html ()
errorView msg =
  div_ [class_ "max-w-md mx-auto text-center py-20 fade-up"] $ do
    iconSz "ti-alert-triangle" "text-5xl text-stone-300 mb-4"
    h1_ [class_ "font-serif text-2xl font-semibold text-stone-700 mb-2"] "Algo não foi encontrado"
    p_ [class_ "text-stone-500 mb-6"] $ toHtml msg
    a_ [href_ "/", class_ "btn inline-flex items-center gap-1.5 bg-emerald-600 hover:bg-emerald-500 text-white px-5 py-2.5 rounded-lg font-medium"] $ do
      icon "ti-arrow-left"
      span_ "Voltar ao início"

-- ─── Watering limit notice (gardener voice) ────────────────────────────────

wateringLimitView :: Int64 -> Html ()
wateringLimitView pid =
  div_ [class_ "max-w-lg mx-auto py-10 fade-up"] $
    div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-sky-200 overflow-hidden"] $ do
      div_ [class_ "h-2 bg-gradient-to-r from-sky-500 to-emerald-500"] emptyHtml
      div_ [class_ "p-7 text-center"] $ do
        div_ [class_ "icon-circle w-16 h-16 bg-sky-50 text-sky-600 text-3xl mx-auto mb-4"] $
          icon "ti-droplet-off"
        h1_ [class_ "font-serif text-2xl font-semibold text-stone-800 mb-3"] "Esta planta já foi regada hoje"
        p_ [class_ "text-stone-600 text-sm leading-relaxed mb-2"] $ do
          "O excesso de água é a "
          strong_ [class_ "font-semibold text-stone-700"] "principal causa de morte"
          " de plantas de interior: encharcar o substrato expulsa o oxigênio das raízes e favorece o apodrecimento (root rot) e fungos."
        p_ [class_ "text-stone-500 text-sm leading-relaxed mb-6"] $ do
          icon "ti-bulb"
          " A maioria das espécies só deve ser regada quando os 2–3 cm superiores do substrato estiverem secos ao toque — geralmente a cada poucos dias, nunca mais de uma vez por dia."
        a_ [href_ ("/plantas/" <> tshow pid), class_ "btn inline-flex items-center gap-1.5 bg-emerald-600 hover:bg-emerald-500 text-white px-5 py-2.5 rounded-lg font-medium"] $ do
          icon "ti-arrow-left"
          span_ "Voltar para a planta"

-- ─── Vitality meter (icon grows with consistent watering) ──────────────────

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
      ringStyle = "background: conic-gradient(#10b981 " <> tshow pct <> "%, #e2e8f0 0);"
  div_ [class_ "bg-gradient-to-br from-emerald-50 via-white to-sky-50 rounded-xl border border-stone-200 p-6 mb-6 text-center"] $ do
    p_ [class_ "text-xs uppercase tracking-widest text-emerald-700 font-semibold mb-4"] "Vitalidade da planta"
    div_ [class_ "inline-flex items-center justify-center rounded-full p-2 mb-4", style_ ringStyle] $
      div_ [class_ "rounded-full bg-white flex items-center justify-center", style_ "width:8.5rem;height:8.5rem;"] $
        iconSz ic (sizeCls <> " text-emerald-600")
    h3_ [class_ "font-serif text-xl font-semibold text-stone-800"] $ toHtml label
    p_ [class_ "text-stone-500 text-sm mt-1"] $
      toHtml (tshow w <> (if w == 1 then " rega registrada" else " regas registradas"))
    case next of
      Just n  -> p_ [class_ "inline-flex items-center gap-1.5 text-emerald-700 text-xs mt-3 font-medium bg-emerald-50 px-3 py-1 rounded-full"] $ do
                   iconSz "ti-arrow-up-right" "text-sm"
                   toHtml ("Faltam " <> tshow n <> (if n == 1 then " rega" else " regas") <> " para o próximo estágio")
      Nothing -> p_ [class_ "inline-flex items-center gap-1.5 text-teal-700 text-xs mt-3 font-medium bg-teal-50 px-3 py-1 rounded-full"] $ do
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

waterStatus :: Day -> Maybe Int -> Maybe Day -> WaterStatus
waterStatus _ Nothing _              = NoSchedule
waterStatus _ (Just _) Nothing       = NeverWatered
waterStatus today (Just iv) (Just w) =
  let nextDue = addDays (fromIntegral iv) w
      d       = diffDays nextDue today
  in if d < 0        then Overdue (fromInteger (negate d))
     else if d == 0  then DueToday
     else                 Upcoming (fromInteger d)

needsAttention :: Day -> (Entity Plant, Maybe Day) -> Bool
needsAttention today (Entity _ p, lastW) =
  case waterStatus today (plantWaterIntervalDays p) lastW of
    Overdue _    -> True
    DueToday     -> True
    NeverWatered -> True
    _            -> False

waterStatusBadge :: WaterStatus -> Html ()
waterStatusBadge st = case st of
  NoSchedule   -> badge "ti-droplet-off"     "Sem agenda"                       "bg-stone-100 text-stone-500"
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
  _            -> span_ [class_ "text-stone-400"] "Em dia"

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
    p_ [class_ "text-emerald-700 font-semibold mb-2 text-xs uppercase tracking-widest"] "Insights"
    h1_ [class_ "font-serif text-3xl sm:text-4xl font-semibold text-stone-800 mb-3 leading-tight"] "Estatísticas do jardim"
    p_ [class_ "text-stone-500 text-base max-w-xl"] "Um panorama de como você tem cuidado das suas plantas."

  div_ [class_ "grid grid-cols-2 lg:grid-cols-4 gap-4 mb-10 fade-up"] $ do
    statCard "ti-plant-2"        (tpack $ show $ length plants) "plantas"        "emerald"
    statCard "ti-clipboard-list" (tpack $ show total)           "cuidados totais" "sky"
    statCard "ti-droplet"        (tpack $ show waterCount)       "regas"          "amber"
    statCard "ti-map-pin"        (tpack $ show $ distinctLocations plants) "locais" "emerald"

  div_ [class_ "grid grid-cols-1 lg:grid-cols-2 gap-8"] $ do
    div_ $ do
      sectionTitle "Cuidados por tipo"
      div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-stone-200 p-5 space-y-3"] $
        if total == 0
          then p_ [class_ "text-stone-400 text-sm text-center py-6"] "Nenhum cuidado registrado ainda."
          else mapM_ (statBar maxC) rows

    div_ $ do
      sectionTitle "Plantas mais cuidadas"
      div_ [class_ "bg-white/90 backdrop-blur rounded-xl border border-stone-200 p-5"] $
        if null topPlants
          then p_ [class_ "text-stone-400 text-sm text-center py-6"] "Nenhum cuidado registrado ainda."
          else div_ [class_ "space-y-3"] $ mapM_ rankRow (zip [1 :: Int ..] topPlants)

statBar :: Int -> (Text, Text, Int) -> Html ()
statBar maxC (label, ic, c) = do
  let pct = c * 100 `div` maxC
  div_ $ do
    div_ [class_ "flex items-center justify-between text-sm mb-1"] $ do
      span_ [class_ "flex items-center gap-1.5 text-stone-600 font-medium"] $ do
        iconSz ic "text-base text-emerald-600"
        toHtml label
      span_ [class_ "text-stone-400 text-xs"] $ toHtml (tshow c)
    div_ [class_ "h-2 bg-stone-100 rounded-full overflow-hidden"] $
      div_ [class_ "h-full bg-gradient-to-r from-emerald-500 to-sky-500 rounded-full", style_ ("width:" <> tshow pct <> "%;")] emptyHtml

rankRow :: (Int, (Text, Int)) -> Html ()
rankRow (pos, (name, c)) =
  div_ [class_ "flex items-center gap-3"] $ do
    div_ [class_ "icon-circle w-8 h-8 bg-emerald-50 text-emerald-700 text-sm font-semibold"] $ toHtml (tshow pos)
    span_ [class_ "flex-1 min-w-0 truncate font-medium text-stone-700 text-sm"] $ toHtml name
    span_ [class_ "text-stone-400 text-xs"] $ toHtml (tshow c <> (if c == 1 then " cuidado" else " cuidados"))

distinctLocations :: [Entity Plant] -> Int
distinctLocations = length . dedupe . map (T.toLower . plantLocation . entityVal)
  where dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- ─── Components ─────────────────────────────────────────────────────────────

plantCard :: Day -> (Entity Plant, Maybe Day) -> Html ()
plantCard today (Entity pid plant, lastW) =
  div_ [ class_ "card bg-white/90 backdrop-blur rounded-xl border border-stone-200 overflow-hidden"
       , makeAttribute "data-search" (searchKey plant) ] $ do
    div_ [class_ "h-1.5 bg-gradient-to-r from-emerald-600 to-sky-500"] emptyHtml
    div_ [class_ "p-5"] $ do
      div_ [class_ "flex items-start gap-3 mb-3"] $ do
        div_ [class_ "icon-circle w-11 h-11 bg-emerald-50 text-emerald-600 text-lg"] $
          icon (plantIcon (plantLocation plant))
        div_ $ do
          h3_ [class_ "font-serif text-lg font-semibold text-stone-800 leading-tight"] $ toHtml $ plantName plant
          mapM_ (\sp -> p_ [class_ "text-stone-400 italic text-sm"] $ toHtml sp) (plantSpecies plant)
      div_ [class_ "flex flex-wrap gap-2 mb-4"] $ do
        badge "ti-map-pin" (plantLocation plant) "bg-stone-100 text-stone-500"
        growthBadge (plantHeightCm plant)
        waterStatusBadge (waterStatus today (plantWaterIntervalDays plant) lastW)
      a_ [href_ (pidUrl pid), class_ "btn flex items-center justify-center gap-1.5 bg-stone-900 hover:bg-stone-800 text-white rounded-lg py-2.5 text-sm font-medium"] $ do
        span_ "Ver detalhes"
        iconSz "ti-arrow-right" "text-base"

searchKey :: Plant -> Text
searchKey p = T.toLower $ T.intercalate " "
  [ plantName p, fromMaybe "" (plantSpecies p), plantLocation p ]

statCard :: Text -> Text -> Text -> Text -> Html ()
statCard iconName valueText label color =
  div_ [class_ "bg-white/90 backdrop-blur border border-stone-200 rounded-xl p-5 flex items-center gap-4"] $ do
    div_ [class_ $ "icon-circle w-12 h-12 text-xl " <> colorBg color <> " " <> colorText color] $
      icon iconName
    div_ $ do
      p_ [class_ "text-xl sm:text-2xl font-semibold font-serif text-stone-800"] $ toHtml valueText
      p_ [class_ "text-stone-500 text-xs sm:text-sm"] $ toHtml label

careLogItem :: Int64 -> Entity CareLog -> Html ()
careLogItem pid (Entity lid log_) =
  div_ [class_ "relative flex gap-4 pb-2"] $ do
    div_ [class_ $ "icon-circle w-10 h-10 text-lg ring-4 ring-stone-50 " <> careTypeBg (careLogCareType log_) <> " " <> careTypeColor (careLogCareType log_)] $
      icon (careTypeIcon (careLogCareType log_))
    div_ [class_ "bg-white rounded-lg p-4 flex-1 border border-stone-200"] $ do
      div_ [class_ "flex items-center justify-between gap-2 flex-wrap"] $ do
        span_ [class_ "font-medium text-stone-700"] $ toHtml $ careTypeLabel (careLogCareType log_)
        span_ [class_ "text-stone-400 text-sm"] $ toHtml $ fmtDay (careLogDate log_)
      mapM_ (\n -> p_ [class_ "text-stone-500 text-sm mt-1"] $ toHtml n)
            (careLogNotes log_)
      form_ [action_ ("/plantas/" <> tshow pid <> "/registros/" <> tshow (fromSqlKey lid) <> "/excluir"), method_ "post", class_ "mt-2"] $
        button_ [type_ "submit", class_ "text-xs text-stone-300 hover:text-red-500 link-fade"] "remover"

miniLogCard :: Entity CareLog -> Html ()
miniLogCard (Entity _ log_) =
  div_ [class_ "flex items-center gap-3 bg-white/90 backdrop-blur rounded-lg border border-stone-200 p-3.5"] $ do
    div_ [class_ $ "icon-circle w-9 h-9 text-base " <> careTypeBg (careLogCareType log_) <> " " <> careTypeColor (careLogCareType log_)] $
      icon (careTypeIcon (careLogCareType log_))
    div_ [class_ "flex-1 min-w-0"] $ do
      p_ [class_ "font-medium text-stone-700 text-sm truncate"] $ toHtml $ careTypeLabel (careLogCareType log_)
      p_ [class_ "text-stone-400 text-xs"] $ toHtml $ fmtDay (careLogDate log_)

quickPlantRow :: Day -> (Entity Plant, Maybe Day) -> Html ()
quickPlantRow today (Entity pid plant, lastW) =
  a_ [href_ (pidUrl pid), class_ "flex items-center gap-3 bg-white/90 backdrop-blur rounded-lg border border-stone-200 p-3.5 hover:border-emerald-300 transition-colors group"] $ do
    div_ [class_ "icon-circle w-10 h-10 bg-emerald-50 text-emerald-600 text-base"] $
      icon (plantIcon (plantLocation plant))
    div_ [class_ "flex-1 min-w-0"] $ do
      p_ [class_ "font-medium text-stone-700 text-sm truncate group-hover:text-emerald-700 transition-colors"] $ toHtml $ plantName plant
      p_ [class_ "text-stone-400 text-xs italic truncate"] $ toHtml $ fromMaybe "Espécie não informada" (plantSpecies plant)
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
        div_ [class_ "toggle-box flex flex-col items-center justify-center gap-1 px-1 py-3 rounded-xl border border-stone-200 bg-white text-stone-500 hover:border-emerald-300 hover:text-emerald-600 text-center select-none"] $ do
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
growthStage Nothing = ("ti-ruler-2", "Altura não registrada", "bg-stone-100", "text-stone-500")
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
    p_ [class_ "text-stone-400 text-sm"] $ toHtml desc

badge :: Text -> Text -> Text -> Html ()
badge iconName txt cls =
  span_ [class_ $ "inline-flex items-center gap-1.5 px-2.5 py-1 rounded-md text-xs font-medium " <> cls] $ do
    iconSz iconName "text-sm"
    toHtml txt

sectionTitle :: Text -> Html ()
sectionTitle t =
  h2_ [class_ "font-serif text-lg sm:text-xl font-semibold text-stone-700 mb-4"] $ toHtml t

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
inputCls = "w-full rounded-lg border border-stone-200 bg-stone-50 px-4 py-2.5 text-sm text-stone-800 placeholder-stone-400"

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
