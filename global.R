## Globala inställningar för Shinyappen: lokala_ekonomier

# Ladda nödvändiga paket
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(leaflet)
library(ggiraph)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(rdshinyappar)

options(shiny.sanitize.errors = TRUE)

# Färgpalett från stilmallen regiondalarna.css (grafiska manualens blå färgvektor).
# Röd används bara som varning eller status, så den finns inte i diagrammen.
rd_farg <- list(
  primar = "#158daf",      # --rd-primary, huvudfärg i diagram
  primar_mork = "#0f7090", # --rd-primary-dark
  accent = "#54a1bd",      # --rd-accent
  ljus = "#8edded",        # --rd-blue-light
  bla_djup = "#0074a2",    # --rd-blue-deep, markerar valt distrikt
  gra_mork = "#424242",    # --rd-gray-dark
  gra = "#969696",         # --rd-gray
  mark = "#d9ecf3",        # ljus accentton för markerad rad
  kvinnor = "#e2a855",     # rgb(226, 168, 85), kvinnor i staplar och linjer
  man = "#459079"          # rgb(69, 144, 121), män i staplar och linjer
)

#============================================================================
# DEL 1: Tabeller (kontrakt, validering, inläsning, exempeldata)
#============================================================================
## Fem tabeller ur schemat `ale`: inkomst, hushall_inkomst, ekonomisk_standard,
## sysselsattning_alder och sysselsattning_kon.
## Här finns kontrakt, validering, inläsning, exempeldata och hjälpfunktioner.
## Tabellerna förutsätts redan vara röjandegranskade. Appen granskar inget själv.

`%||%` <- function(a, b) if (is.null(a)) b else a

# nyckel = kolumner som identifierar en rad. matt = mått (NULL = alla övriga kolumner).
# antal  = mått som inte får vara negativa (inkomstmått kan vara negativa).
tabellkontrakt <- list(
  inkomst = list(
    nyckel = c("ar", "omrade", "kon", "aldersgrupp"),
    text = c("kon", "aldersgrupp"),
    matt = c("antal_ind", "median_ink", "summa_ink", "medel_ink"),
    antal = "antal_ind"
  ),
  # Flyttnetto per åldersgrupp och kön (kan vara negativt). Tabellen kan vara lång (en kolumn
  # kon med man/kvinnor/totalt) eller bred (kolumnerna man, kvinnor, totalt). Saknas både kon
  # och könskolumner tolkas raderna som Totalt. Saknas flyttnetto räknas det ut som
  # inflyttad minus utflyttad.
  flytt_alder = list(
    nyckel = c("ar", "omrade", "kon", "aldersgrupp"), text = c("kon", "aldersgrupp"),
    matt = "flyttnetto", antal = character()
  ),
  hushall_inkomst = list(
    nyckel = c("ar", "omrade"), text = character(),
    matt = c("antal_hus", "summa_ink", "medel_ink", "median_ink"),
    antal = "antal_hus"
  ),
  ekonomisk_standard = list(
    nyckel = c("ar", "omrade", "kvartil_ink"), text = "kvartil_ink",
    matt = c("antal_hus", "tot_hus", "andel"),
    antal = c("antal_hus", "tot_hus")
  ),
  sysselsattning_alder = list(
    nyckel = c("ar", "omrade", "block", "rad"), text = c("block", "rad"),
    matt = NULL, antal = NULL
  ),
  sysselsattning_kon = list(
    nyckel = c("ar", "omrade", "block", "rad"), text = c("block", "rad"),
    matt = c("man", "kvinnor"), antal = NULL
  ),
  # Befolkning och flytt över flera år per distrikt (längre serie än flytt_sum). Kolumner:
  # ar, omrade, man, kvinnor (befolkning räknas ut), inflyttad, utflyttad.
  flytt_serie_ar = list(
    nyckel = c("ar", "omrade"), text = character(), matt = "befolkning",
    antal = c("befolkning", "man", "kvinnor", "inflyttad", "utflyttad")  # flyttnetto kan vara negativt
  ),
  # De största pendlingsriktningarna ut från distriktet: en rad per år, distrikt och rang (1 = störst).
  # Kolumner som känns igen: rang, kommun (kod eller namn), antal. Se forbered_riktning().
  riktning_storst_pendling = list(
    nyckel = c("ar", "omrade", "rang"), text = character(), matt = "antal", antal = "antal"
  ),
  # Pendling per distrikt, år och kön (bred tabell). Kolumner som används: borarbetar,
  # inpendling, utpendling samt dagbef och nattbef om de finns. Övriga kolumner (t.ex.
  # uppgift_arb_saknas) behålls men visas inte.
  # Personbilar i trafik per distrikt och år, en kolumn per drivmedel (personbilar_bensin m.fl.).
  bilar_distrikt = list(
    nyckel = c("ar", "omrade"), text = character(), matt = NULL, antal = NULL
  ),
  pendling = list(
    nyckel = c("ar", "omrade", "kon"), text = "kon", matt = NULL,
    antal = c("borarbetar", "inpendling", "utpendling", "dagbef", "nattbef", "uppgift_arb_saknas")
  )
)

#---- Hjälpfunktioner -----------------------------------------------------------

# Könsvärden i databasen är gemener (man, kvinnor, totalt); appen använder Män/Kvinnor/Totalt.
normalisera_kon <- function(x) {
  k <- tolower(trimws(as.character(x)))
  ut <- rep(NA_character_, length(k))
  ut[k %in% c("man", "män", "1")] <- "Män"
  ut[k %in% c("kvinnor", "kvinna", "2")] <- "Kvinnor"
  ut[k %in% c("totalt", "total")] <- "Totalt"
  ut
}

# Sortera text med tal först (0 år, 1-3 år, 18-24 år ...). Text utan tal hamnar sist.
sortera_naturligt <- function(x) {
  x <- unique(as.character(x))
  tal <- suppressWarnings(as.numeric(sub("^\\D*(\\d+).*$", "\\1", x)))
  x[order(is.na(tal), tal, x)]
}

# Summa som förblir NA om alla värden saknas (saknat är inte noll).
summa_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

radetiketter <- c(
  totalt = "Totalt", okand = "Okänd", uppgift_saknas = "Uppgift saknas",
  stat = "Stat", region = "Region", kommun = "Kommun",
  privat_naringsliv = "Privat näringsliv", offentliga_bolag = "Offentliga bolag"
)
etikettera_rad <- function(x) {
  x <- as.character(x)
  ut <- unname(radetiketter[x])
  rest <- gsub("_", " ", x)
  ifelse(is.na(ut), paste0(toupper(substring(rest, 1, 1)), substring(rest, 2)), ut)
}

dt_alt <- function(...) {
  c(list(language = list(search = "Sök:", emptyTable = "Inga uppgifter",
                         zeroRecords = "Inga träffar")), list(...))
}

# Tabell med mellanslag som tusentalsavgränsare och kommatecken som decimaltecken.
visa_tabell <- function(df, heltal = character(), procent = character(), ...) {
  dt <- DT::datatable(df, rownames = FALSE, colnames = gsub("_", " ", names(df)),
                      escape = TRUE, options = dt_alt(...))
  h <- intersect(heltal, names(df))
  p <- intersect(procent, names(df))
  if (length(h)) dt <- DT::formatRound(dt, h, digits = 0, mark = " ")
  if (length(p)) dt <- DT::formatPercentage(dt, p, digits = 1, dec.mark = ",")
  dt
}

# Är Poppins installerat på maskinen som kör R? Då räknar diagrammen ut textens bredd med Poppins,
# så att etiketter inte överlappar. Annars används standardtypsnittet för måtten (webbläsaren ritar
# ändå Poppins, vilket är något bredare).
poppins_finns <- tryCatch(
  requireNamespace("systemfonts", quietly = TRUE) &&
    any(grepl("^Poppins", systemfonts::system_fonts()$family)),
  error = function(e) FALSE)
# Poppins för statiska diagram (cirkeldiagrammet ritas som bild i R, inte i webbläsaren, så typsnittet
# måste finnas för R). Ordning: 1) installerat på maskinen, 2) fontfil i appmappen
# (fonts/Poppins-Regular.ttf, ev. Poppins-Bold.ttf), 3) hämtas från Google Fonts (kräver internet).
# Returnerar typsnittsnamnet som diagrammet ska använda ("sans" om inget lyckas).
statisk_font <- local({
  klar <- NULL
  function() {
    if (!is.null(klar)) return(klar)
    klar <<- "sans"
    if (poppins_finns) {
      klar <<- "Poppins"
      # Standardenheten för bilder i Windows slår bara upp typsnitt i R:s egen Windows-lista
      # ("font family not found in Windows font database"). Därför: använd ragg om det finns
      # (läser systemets typsnitt direkt), annars registrera Poppins i Windows-listan.
      if (requireNamespace("ragg", quietly = TRUE)) {
        options(shiny.useragg = TRUE)
      } else if (.Platform$OS.type == "windows") {
        tryCatch(grDevices::windowsFonts(Poppins = grDevices::windowsFont("Poppins")),
                 error = function(e) { klar <<- "sans" })
      }
    } else if (requireNamespace("showtext", quietly = TRUE) && requireNamespace("sysfonts", quietly = TRUE)) {
      reg <- file.path("fonts", "Poppins-Regular.ttf")
      fet <- file.path("fonts", "Poppins-Bold.ttf")
      ok <- tryCatch({
        if (file.exists(reg)) {
          sysfonts::font_add("Poppins", regular = reg, bold = if (file.exists(fet)) fet else reg)
        } else {
          sysfonts::font_add_google("Poppins", "Poppins")
        }
        TRUE
      }, error = function(e) { message("Poppins kunde inte laddas för cirkeldiagrammet: ", conditionMessage(e)); FALSE })
      if (ok) {
        showtext::showtext_opts(dpi = 96)   # samma upplösning som renderPlot(res = 96)
        showtext::showtext_auto()
        klar <<- "Poppins"
      }
    }
    klar
  }
})
tema_app <- function() ggplot2::theme_minimal(base_size = 12, base_family = if (poppins_finns) "Poppins" else "")
# girafe med Poppins som typsnitt när det finns (och när ggiraph-versionen har argumentet fonts)
girafe_alt <- function(...) {
  if (poppins_finns && "fonts" %in% names(formals(girafe))) {
    girafe(..., fonts = list(sans = "Poppins"))
  } else girafe(...)
}

#---- Validering ----------------------------------------------------------------

validera_tabell <- function(df, namn, giltiga_omraden = NULL) {
  k <- tabellkontrakt[[namn]]
  fel <- function(text) stop(sprintf("%s: %s", namn, text), call. = FALSE)
  if (!is.data.frame(df)) fel("är ingen data.frame")
  matt <- if (is.null(k$matt)) setdiff(names(df), k$nyckel) else k$matt
  saknas <- setdiff(c(k$nyckel, matt), names(df))
  if (length(saknas)) fel(paste("saknar kolumner:", paste(saknas, collapse = ", ")))
  if (length(matt) < 1) fel("saknar mått")
  if (!is.character(df$omrade) || any(!nzchar(df$omrade), na.rm = TRUE)) {
    fel("omrade måste vara text utan tomma värden")
  }
  na_kol <- k$nyckel[vapply(df[k$nyckel], anyNA, logical(1))]
  if (length(na_kol)) fel(paste("saknade värden i nyckelkolumner:", paste(na_kol, collapse = ", ")))
  if (anyDuplicated(df[k$nyckel])) fel("dubbla nycklar")
  if (!is.numeric(df$ar) || any(!is.finite(df$ar)) || any(df$ar != trunc(df$ar))) {
    fel("ogiltiga år")
  }
  if (!all(vapply(df[matt], is.numeric, logical(1)))) fel("måtten måste vara numeriska")
  varden <- unlist(df[matt], use.names = FALSE)
  if (any(!is.finite(varden) & !is.na(varden))) fel("oändliga mått")
  antal <- intersect(if (is.null(k$antal)) matt else k$antal, matt)
  if (any(unlist(df[antal], use.names = FALSE) < 0, na.rm = TRUE)) fel("negativa antal")
  if ("andel" %in% matt && any(df$andel < 0 | df$andel > 1, na.rm = TRUE)) {
    fel("andel utanför 0–1")
  }
  if ("kon" %in% names(df) && !all(df$kon %in% c("Kvinnor", "Män", "Totalt"))) {
    fel("okända könsvärden")
  }
  if (!is.null(giltiga_omraden) && any(!df$omrade %in% c(giltiga_omraden, "(alla)"))) {
    fel("innehåller områden som saknas i omraden")
  }
  invisible(df)
}

validera_tabeller <- function(data) {
  for (namn in names(tabellkontrakt)) {
    df <- data[[namn]]
    if (is.null(df)) next
    validera_tabell(df, namn, data$omraden$omrade)
  }
  invisible(data)
}

#---- Inläsning ur databasen ----------------------------------------------------

till_tal <- function(x, kolumn, namn) {
  y <- suppressWarnings(as.numeric(x))
  if (anyNA(y) && !anyNA(x)) {
    stop(sprintf("%s: kolumnen %s innehåller värden som inte är tal", namn, kolumn),
         call. = FALSE)
  }
  y
}

# Flytt per åldersgrupp: gör om bred form (man, kvinnor, totalt) till lång form med kolumnen kon,
# och räkna ut flyttnetto om det saknas. Utan könsuppgift tolkas raderna som Totalt.
forbered_flytt_alder <- function(df) {
  bredkol <- intersect(c("man", "kvinnor", "totalt"), names(df))
  if (!"kon" %in% names(df) && length(bredkol) > 0 && !"flyttnetto" %in% names(df)) {
    df <- tidyr::pivot_longer(df, dplyr::all_of(bredkol), names_to = "kon", values_to = "flyttnetto")
    df <- as.data.frame(df)
  }
  if (!"kon" %in% names(df)) df$kon <- "Totalt"
  if (!"flyttnetto" %in% names(df) && all(c("inflyttad", "utflyttad") %in% names(df))) {
    df$flyttnetto <- as.numeric(df$inflyttad) - as.numeric(df$utflyttad)
  }
  df
}

# Gör om en tabell från databasen (gemena kolumnnamn) till appens kontrakt.
# Flytt över flera år: en rad per år och område. Befolkningen anges som kolumnerna man och
# kvinnor (befolkning = man + kvinnor räknas ut om kolumnen saknas). Inflyttade och utflyttade
# har ingen uppdelning på kön. Böjningsformer av kolumnnamnen godtas. Har tabellen fler
# uppdelningar (t.ex. åldersgrupp) används raden för totalt om den finns, annars summeras den.
forbered_flytt_serie <- function(df) {
  byt <- c(inflyttade = "inflyttad", utflyttade = "utflyttad", invanare = "befolkning",
           folkmangd = "befolkning", befolkning_totalt = "befolkning",
           män = "man", kvinna = "kvinnor")
  traff <- names(df) %in% names(byt)
  names(df)[traff] <- unname(byt[names(df)[traff]])
  if (!all(c("ar", "omrade") %in% names(df))) return(df)
  tal <- intersect(c("man", "kvinnor", "befolkning", "inflyttad", "utflyttad", "flyttnetto"), names(df))
  for (kol in tal) df[[kol]] <- suppressWarnings(as.numeric(df[[kol]]))
  # Övriga textkolumner är extra uppdelningar. Raden för totalt används om den finns.
  extra <- setdiff(names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))],
                   c("ar", "omrade"))
  for (e in extra) {
    v <- tolower(trimws(as.character(df[[e]])))
    tot <- v %in% c("totalt", "total", "alla", "samtliga", "summa", "totalt alla")
    if (any(tot)) df <- df[tot, , drop = FALSE]
  }
  if ("kon" %in% names(df)) df$kon <- NULL
  nyckel <- c("ar", "omrade")
  if (length(tal) && anyDuplicated(df[nyckel])) {
    df <- stats::aggregate(df[tal], df[nyckel], function(x) summa_na(x))
  }
  if (!"befolkning" %in% names(df) && all(c("man", "kvinnor") %in% names(df))) {
    df$befolkning <- ifelse(is.na(df$man) & is.na(df$kvinnor), NA_real_,
                            rowSums(df[c("man", "kvinnor")], na.rm = TRUE))
  }
  if (!"flyttnetto" %in% names(df) && all(c("inflyttad", "utflyttad") %in% names(df))) {
    df$flyttnetto <- df$inflyttad - df$utflyttad
  }
  df
}

# De största pendlingsriktningarna. Kolumnerna identifieras på namn:
#  - kommun: kod (fyra siffror) eller namn, t.ex. kommun, kommunkod, till_kommun, arbetskommun, riktning
#  - antal: antal, pendlare, utpendlare
#  - rang: rang, rank, plats (saknas den rangordnas raderna efter antal)
# Resultatet har kolumnerna ar, omrade, rang, kommunkod, kommunnamn och antal.
forbered_riktning <- function(df) {
  forst <- function(kandidater) { i <- kandidater[kandidater %in% names(df)]; if (length(i)) i[1] else NA_character_ }
  if (!"ar" %in% names(df)) return(df)
  # Saknas kolumn för distrikt gäller tabellen hela regionen: samma riktningar visas för alla
  # distrikt (markeras med omrade = "(alla)" och förklaras i kartkortet).
  if (!"omrade" %in% names(df)) df$omrade <- "(alla)"
  kol_kommun <- forst(c("kommunkod", "kommun_kod", "kommunkod_tx", "till_kommunkod", "arbkommunkod",
                        "kommun", "till_kommun", "tillkommun", "mal_kommun", "malkommun", "arbetskommun",
                        "arbkommun", "kommunnamn", "kommun_namn", "riktning", "destination", "mal"))
  if (is.na(kol_kommun)) {
    stop(sprintf("riktning_storst_pendling: hittar ingen kolumn för kommun. Kolumner i tabellen: %s",
                 paste(names(df), collapse = ", ")), call. = FALSE)
  }
  kol_namn <- forst(c("kommunnamn", "kommun_namn", "till_kommun", "arbetskommun", "namn"))
  kol_antal <- forst(c("antal", "pendlare", "utpendlare", "antal_pendlare", "antal_utpendlare", "utpendling"))
  kol_rang <- forst(c("rang", "rank", "plats", "ordning", "placering", "nr"))
  kol_kon <- forst("kon")
  if (!is.na(kol_kon)) {
    k <- normalisera_kon(df$kon)
    if (any(k %in% "Totalt")) df <- df[k %in% "Totalt" | is.na(k), , drop = FALSE]
  }
  rakna <- trimws(as.character(df[[kol_kommun]]))
  ar_kod <- all(grepl("^\\d{3,4}$", rakna[!is.na(rakna)]))
  ut <- data.frame(ar = df$ar, omrade = df$omrade,
                   kommunkod = if (ar_kod) sprintf("%04d", suppressWarnings(as.integer(rakna))) else NA_character_,
                   kommunnamn = if (ar_kod) {
                     if (!is.na(kol_namn) && kol_namn != kol_kommun) as.character(df[[kol_namn]]) else NA_character_
                   } else rakna,
                   antal = if (!is.na(kol_antal)) suppressWarnings(as.numeric(df[[kol_antal]])) else NA_real_,
                   stringsAsFactors = FALSE)
  ut$rang <- if (!is.na(kol_rang)) suppressWarnings(as.integer(df[[kol_rang]])) else NA_integer_
  if (anyNA(ut$rang)) {
    # Rangordna efter antal (störst först), annars efter radordning, inom år och distrikt
    ordn <- order(ut$ar, ut$omrade, -ifelse(is.na(ut$antal), -Inf, ut$antal))
    ut <- ut[ordn, , drop = FALSE]
    ut$rang <- stats::ave(seq_len(nrow(ut)), ut$ar, ut$omrade, FUN = seq_along)
  }
  ut[c("ar", "omrade", "rang", "kommunkod", "kommunnamn", "antal")]
}

normalisera_tabell <- function(df, namn) {
  k <- tabellkontrakt[[namn]]
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  if (namn == "flytt_alder") df <- forbered_flytt_alder(df)
  if (namn == "flytt_serie_ar") df <- forbered_flytt_serie(df)
  if (namn == "riktning_storst_pendling") df <- forbered_riktning(df)
  saknas <- setdiff(k$nyckel, names(df))
  if (length(saknas)) {
    stop(sprintf("%s: saknar nyckelkolumner %s. Kolumner i tabellen: %s", namn,
                 paste(saknas, collapse = ", "), paste(names(df), collapse = ", ")),
         call. = FALSE)
  }
  df$ar <- as.integer(df$ar)
  df$omrade <- as.character(df$omrade)
  for (kol in intersect(k$text, names(df))) df[[kol]] <- as.character(df[[kol]])

  # Rader utan år eller område kan inte placeras och utelämnas.
  utan_plats <- is.na(df$ar) | is.na(df$omrade) | !nzchar(df$omrade)
  if (any(utan_plats)) {
    message(namn, ": ", sum(utan_plats), " rader utan år eller område utelämnas.")
    df <- df[!utan_plats, , drop = FALSE]
  }
  # Saknad textkategori (t.ex. kvartil, block, rad) blir egen kategori, inte borttappad.
  for (kol in setdiff(intersect(k$text, names(df)), "kon")) {
    n_na <- sum(is.na(df[[kol]]))
    if (n_na > 0) {
      message(namn, ": ", n_na, " rader med saknat värde i ", kol, " får kategorin 'Uppgift saknas'.")
      df[[kol]][is.na(df[[kol]])] <- "Uppgift saknas"
    }
  }

  if ("kon" %in% names(df)) {
    df$kon <- normalisera_kon(df$kon)
    n_na <- sum(is.na(df$kon))
    if (n_na > 0) {
      message(namn, ": ", n_na, " rader med okänt kön utelämnas.")
      df <- df[!is.na(df$kon), , drop = FALSE]
    }
  }
  if ("aldersgrupp" %in% names(df)) df$aldersgrupp[is.na(df$aldersgrupp)] <- "Uppgift saknas"

  if (namn == "ekonomisk_standard" && !"andel" %in% names(df) &&
      all(c("antal_hus", "tot_hus") %in% names(df))) {
    df$andel <- ifelse(df$tot_hus > 0, df$antal_hus / df$tot_hus, NA_real_)
  }
  matt <- if (is.null(k$matt)) setdiff(names(df), k$nyckel) else k$matt
  # Flytt över flera år: befolkning krävs, övriga flyttmått behålls om de finns
  if (namn == "flytt_serie_ar") {
    matt <- union("befolkning", intersect(c("man", "kvinnor", "inflyttad", "utflyttad", "flyttnetto"), names(df)))
  }
  saknas_matt <- setdiff(matt, names(df))
  if (length(saknas_matt)) {
    stop(sprintf("%s: saknar mått %s. Kolumner i tabellen: %s", namn,
                 paste(saknas_matt, collapse = ", "), paste(names(df), collapse = ", ")),
         call. = FALSE)
  }
  # Dubbla nycklar beror oftast på en extra kolumn som skiljer raderna åt (kön, typ, riktning).
  # Den tas annars bort tyst, så den namnges här.
  if (anyDuplicated(df[k$nyckel])) {
    extra <- setdiff(names(df), c(k$nyckel, matt))
    d <- df[duplicated(df[k$nyckel]), k$nyckel, drop = FALSE][1, , drop = FALSE]
    stop(sprintf("%s: dubbla nycklar, t.ex. %s. Övriga kolumner i tabellen som kan skilja raderna åt: %s",
                 namn, paste(names(d), unlist(d), sep = "=", collapse = ", "),
                 if (length(extra)) paste(extra, collapse = ", ") else "inga"), call. = FALSE)
  }
  # Riktningstabellen behåller kommunkod och kommunnamn (text) utöver nyckel och mått
  behall <- if (namn == "riktning_storst_pendling") c("kommunkod", "kommunnamn") else character()
  df <- df[c(k$nyckel, behall, matt)]
  for (kol in matt) df[[kol]] <- till_tal(df[[kol]], kol, namn)
  df
}

# `las` är en funktion(tabellnamn) som returnerar en data.frame ur schemat.
# En tabell som inte går att läsa blir NULL och orsaken sparas; övriga läses ändå.
hamta_fem_tabeller <- function(las, inst) {
  tabeller <- list()
  fel <- character()
  for (namn in names(tabellkontrakt)) {
    resultat <- tryCatch({
      df <- normalisera_tabell(las(inst$tabeller[[namn]] %||% namn), namn)
      validera_tabell(df, namn)
      df
    }, error = function(e) {
      message("Tabellen ", namn, " kunde inte läsas: ", conditionMessage(e))
      fel[[namn]] <<- conditionMessage(e)
      NULL
    })
    tabeller[namn] <- list(resultat)
  }
  # Kommuntabellen för bilar ligger utanför kontraktet (ingen omrade-kolumn)
  tabeller["bilar_kommun"] <- list(tryCatch(
    forbered_bilar_kommun(las(inst$tabeller$bilar_kommun %||% "bilar_kommun")),
    error = function(e) {
      message("Tabellen bilar_kommun kunde inte läsas: ", conditionMessage(e))
      fel[["bilar_kommun"]] <<- conditionMessage(e)
      NULL
    }))
  list(tabeller = tabeller, fel = fel)
}

# Tabell med könen som kolumngrupper bredvid varandra: en rubrikrad med Kvinnor och Män (colspan)
# och under den delkolumnerna (t.ex. år), med samma rader för båda könen.
#  rader   : data.frame med radetiketter (en rad per tabellrad), t.ex. Mått eller Åldersgrupp
#  grupper : namngiven lista, ett data.frame per grupp (kön) med lika många rader som `rader`;
#            kolumnnamnen (t.ex. år) blir delkolumner och ska vara lika i alla grupper
#  markera : delkolumner som får markeringsfärg (t.ex. valt år)
#  fet_rad : c(kolumn, värde): rader där kolumnen har värdet visas med fet text (t.ex. Totalt)
grupperad_tabell <- function(rader, grupper, decimaler = 0, markera = character(),
                             fet_rad = NULL, ...) {
  delnamn <- names(grupper[[1]])
  vardekol <- unlist(lapply(names(grupper), function(g) paste(g, delnamn, sep = "||")))
  vard <- do.call(cbind, lapply(names(grupper), function(g) {
    x <- as.data.frame(grupper[[g]], check.names = FALSE)
    setNames(x, paste(g, names(x), sep = "||"))
  }))
  df <- cbind(as.data.frame(rader, check.names = FALSE, stringsAsFactors = FALSE), vard)
  n_rad <- ncol(as.data.frame(rader))
  starter <- n_rad + (seq_along(grupper) - 1) * length(delnamn)   # 0-baserade kolumner där en grupp börjar
  th <- htmltools::tags$th
  rubrik1 <- htmltools::tags$tr(
    lapply(names(rader), function(n) th(rowspan = 2, gsub("_", " ", n))),
    lapply(seq_along(grupper), function(i)
      th(colspan = length(delnamn), class = "dt-center grupp-start", names(grupper)[i])))
  rubrik2 <- htmltools::tags$tr(
    lapply(seq_along(vardekol), function(i)
      th(delnamn[(i - 1) %% length(delnamn) + 1],
         class = if ((i - 1) %% length(delnamn) == 0) "grupp-start" else NULL)))
  container <- htmltools::tags$table(class = "display", htmltools::tags$thead(rubrik1, rubrik2))
  tillval <- dt_alt(...)
  tillval$columnDefs <- list(list(className = "grupp-start", targets = as.list(starter)))
  dt <- DT::datatable(df, container = container, rownames = FALSE, escape = TRUE, options = tillval)
  dt <- DT::formatRound(dt, vardekol, digits = decimaler, mark = " ", dec.mark = ",")
  for (k in vardekol[sub("^.*\\|\\|", "", vardekol) %in% markera]) {
    dt <- DT::formatStyle(dt, k, backgroundColor = rd_farg$mark)
  }
  if (!is.null(fet_rad)) {
    dt <- DT::formatStyle(dt, fet_rad[1], target = "row",
                          fontWeight = DT::styleEqual(fet_rad[2], "bold"))
  }
  dt
}

#---- Bilinnehav -----------------------------------------------------------------
# Drivmedel i tabellerna bilar_distrikt och bilar_kommun (kolumnerna heter personbilar_<nyckel>).
bilar_drivmedel <- c(bensin = "Bensin", diesel = "Diesel", etanol = "Etanol", gas = "Gas",
                     elhybrid = "Elhybrid", laddhybrid = "Laddhybrid", el = "El", ovriga = "Övriga")
bilar_kolumner <- paste0("personbilar_", names(bilar_drivmedel))
# Fossila drivmedel i grått, eldrivna i stilmallens blå toner
bilar_farger <- c(Bensin = "#424242", Diesel = "#969696", Etanol = "#c9c9c9", Gas = "#96cfc0",
                  Elhybrid = "#54a1bd", Laddhybrid = "#8edded", El = "#0f7090", Övriga = "#e6e6e6")
# Mått att jämföra (andel av alla personbilar, procent)
bilar_matt <- c("El (helt eldrivna)" = "el", "Laddbara (el och laddhybrid)" = "laddbara",
                "Elektrifierade (el, elhybrid och laddhybrid)" = "elektrifierade",
                "Bensin" = "bensin", "Diesel" = "diesel")
bilar_matt_kolumner <- list(el = "el", laddbara = c("el", "laddhybrid"),
                            elektrifierade = c("el", "elhybrid", "laddhybrid"),
                            bensin = "bensin", diesel = "diesel")

# Andel (procent) av alla personbilar för ett mått. Saknad total ger NA.
bilar_andel <- function(df, matt) {
  kol <- intersect(paste0("personbilar_", bilar_matt_kolumner[[matt]]), names(df))
  alla <- intersect(bilar_kolumner, names(df))
  tot <- rowSums(df[alla], na.rm = TRUE)
  ifelse(tot > 0, rowSums(df[kol], na.rm = TRUE) / tot * 100, NA_real_)
}

# Kommun för demodistrikten (två kommuner). För riktiga distrikt säger koden inget om kommunen;
# där hämtas kommunkoden ur kartans geometri (se hamta_appdata_db, kolumnen kommunkod i omraden).
kommun_for_distrikt <- function(omrade) {
  ifelse(grepl("^demo_\\d+$", omrade),
         ifelse(suppressWarnings(as.integer(sub("demo_", "", omrade))) <= 4, "Exempelkommun A", "Exempelkommun B"),
         NA_character_)
}

# Tabellen bilar_kommun har kommun (kod eller namn) i stället för omrade, så den ligger utanför
# tabellkontraktet och kontrolleras här. Kommunkolumnen kan också innehålla län och riket.
forbered_bilar_kommun <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  if (!all(c("ar", "kommun") %in% names(df))) {
    stop(sprintf("bilar_kommun: saknar ar eller kommun. Kolumner i tabellen: %s",
                 paste(names(df), collapse = ", ")), call. = FALSE)
  }
  kol <- intersect(bilar_kolumner, names(df))
  if (!length(kol)) {
    stop(sprintf("bilar_kommun: hittar inga personbilar_-kolumner. Kolumner i tabellen: %s",
                 paste(names(df), collapse = ", ")), call. = FALSE)
  }
  df$ar <- as.integer(df$ar)
  df$kommun <- trimws(as.character(df$kommun))
  df <- df[!is.na(df$ar) & !is.na(df$kommun) & nzchar(df$kommun), , drop = FALSE]
  for (k in kol) df[[k]] <- till_tal(df[[k]], k, "bilar_kommun")
  if (anyDuplicated(df[c("ar", "kommun")])) {
    stop("bilar_kommun: dubbla rader för samma år och kommun.", call. = FALSE)
  }
  if (any(unlist(df[kol]) < 0, na.rm = TRUE)) stop("bilar_kommun: negativa antal.", call. = FALSE)
  df[c("ar", "kommun", kol)]
}

#---- Färger för näringsgrenar (ur Bransch_Gxx_farger.xlsx) ----------------------------
# 15 branschgrupper (G01–G15) plus Okänt. Flera näringsgrenar ingår i samma grupp (t.ex. B+C) och
# får då gruppens färg; den andra och följande näringsgrenen i en grupp får samma färg
# blandad med vitt, så att intilliggande staplar går att skilja åt.
bransch_grupper <- data.frame(
  brkod = c("G01", "G02", "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11", "G12", "G13", "G14", "G15", "G99"),
  br15kod = c("A", "B+C", "D+E", "F", "G", "H", "I", "J", "K", "L", "M+N", "O", "P", "Q", "R+S+T+U", "US"),
  bransch = c("Jord- och skogsbruk", "Tillverkning och utvinning", "Energi och miljö", "Bygg", "Handel",
              "Transport", "Hotell och restaurang", "IT och kommunikation", "Finans och försäkring", "Fastighet",
              "Företagstjänster", "Offentlig förvaltning", "Utbildning", "Vård och omsorg",
              "Kultur, fritid och nöje", "Okänt"),
  hex = c("#87f7b4", "#36e60a", "#10d0fa", "#eb12c8", "#f1fa86", "#366631", "#faff01", "#b1dfed",
          "#0000ff", "#ec9eed", "#94bf69", "#fcac34", "#8f0e25", "#f5370d", "#660080", "#ebe2e2"),
  stringsAsFactors = FALSE
)
# Näringsgrenens namn (SNI 2007-avdelning) -> grupp och plats i gruppen. Mönstren prövas i ordning;
# . står för å, ä och ö så att kodningen inte spelar roll.
bransch_regler <- data.frame(
  monster = c("ok.nd|ok.nt|^okant|uppgift saknas", "vattenf.rs.rjning|avfall", "f.rs.rjning av el|el, gas",
              "utvinning", "tillverkning", "jordbruk|skogsbruk", "bygg", "^handel", "transport", "hotell",
              "information|kommunikation", "finans", "^fastighetsverksamhet|^fastighet$", "juridik|ekonomi, vetenskap",
              "uthyrning", "offentlig", "^utbildning", "v.rd och omsorg", "kultur", "annan serviceverksamhet",
              "hush.ll", "internationella|ambassader"),
  brkod = c("G99", "G03", "G03", "G02", "G02", "G01", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11",
            "G11", "G12", "G13", "G14", "G15", "G15", "G15", "G15"),
  medlem = c(1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 3, 4),
  stringsAsFactors = FALSE
)
# SNI-bokstav (A–U) -> grupp och plats, om näringsgrenarna anges med kod i stället för namn
bransch_bokstav <- data.frame(
  bokstav = LETTERS[1:21],
  brkod = c("G01", "G02", "G02", "G03", "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11", "G11",
            "G12", "G13", "G14", "G15", "G15", "G15", "G15"),
  medlem = c(1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 3, 4),
  stringsAsFactors = FALSE
)
# Färg för varje namn i `rader`; NA för namn som inte går att koppla till en grupp.
bransch_farg_for <- function(rader) {
  x <- tolower(trimws(as.character(rader)))
  grupp <- rep(NA_character_, length(x)); medlem <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) next
    if (grepl("^g\\d\\d$", x[i]) && toupper(x[i]) %in% bransch_grupper$brkod) {
      grupp[i] <- toupper(x[i]); medlem[i] <- 1; next
    }
    if (grepl("^[a-u]$", x[i])) {
      j <- match(toupper(x[i]), bransch_bokstav$bokstav)
      grupp[i] <- bransch_bokstav$brkod[j]; medlem[i] <- bransch_bokstav$medlem[j]; next
    }
    for (j in seq_len(nrow(bransch_regler))) {
      if (grepl(bransch_regler$monster[j], x[i], perl = FALSE)) {
        grupp[i] <- bransch_regler$brkod[j]; medlem[i] <- bransch_regler$medlem[j]; break
      }
    }
  }
  hex <- bransch_grupper$hex[match(grupp, bransch_grupper$brkod)]
  andel_vitt <- c(0, 0.35, 0.55, 0.7)[pmin(medlem, 4)]
  ut <- vapply(seq_along(hex), function(i) {
    if (is.na(hex[i])) return(NA_character_)
    rgb <- grDevices::col2rgb(hex[i])[, 1]
    m <- round(rgb + (255 - rgb) * andel_vitt[i])
    grDevices::rgb(m[1], m[2], m[3], maxColorValue = 255)
  }, character(1))
  setNames(ut, rader)
}

#---- Exempeldata -----------------------------------------------------------------

skapa_exempeltabeller <- function(omraden) {
  rutnat <- expand.grid(ar = 2020:2024, omrade = omraden$omrade, stringsAsFactors = FALSE)
  rutnat$i <- match(rutnat$omrade, omraden$omrade)
  rutnat$t <- rutnat$ar - 2020

  # Inkomst: som i riktiga data finns bara Kvinnor och Män, ingen totalrad.
  aldersgrupper <- c("0 år", "1-3 år", "4-6 år", "7-10 år", "11-14 år", "15-17 år",
                     "18-24 år", "25-50 år", "51-70 år", "71+ år")
  niva <- c(0.95, 0.95, 0.97, 0.98, 1.00, 1.02, 0.70, 1.00, 1.15, 0.85)
  inkomst <- merge(rutnat, expand.grid(kon = c("Kvinnor", "Män"), aldersgrupp = aldersgrupper,
                                       stringsAsFactors = FALSE))
  g <- match(inkomst$aldersgrupp, aldersgrupper)
  inkomst$antal_ind <- 15 + inkomst$i * 2 + g * 3 + inkomst$t
  inkomst$medel_ink <- round(190000 * niva[g] * (1 + 0.012 * inkomst$i + 0.025 * inkomst$t) *
                               ifelse(inkomst$kon == "Män", 1.08, 0.94))
  inkomst$median_ink <- round(inkomst$medel_ink * 0.92)
  inkomst$summa_ink <- inkomst$antal_ind * inkomst$medel_ink
  inkomst <- inkomst[order(inkomst$ar, inkomst$omrade, inkomst$kon, g), ]
  inkomst <- inkomst[c("ar", "omrade", "kon", "aldersgrupp",
                       "antal_ind", "median_ink", "summa_ink", "medel_ink")]

  # Hushållens disponibla inkomst
  hushall <- data.frame(ar = rutnat$ar, omrade = rutnat$omrade,
                        antal_hus = 300 + rutnat$i * 80 + rutnat$t * 4,
                        stringsAsFactors = FALSE)
  hushall$medel_ink <- round(330000 + rutnat$i * 7000 + rutnat$t * 11000, -2)
  hushall$median_ink <- round(hushall$medel_ink * 0.9, -2)
  hushall$summa_ink <- hushall$antal_hus * hushall$medel_ink

  # Ekonomisk standard: kvartilgränserna är rikets, så 25 % per kvartil = som riket.
  ek <- merge(rutnat, data.frame(kvartil_ink = paste("kvartil", 1:4), stringsAsFactors = FALSE))
  ek$q <- as.integer(sub("kvartil ", "", ek$kvartil_ink))
  ek$tot_hus <- 300 + ek$i * 80 + ek$t * 4
  ek$p <- c(0.31, 0.27, 0.23, 0.19)[ek$q] + (ek$q - 2.5) * 0.01 * (ek$i - 4.5)
  ek <- ek |>
    dplyr::group_by(ar, omrade) |>
    dplyr::mutate(antal_hus = round(tot_hus * p),
                  antal_hus = dplyr::if_else(q == 4L, tot_hus - sum(antal_hus[q < 4L]), antal_hus)) |>
    dplyr::ungroup() |>
    dplyr::arrange(ar, omrade, q) |>
    as.data.frame()
  ek$andel <- ek$antal_hus / ek$tot_hus
  ek <- ek[c("ar", "omrade", "kvartil_ink", "antal_hus", "tot_hus", "andel")]

  # Sysselsättning. Rader inom ett block är här delmängder av totalen i demonstrationen;
  # i riktiga data kan rader överlappa, så summera aldrig över rader.
  ageklasser <- c("16-24 år", "25-44 år", "45-65 år", "66+ år")
  rader <- rbind(
    data.frame(block = "naringsgren", rad = c(paste("Bransch", LETTERS[1:6]), "okand"),
               andel = c(.18, .22, .15, .20, .17, .06, .02), stringsAsFactors = FALSE),
    data.frame(block = "sektor",
               rad = c("stat", "region", "kommun", "privat_naringsliv", "offentliga_bolag", "uppgift_saknas"),
               andel = c(.05, .12, .28, .45, .06, .04), stringsAsFactors = FALSE),
    data.frame(block = "totalt", rad = "totalt", andel = 1, stringsAsFactors = FALSE)
  )
  lang <- merge(merge(rutnat, rader),
                data.frame(alder = ageklasser, vikt = c(.10, .38, .42, .10), stringsAsFactors = FALSE))
  lang$antal <- as.integer(round((360 + lang$i * 100 + lang$t * 5) * lang$vikt * lang$andel))
  syss_alder <- as.data.frame(tidyr::pivot_wider(
    lang[c("ar", "omrade", "block", "rad", "alder", "antal")],
    names_from = alder, values_from = antal
  ))
  syss_alder <- syss_alder[order(syss_alder$ar, syss_alder$omrade, syss_alder$block, syss_alder$rad),
                           c("ar", "omrade", "block", "rad", ageklasser)]
  rownames(syss_alder) <- NULL

  syss_kon <- syss_alder[c("ar", "omrade", "block", "rad")]
  tot <- rowSums(syss_alder[ageklasser])
  syss_kon$man <- as.integer(round(tot * 0.52))
  syss_kon$kvinnor <- as.integer(tot - syss_kon$man)

  # Flytt per åldersgrupp. Summan över åldersgrupper blir samma som totalen för distriktet.
  flytt_ag <- c("0-16", "17-24", "25-34", "35-44", "45-54", "55-64", "65+")
  tot_in <- 40 + rutnat$i * 8 + rutnat$t * 3
  tot_ut <- 60 + rutnat$i * 3 + rutnat$t * 2
  vikt_in <- c(.08, .24, .26, .20, .10, .07, .05)
  vikt_ut <- c(.07, .30, .22, .16, .10, .08, .07)
  fordela <- function(tot, vikt) {
    m <- round(outer(tot, vikt))
    m[, ncol(m)] <- tot - rowSums(m[, -ncol(m), drop = FALSE])
    m
  }
  m_in <- fordela(tot_in, vikt_in); m_ut <- fordela(tot_ut, vikt_ut)
  flytt_alder <- data.frame(
    ar = rep(rutnat$ar, times = length(flytt_ag)),
    omrade = rep(rutnat$omrade, times = length(flytt_ag)),
    aldersgrupp = rep(flytt_ag, each = nrow(rutnat)),
    inflyttad = as.vector(m_in), utflyttad = as.vector(m_ut), stringsAsFactors = FALSE)
  flytt_alder$flyttnetto <- flytt_alder$inflyttad - flytt_alder$utflyttad
  # Könsuppdelning: männen står för ungefär hälften, kvinnorna för resten, Totalt är summan.
  man <- flytt_alder
  man$inflyttad <- round(man$inflyttad * 0.47); man$utflyttad <- round(man$utflyttad * 0.52)
  kvinnor <- flytt_alder
  kvinnor$inflyttad <- flytt_alder$inflyttad - man$inflyttad
  kvinnor$utflyttad <- flytt_alder$utflyttad - man$utflyttad
  man$flyttnetto <- man$inflyttad - man$utflyttad
  kvinnor$flyttnetto <- kvinnor$inflyttad - kvinnor$utflyttad
  flytt_alder <- rbind(cbind(flytt_alder, kon = "Totalt"), cbind(man, kon = "Män"),
                       cbind(kvinnor, kon = "Kvinnor"))
  flytt_alder <- flytt_alder[c("ar", "omrade", "kon", "aldersgrupp",
                               "inflyttad", "utflyttad", "flyttnetto")]
  flytt_alder <- flytt_alder[order(flytt_alder$ar, flytt_alder$omrade,
                                   match(flytt_alder$kon, c("Totalt", "Män", "Kvinnor")),
                                   match(flytt_alder$aldersgrupp, flytt_ag)), ]
  rownames(flytt_alder) <- NULL

  # Längre serie för befolkning och flytt (2012-2024)
  serie <- expand.grid(ar = 2012:2024, omrade = omraden$omrade, stringsAsFactors = FALSE)
  si <- match(serie$omrade, omraden$omrade); st <- serie$ar - 2020
  tot_bef <- 800 + si * 240 + st * (si * 9 - 30)
  serie$man <- round(tot_bef * 0.51)
  serie$kvinnor <- tot_bef - serie$man
  serie$befolkning <- serie$man + serie$kvinnor
  serie$inflyttad <- 40 + si * 8 + st * 3
  serie$utflyttad <- 60 + si * 3 + st * 2
  serie <- serie[order(serie$ar, serie$omrade), ]
  rownames(serie) <- NULL

  # De största pendlingsriktningarna (kommunnamn i demot)
  riktning <- do.call(rbind, lapply(1:3, function(r) data.frame(
    ar = rutnat$ar, omrade = rutnat$omrade, rang = r,
    kommunnamn = c("Falun", "Borlänge", "Avesta")[(rutnat$i + r - 2) %% 3 + 1],
    antal = round((120 - r * 30) + rutnat$i * 4 + rutnat$t * 2), stringsAsFactors = FALSE)))
  riktning$kommunkod <- NA_character_
  riktning <- riktning[order(riktning$ar, riktning$omrade, riktning$rang),
                       c("ar", "omrade", "rang", "kommunkod", "kommunnamn", "antal")]
  rownames(riktning) <- NULL

  # Pendling: bred tabell per kön, som i databasen (Totalt är summan av Kvinnor och Män)
  pend_kon <- function(andel, kon) data.frame(
    ar = rutnat$ar, omrade = rutnat$omrade, kon = kon,
    borarbetar = round((400 + rutnat$i * 20 + rutnat$t * 4) * andel),
    utpendling = round((90 + rutnat$i * 5 + rutnat$t * 4) * andel),
    inpendling = round((60 + rutnat$i * 6 + rutnat$t * 4) * andel),
    stringsAsFactors = FALSE)
  pendling <- rbind(pend_kon(0.52, "Män"), pend_kon(0.48, "Kvinnor"))
  pendling$dagbef <- pendling$borarbetar + pendling$inpendling
  pendling$nattbef <- pendling$borarbetar + pendling$utpendling
  pendling$uppgift_arb_saknas <- round(pendling$nattbef * 0.1)
  tot <- aggregate(cbind(borarbetar, utpendling, inpendling, dagbef, nattbef, uppgift_arb_saknas) ~ ar + omrade,
                   data = pendling, FUN = sum)
  pendling <- rbind(pendling, cbind(tot[c("ar", "omrade")], kon = "Totalt",
                                    tot[setdiff(names(tot), c("ar", "omrade"))])[names(pendling)])
  pendling <- pendling[order(pendling$ar, pendling$omrade, match(pendling$kon, c("Totalt", "Män", "Kvinnor"))), ]
  rownames(pendling) <- NULL


  # Personbilar per drivmedel: elbilar och laddhybrider ökar över tiden
  bil_tot <- 500 + rutnat$i * 120 + rutnat$t * 10
  andelar <- cbind(bensin = 0.60 - 0.04 * rutnat$t, diesel = 0.30 - 0.035 * rutnat$t,
                   etanol = 0.005, gas = 0.003, elhybrid = 0.02 + 0.01 * rutnat$t,
                   laddhybrid = 0.015 + 0.012 * rutnat$t + 0.002 * rutnat$i,
                   el = 0.01 + 0.015 * rutnat$t + 0.003 * rutnat$i)
  antal_bil <- round(bil_tot * andelar)
  bilar_distrikt <- data.frame(ar = rutnat$ar, omrade = rutnat$omrade, antal_bil,
                               stringsAsFactors = FALSE)
  bilar_distrikt$ovriga <- pmax(bil_tot - rowSums(antal_bil), 0)
  names(bilar_distrikt)[3:10] <- paste0("personbilar_", c(colnames(andelar), "ovriga"))
  bilar_distrikt <- bilar_distrikt[order(bilar_distrikt$ar, bilar_distrikt$omrade), ]
  rownames(bilar_distrikt) <- NULL
  # Kommuner (två i demot) med län och riket som jämförelse
  bk <- bilar_distrikt
  bk$kommun <- kommun_for_distrikt(bk$omrade)
  kol_bil <- grep("^personbilar_", names(bk), value = TRUE)
  bilar_kommun <- aggregate(bk[kol_bil], by = list(ar = bk$ar, kommun = bk$kommun), FUN = sum)
  lan <- aggregate(bk[kol_bil], by = list(ar = bk$ar), FUN = sum)
  lan$kommun <- "Dalarnas län"
  riket <- lan
  riket$kommun <- "Riket"
  riket[kol_bil] <- round(riket[kol_bil] * 40)
  bilar_kommun <- rbind(bilar_kommun, lan[names(bilar_kommun)], riket[names(bilar_kommun)])
  bilar_kommun <- bilar_kommun[order(bilar_kommun$ar, bilar_kommun$kommun), ]
  rownames(bilar_kommun) <- NULL

  list(inkomst = inkomst, flytt_alder = flytt_alder, hushall_inkomst = hushall, ekonomisk_standard = ek,
       sysselsattning_alder = syss_alder, sysselsattning_kon = syss_kon, pendling = pendling,
       flytt_serie_ar = serie, riktning_storst_pendling = riktning,
       bilar_distrikt = bilar_distrikt, bilar_kommun = bilar_kommun,
       tabellfel = character())
}

APP_TITEL <- "Lokala ekonomier"
geodata_referens <- list(
  databas = "geodata", schema = "karta",
  distrikt = "distrikt", kommun = "kommun_scb"
)

skapa_exempeldata <- function() {
  omraden <- data.frame(
    omrade = sprintf("demo_%02d", 1:8),
    namn = paste("Exempeldistrikt", LETTERS[1:8])
  )
  omraden$kommunkod <- kommun_for_distrikt(omraden$omrade)
  flytt <- expand.grid(
    ar = 2020:2024, omrade = omraden$omrade,
    stringsAsFactors = FALSE
  )
  i <- match(flytt$omrade, omraden$omrade)
  t <- flytt$ar - 2020
  flytt$befolkning <- 800 + i * 240 + t * (i * 9 - 30)
  flytt$inflyttad <- 40 + i * 8 + t * 3
  flytt$utflyttad <- 60 + i * 3 + t * 2
  flytt$flyttnetto <- flytt$inflyttad - flytt$utflyttad

  utbildning <- expand.grid(
    ar = 2020:2024, omrade = omraden$omrade,
    kon = c("Kvinnor", "Män", "Totalt"), stringsAsFactors = FALSE
  )
  i <- match(utbildning$omrade, omraden$omrade)
  t <- utbildning$ar - 2020
  multiplikator <- ifelse(utbildning$kon == "Totalt", 2, 1)
  utbildning$antal_forgymnasial <- (40 + i * 3 - t) * multiplikator
  utbildning$antal_gymnasial <- (130 + i * 9 + t) * multiplikator
  utbildning$antal_eftergymnasial <- (70 + i * 12 + t * 4) * multiplikator
  utbildning$antal_okand <- (5 + i) * multiplikator
  utbildning$totalt <- rowSums(utbildning[c(
    "antal_forgymnasial", "antal_gymnasial",
    "antal_eftergymnasial", "antal_okand"
  )])
  c(
    list(
      omraden = omraden, flytt_summering = flytt, utbildning = utbildning,
      distrikt = NULL, kommun = NULL, exempeldata = TRUE,
      kalla = "Syntetiska exempeldata – inte statistik om Dalarna"
    ),
    skapa_exempeltabeller(omraden)
  )
}

validera_appdata <- function(data) {
  kontrakt <- list(
    omraden = c("omrade", "namn"),
    flytt_summering = c(
      "ar", "omrade", "befolkning", "inflyttad", "utflyttad", "flyttnetto"
    ),
    utbildning = c(
      "ar", "omrade", "kon", "totalt", "antal_forgymnasial",
      "antal_gymnasial", "antal_eftergymnasial", "antal_okand"
    )
  )
  nycklar <- list(
    omraden = "omrade", flytt_summering = c("ar", "omrade"),
    utbildning = c("ar", "omrade", "kon")
  )
  for (tabell in names(kontrakt)) {
    df <- data[[tabell]]
    if (!is.data.frame(df) || !all(kontrakt[[tabell]] %in% names(df))) {
      stop(paste("Saknade tabeller eller kolumner:", tabell), call. = FALSE)
    }
    if (!is.character(df$omrade) || anyNA(df[nycklar[[tabell]]]) ||
        any(!nzchar(df$omrade)) || anyDuplicated(df[nycklar[[tabell]]])) {
      stop(paste("Ogiltiga eller dubbla nycklar:", tabell), call. = FALSE)
    }
    if (tabell != "omraden") {
      matt <- setdiff(kontrakt[[tabell]], c("omrade", "kon"))
      if (!all(vapply(df[matt], is.numeric, logical(1))) ||
          any(!is.finite(df$ar)) || any(df$ar != trunc(df$ar)) ||
          any(!df$omrade %in% data$omraden$omrade)) {
        stop(paste("Ogiltiga år, mått eller områdesnycklar:", tabell), call. = FALSE)
      }
      varden <- unlist(df[setdiff(matt, "ar")], use.names = FALSE)
      if (any(!is.finite(varden) & !is.na(varden))) {
        stop(paste("Oändliga mått:", tabell), call. = FALSE)
      }
      antal <- unlist(df[setdiff(matt, c("ar", "flyttnetto"))], use.names = FALSE)
      if (any(antal < 0, na.rm = TRUE)) {
        stop(paste("Negativa antal:", tabell), call. = FALSE)
      }
    }
  }
  if (!is.character(data$omraden$namn) || anyNA(data$omraden$namn) ||
      any(!nzchar(data$omraden$namn)) ||
      !all(data$utbildning$kon %in% c("Kvinnor", "Män", "Totalt")) ||
      !is.logical(data$exempeldata) || length(data$exempeldata) != 1 ||
      is.na(data$exempeldata) ||
      !is.character(data$kalla) || length(data$kalla) != 1 ||
      is.na(data$kalla) || !nzchar(data$kalla)) {
    stop("Ogiltiga namn, könsvärden eller källmetadata.", call. = FALSE)
  }
  for (lager in c("distrikt", "kommun")) {
    geo <- data[[lager]]
    if (is.null(geo)) next
    if (!requireNamespace("sf", quietly = TRUE) || !inherits(geo, "sf") ||
        is.na(sf::st_crs(geo)) ||
        any(!as.character(sf::st_geometry_type(geo)) %in% c("POLYGON", "MULTIPOLYGON")) ||
        any(sf::st_is_empty(geo)) || !all(sf::st_is_valid(geo))) {
      stop(paste("Ogiltig polygongeometri eller saknat CRS:", lager), call. = FALSE)
    }
    if (lager == "distrikt" &&
        (!is.character(geo$omrade) || anyNA(geo$omrade) ||
         anyDuplicated(geo$omrade) ||
         !setequal(geo$omrade, data$omraden$omrade))) {
      stop("Distriktskartan måste ha en unik omrade-nyckel per område.", call. = FALSE)
    }
  }
  # Inkomst, hushållsinkomst, ekonomisk standard och sysselsättning (valfria tabeller)
  validera_tabeller(data)
  data
}

# Datakälla i prioritetsordning:
#  1. options(lokala_ekonomier.data = <lista>) – färdigt objekt (t.ex. vid test)
#  2. Postgres (schema ale) via rdpaket, om options(lokala_ekonomier.databas = TRUE)
#  3. Syntetiska exempeldata (fallback, även om databashämtningen misslyckas)
# Tabellerna i databasen ska redan vara röjandegranskade.

#============================================================================
# DEL 2: Hämtning ur Postgres (schema ale)
#============================================================================
## Hämtar färdiga, röjandegranskade tabeller ur Postgres (schema `ale`) och
## mappar dem till appens datakontrakt. Läser endast; skriver aldrig.
## Anslutning sker med rdshinyappar::shiny_uppkoppling_las() (användare shiny_las,
## lösenord via rdshinyappar::shiny_get_password()).

ale_inst <- list(
  db_name_sek = "sekretess",                      # databasen där schemat `ale` ligger (tabellerna)
  db_user_sek = "shiny_las_sekretess",            # KONTROLLERA: läsanvändare för sekretess
  db_name_geo = "geodata",                        # databasen med kartor (schema karta)
  schema = "ale",
  # Distriktskoder som ska finnas med trots att deras geometri saknas i länets distrikt, t.ex. ett
  # distrikt vars kod finns i två län i karta.distrikt (214046 ligger där bara under länskod 18).
  # De får data men ingen polygon i kartorna (geometrin från ett annat län ska inte ritas).
  extra_distrikt = c("214046"),
  tabell_flytt = "flytt_sum",
  tabell_utbildning = "utbildning",
  tabeller = list(                      # tabellnamn i schemat ale
    inkomst = "inkomst", flytt_alder = "flytt_alder", hushall_inkomst = "hushall_inkomst",
    ekonomisk_standard = "ekonomisk_standard",
    sysselsattning_alder = "sysselsattning_alder",
    sysselsattning_kon = "sysselsattning_kon",
    pendling = "pendling",
    riktning_storst_pendling = "riktning_storst_pendling",
    flytt_serie_ar = "flytt_serie_ar",
    bilar_distrikt = "bilar_distrikt",
    bilar_kommun = "bilar_kommun"
  ),
  lanskod = "20"
)

intern_kol <- function(df, monster, tabell) {
  traff <- grep(monster, names(df), value = TRUE)
  if (length(traff) < 1) {
    stop(sprintf("Hittar ingen kolumn som matchar '%s' i %s. Kolumner: %s",
                 monster, tabell, paste(names(df), collapse = ", ")), call. = FALSE)
  }
  traff[1]
}

# Kommungeometrin behåller kommunkod (fyra siffror) och kommunnamn när kolumnerna kan identifieras,
# så att pendlingsriktningar kan kopplas till kommuner på kartan.
forbered_kommungeometri <- function(geo) {
  nm <- names(geo)
  kod_kol <- grep("^(kommunkod|kommun_kod|kommunkod_tx|knkod|kn_kod|kommunid|kommun_id|kod)$", nm,
                  ignore.case = TRUE, value = TRUE)[1]
  if (is.na(kod_kol)) {
    # Annars: första textkolumn där alla värden är fyra siffror som börjar på länskoden
    for (k in setdiff(nm, attr(geo, "sf_column"))) {
      v <- as.character(geo[[k]])
      if (all(grepl("^20\\d{2}$", v[!is.na(v)])) && any(!is.na(v))) { kod_kol <- k; break }
    }
  }
  namn_kol <- grep("^(kommunnamn|kommun_namn|knnamn|namn|name|kommun)$", setdiff(nm, kod_kol),
                   ignore.case = TRUE, value = TRUE)[1]
  message("Kommungeometri: kod = ", if (is.na(kod_kol)) "saknas" else kod_kol,
          ", namn = ", if (is.na(namn_kol)) "saknas" else namn_kol)
  ut <- sf::st_sf(
    kommunkod = if (is.na(kod_kol)) NA_character_ else sprintf("%04d", suppressWarnings(as.integer(as.character(geo[[kod_kol]])))),
    kommunnamn = if (is.na(namn_kol)) NA_character_ else as.character(geo[[namn_kol]]),
    geometry = sf::st_geometry(geo))
  sf::st_make_valid(ut)
}

hamta_appdata_db <- function(inst = ale_inst) {
  # Två uppkopplingar: sekretess för tabellerna i schemat ale, geodata för kartor.
  con_sek <- rdshinyappar::shiny_uppkoppling_las(db_name = inst$db_name_sek,
                                                 db_user = inst$db_user_sek)
  if (is.null(con_sek)) stop("Kunde inte ansluta till sekretess-databasen.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con_sek), add = TRUE)
  # Kartuppkopplingen öppnas först när tabellerna är lästa; misslyckas den visas appen utan karta.
  con_geo <- NULL
  on.exit(if (!is.null(con_geo)) DBI::dbDisconnect(con_geo), add = TRUE)

  las <- function(tabell) {
    df <- rdpostgres::postgres_tabell_till_df(con_sek, inst$schema, tabell)
    if (is.null(df)) stop(paste("Kunde inte läsa", tabell), call. = FALSE)
    names(df) <- tolower(names(df))
    df
  }

  # --- Flytt och befolkning --------------------------------------------------
  f <- las(inst$tabell_flytt)
  flytt <- data.frame(
    ar = as.integer(f[[intern_kol(f, "^ar$", inst$tabell_flytt)]]),
    omrade = as.character(f[[intern_kol(f, "^omrade$", inst$tabell_flytt)]]),
    befolkning = as.numeric(f[[intern_kol(f, "^befolkning$", inst$tabell_flytt)]]),
    inflyttad = as.numeric(f[[intern_kol(f, "^inflyttad$", inst$tabell_flytt)]]),
    utflyttad = as.numeric(f[[intern_kol(f, "^utflyttad$", inst$tabell_flytt)]])
  )
  flytt$flyttnetto <- if ("flyttnetto" %in% names(f)) as.numeric(f$flyttnetto) else
    flytt$inflyttad - flytt$utflyttad
  # Ev. könsuppdelade rader ska inte dubblera nyckeln: behåll totalrad.
  if ("kon" %in% names(f)) flytt <- flytt[is.na(f$kon) | normalisera_kon(f$kon) %in% "Totalt", ]

  # --- Utbildning (bred tabell: antal_<nivå> per kön) -------------------------
  u <- las(inst$tabell_utbildning)
  # Könsvärdena i databasen är gemener (man, kvinnor, totalt)
  kon <- normalisera_kon(u[[intern_kol(u, "^kon$", inst$tabell_utbildning)]])
  nivakol <- function(m) intern_kol(u, paste0("^antal_(.*_)?(", m, ")"), inst$tabell_utbildning)
  utb <- data.frame(
    ar = as.integer(u[[intern_kol(u, "^ar$", inst$tabell_utbildning)]]),
    omrade = as.character(u[[intern_kol(u, "^omrade$", inst$tabell_utbildning)]]),
    kon = kon,
    antal_forgymnasial = as.numeric(u[[nivakol("forgymn")]]),
    antal_gymnasial = as.numeric(u[[nivakol("gymn")]]),
    antal_eftergymnasial = as.numeric(u[[nivakol("efter")]]),
    antal_okand = as.numeric(u[[nivakol("okand|saknas|uppgift")]])
  )
  utb$totalt <- if ("totalt" %in% names(u)) as.numeric(u$totalt) else
    rowSums(utb[c("antal_forgymnasial", "antal_gymnasial",
                  "antal_eftergymnasial", "antal_okand")], na.rm = TRUE)
  utb <- utb[!is.na(utb$kon), ]    # rader med okänt kön ingår inte

  # --- Inkomst, hushållsinkomst, ekonomisk standard och sysselsättning --------
  tab <- hamta_fem_tabeller(las, inst)

  # --- Geometri (geodata-databasen) --------------------------------------------
  con_geo <- rdshinyappar::shiny_uppkoppling_las(db_name = inst$db_name_geo)
  if (is.null(con_geo)) message("Kunde inte ansluta till geodata – kartan visas inte.")
  geo_distrikt <- if (is.null(con_geo)) NULL else tryCatch(
    rdgis::postgis_postgistabell_till_sf(con_geo, "karta", "distrikt",
                                         query = paste0("WHERE lankod = '", inst$lanskod, "'")),
    error = function(e) { message("Distriktsgeometri: ", conditionMessage(e)); NULL })
  geo_kommun <- if (is.null(con_geo)) NULL else tryCatch(
    rdgis::postgis_postgistabell_till_sf(con_geo, "karta", "kommun_scb",
                                         query = paste0("WHERE lanskod_tx = '", inst$lanskod, "'")),
    error = function(e) { message("Kommungeometri: ", conditionMessage(e)); NULL })

  # Distriktslistan byggs av de tabeller som definierar distrikten. bilar_distrikt ingår inte:
  # den kan innehålla koder utanför Dalarna (exklaver som ligger under en annan kommun), och
  # sådana rader ignoreras nedan i stället för att ge distrikt utan geometri.
  koder <- sort(unique(c(flytt$omrade, utb$omrade,
                         unlist(lapply(tab$tabeller[setdiff(names(tab$tabeller), "bilar_distrikt")],
                                       function(d) d$omrade), use.names = FALSE))))
  extra <- character(0)
  if (!is.null(tab$tabeller$bilar_distrikt)) {
    bd <- tab$tabeller$bilar_distrikt
    # Ett distrikt som finns i länets geometri men saknas i de andra tabellerna (t.ex. för att det
    # saknar rader där) ska ändå få behålla sina bilrader. Bara koder som varken finns bland
    # distrikten eller i länets geometri räknas som "utanför".
    geo_koder <- if (is.null(geo_distrikt)) character(0) else as.character(geo_distrikt$distriktskod)
    nya <- setdiff(intersect(unique(bd$omrade), geo_koder), c(koder, "(alla)"))
    if (length(nya)) {
      message("bilar_distrikt: ", length(nya), " distrikt finns i geometrin men saknas i övriga tabeller ",
              "och läggs till (t.ex. ", paste(head(nya, 3), collapse = ", "), ").")
      koder <- sort(c(koder, nya))
    }
    extra <- intersect(as.character(inst$extra_distrikt), unique(as.character(bd$omrade)))
    extra <- setdiff(extra, c(koder, "(alla)"))
    if (length(extra)) {
      message("bilar_distrikt: distrikt ", paste(extra, collapse = ", "),
              " läggs till utan geometri (inst$extra_distrikt).")
      koder <- sort(c(koder, extra))
    }
    utanfor <- !bd$omrade %in% koder
    if (any(utanfor) && !is.null(con_geo)) {
      # Felsökning: finns de utelämnade koderna alls i distriktstabellen, och vilket län har de då?
      sok <- unique(as.character(bd$omrade[utanfor]))
      fyll <- tryCatch(
        rdgis::postgis_postgistabell_till_sf(con_geo, "karta", "distrikt",
                                             query = paste0("WHERE distriktskod IN ('", paste(sok, collapse = "','"), "')")),
        error = function(e) NULL)
      if (is.null(fyll) || nrow(fyll) == 0) {
        message("Felsökning: ingen av koderna ", paste(head(sok, 5), collapse = ", "),
                " finns i karta.distrikt (kolumnen distriktskod).")
      } else {
        lk <- intersect(c("lankod", "lankod_tx", "lan_kod"), names(fyll))
        message("Felsökning: ", nrow(fyll), " av koderna finns i karta.distrikt med ",
                if (length(lk)) paste0(lk[1], " = ", paste(unique(fyll[[lk[1]]]), collapse = "/"), " (appen filtrerar på ", inst$lanskod, ")")
                else "okänd länskolumn", ".")
      }
    }
    if (any(utanfor)) {
      message("bilar_distrikt: ", sum(utanfor), " rader med ", length(unique(bd$omrade[utanfor])),
              " distriktskoder som saknas bland distrikten utelämnas (t.ex. ",
              paste(head(unique(bd$omrade[utanfor]), 3), collapse = ", "), ").")
      tab$tabeller$bilar_distrikt <- bd[!utanfor, , drop = FALSE]
    }
  }
  # "(alla)" är en markör för tabeller utan distriktskolumn (riktning_storst_pendling), inget distrikt
  koder <- setdiff(koder, "(alla)")
  namn <- koder
  kommunkod <- rep(NA_character_, length(koder))
  if (!is.null(geo_distrikt)) {
    geo_distrikt$omrade <- as.character(geo_distrikt$distriktskod)
    # Kommunkod per distrikt: ur en kolumn i distriktsgeometrin om det finns en (före beskärningen nedan)
    kk <- grep("^(kommunkod|kommun_kod|kommunkod_tx|knkod|kn_kod|kommunid|kommun_id)$", names(geo_distrikt),
               ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(kk)) {
      kv <- sprintf("%04d", suppressWarnings(as.integer(as.character(geo_distrikt[[kk]]))))
      kommunkod <- kv[match(koder, geo_distrikt$omrade)]
    }
    nk <- grep("^(distriktsnamn|distrikt_namn|namn|name)$", names(geo_distrikt),
               ignore.case = TRUE, value = TRUE)
    if (length(nk)) namn <- geo_distrikt[[nk[1]]][match(koder, geo_distrikt$omrade)]
    namn <- ifelse(is.na(namn), paste("Distrikt", koder), as.character(namn))
    geo_distrikt <- sf::st_make_valid(geo_distrikt[geo_distrikt$omrade %in% koder, "omrade"])
    if (!setequal(geo_distrikt$omrade, setdiff(koder, as.character(inst$extra_distrikt)))) {
      warning("Geometri saknas för vissa distriktskoder – kartan visas inte.")
      geo_distrikt <- NULL
    }
  } else namn <- paste("Distrikt", koder)
  if (!is.null(geo_kommun)) geo_kommun <- forbered_kommungeometri(geo_kommun)
  # Saknas kommunkod i distriktsgeometrin: placera en punkt i varje distrikt och se vilken kommun den ligger i
  if (!is.null(geo_distrikt) && !is.null(geo_kommun) && anyNA(kommunkod) && !all(is.na(geo_kommun$kommunkod))) {
    kommunkod_geo <- tryCatch({
      pkt <- suppressWarnings(sf::st_point_on_surface(sf::st_transform(geo_distrikt, sf::st_crs(geo_kommun))))
      j <- sf::st_join(pkt, geo_kommun["kommunkod"], join = sf::st_within)
      j <- j[!duplicated(j$omrade), ]
      j$kommunkod[match(koder, j$omrade)]
    }, error = function(e) { message("Kommunkoppling för distrikt: ", conditionMessage(e)); rep(NA_character_, length(koder)) })
    kommunkod[is.na(kommunkod)] <- kommunkod_geo[is.na(kommunkod)]
  }
  message("Kommunkod hittades för ", sum(!is.na(kommunkod)), " av ", length(koder), " distrikt.")

  c(
    list(
      omraden = data.frame(omrade = koder, namn = namn, kommunkod = kommunkod, stringsAsFactors = FALSE),
      flytt_summering = flytt, utbildning = utb,
      distrikt = geo_distrikt, kommun = geo_kommun, exempeldata = FALSE,
      kalla = "Region Dalarna, Samhällsanalys (databasschema ale)",
      tabellfel = tab$fel
    ),
    tab$tabeller
  )
}

#============================================================================
# DEL 3: Val av datakälla och globala objekt
#============================================================================
appdata_kalla <- getOption("lokala_ekonomier.data", default = NULL)
if (is.null(appdata_kalla) && isTRUE(getOption("lokala_ekonomier.databas", TRUE))) {
  appdata_kalla <- tryCatch(
    validera_appdata(hamta_appdata_db()),
    error = function(e) {
      message("Databashämtning misslyckades, använder exempeldata: ", conditionMessage(e))
      NULL
    }
  )
}
if (is.null(appdata_kalla)) appdata_kalla <- skapa_exempeldata()
appdata <- validera_appdata(appdata_kalla)
# Jämförelsemått mellan distrikt. Tabellen anger varifrån måttet hämtas.
indikatorer <- list(
  "Flytt och befolkning" = c("Befolkning" = "befolkning", "Inflyttade" = "inflyttad",
                             "Utflyttade" = "utflyttad", "Flyttnetto" = "flyttnetto"),
  "Hushållens inkomst" = c("Antal hushåll" = "antal_hus", "Medelinkomst, hushåll" = "medel_ink",
                           "Medianinkomst, hushåll" = "median_ink")
)
indikatorkalla <- c(befolkning = "flytt_summering", inflyttad = "flytt_summering",
                    utflyttad = "flytt_summering", flyttnetto = "flytt_summering",
                    antal_hus = "hushall_inkomst", medel_ink = "hushall_inkomst",
                    median_ink = "hushall_inkomst")
indikatornamn <- unlist(indikatorer, use.names = TRUE)
names(indikatornamn) <- sub("^[^.]*\\.", "", names(indikatornamn))
omradesval <- setNames(
  appdata$omraden$omrade,
  paste0(appdata$omraden$namn, " (", appdata$omraden$omrade, ")")
)
arval <- sort(unique(c(
  appdata$flytt_summering$ar, appdata$utbildning$ar,
  # bilar_distrikt styr inte årsvalet: ett senare bilår ska inte bli förvalt år för övriga flikar
  unlist(lapply(setdiff(names(tabellkontrakt), c("flytt_serie_ar", "bilar_distrikt")),
                function(n) appdata[[n]]$ar), use.names = FALSE)
)))

# Val och ordning för de nya tabellerna
inkomstmatt <- c("Medelinkomst" = "medel_ink", "Medianinkomst" = "median_ink")
blockval <- c("Näringsgren" = "naringsgren", "Sektor" = "sektor", "Totalt" = "totalt")
# Näringsliv visar bara näringsgren och sektor; totalen hör till Arbetsmarknad
blockval_syss <- c("Näringsgren" = "naringsgren", "Sektor" = "sektor")
# Val av totalt eller uppdelat på kön
visaval <- c("Totalt", "Uppdelat på kön")
aldersgruppsordning <- if (is.null(appdata$inkomst)) character() else
  sortera_naturligt(appdata$inkomst$aldersgrupp)
# Åldersgrupperna i sysselsattning_alder är kolumner. Kolumnen totalt (om den finns)
# summeras inte med åldersgrupperna.
alderskolumner <- if (is.null(appdata$sysselsattning_alder)) character() else
  sortera_naturligt(setdiff(names(appdata$sysselsattning_alder),
                            tabellkontrakt$sysselsattning_alder$nyckel))
summakolumner <- setdiff(alderskolumner, "totalt")
flytt_alder_ordning <- if (is.null(appdata$flytt_alder)) character() else
  sortera_naturligt(appdata$flytt_alder$aldersgrupp)
utbildningsnivaer <- c(
  antal_forgymnasial = "Förgymnasial", antal_gymnasial = "Gymnasial",
  antal_eftergymnasial = "Eftergymnasial", antal_okand = "Uppgift saknas"
)

skapa_diagram <- function(data, x, y, titel, linje = FALSE) {
  # Paketets summering får inte omvandla saknade observationer till noll.
  if (requireNamespace("rddiagram", quietly = TRUE) &&
      all(is.finite(data[[y]]))) {
    funktion <- if (linje) rddiagram::SkapaLinjeDiagram else rddiagram::SkapaStapelDiagram
    return(funktion(
      skickad_df = data, skickad_x_var = x, skickad_y_var = y,
      output_mapp = tempdir(), filnamn_diagram = "lokala_ekonomier",
      diagram_titel = titel, diagram_capt = appdata$kalla,
      manual_x_axis_title = NULL, manual_y_axis_title = "Antal",
      manual_color = rd_farg$primar, lagg_pa_logga = FALSE,
      skriv_till_diagramfil = FALSE
    ))
  }
  p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]]))
  p <- if (linje) p + geom_line(aes(group = 1), colour = rd_farg$primar) +
    geom_point(colour = rd_farg$primar) else p + geom_col(fill = rd_farg$primar)
  p + theme_minimal(base_size = 12) +
    labs(title = titel, x = NULL, y = "Antal", caption = appdata$kalla)
}
