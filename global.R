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

tema_app <- function() ggplot2::theme_minimal(base_size = 12)

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
  if (!is.null(giltiga_omraden) && any(!df$omrade %in% giltiga_omraden)) {
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
normalisera_tabell <- function(df, namn) {
  k <- tabellkontrakt[[namn]]
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  if (namn == "flytt_alder") df <- forbered_flytt_alder(df)
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
  df <- df[c(k$nyckel, matt)]
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
  list(tabeller = tabeller, fel = fel)
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

  list(inkomst = inkomst, flytt_alder = flytt_alder, hushall_inkomst = hushall, ekonomisk_standard = ek,
       sysselsattning_alder = syss_alder, sysselsattning_kon = syss_kon,
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
  tabell_flytt = "flytt_sum",
  tabell_utbildning = "utbildning",
  tabeller = list(                      # tabellnamn i schemat ale
    inkomst = "inkomst", flytt_alder = "flytt_alder", hushall_inkomst = "hushall_inkomst",
    ekonomisk_standard = "ekonomisk_standard",
    sysselsattning_alder = "sysselsattning_alder",
    sysselsattning_kon = "sysselsattning_kon"
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

  koder <- sort(unique(c(flytt$omrade, utb$omrade,
                         unlist(lapply(tab$tabeller, function(d) d$omrade), use.names = FALSE))))
  namn <- koder
  if (!is.null(geo_distrikt)) {
    geo_distrikt$omrade <- as.character(geo_distrikt$distriktskod)
    nk <- grep("^(distriktsnamn|distrikt_namn|namn|name)$", names(geo_distrikt),
               ignore.case = TRUE, value = TRUE)
    if (length(nk)) namn <- geo_distrikt[[nk[1]]][match(koder, geo_distrikt$omrade)]
    namn <- ifelse(is.na(namn), paste("Distrikt", koder), as.character(namn))
    geo_distrikt <- sf::st_make_valid(geo_distrikt[geo_distrikt$omrade %in% koder, "omrade"])
    if (!setequal(geo_distrikt$omrade, koder)) {
      warning("Geometri saknas för vissa distriktskoder – kartan visas inte.")
      geo_distrikt <- NULL
    }
  } else namn <- paste("Distrikt", koder)
  if (!is.null(geo_kommun)) geo_kommun <- sf::st_make_valid(sf::st_geometry(geo_kommun) |> sf::st_sf())

  c(
    list(
      omraden = data.frame(omrade = koder, namn = namn, stringsAsFactors = FALSE),
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
  unlist(lapply(names(tabellkontrakt), function(n) appdata[[n]]$ar), use.names = FALSE)
)))

# Val och ordning för de nya tabellerna
inkomstmatt <- c("Medelinkomst" = "medel_ink", "Medianinkomst" = "median_ink")
blockval <- c("Näringsgren" = "naringsgren", "Sektor" = "sektor", "Totalt" = "totalt")
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
      manual_color = "#178571", lagg_pa_logga = FALSE,
      skriv_till_diagramfil = FALSE
    ))
  }
  p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]]))
  p <- if (linje) p + geom_line(aes(group = 1), colour = "#178571") +
    geom_point(colour = "#178571") else p + geom_col(fill = "#178571")
  p + theme_minimal(base_size = 12) +
    labs(title = titel, x = NULL, y = "Antal", caption = appdata$kalla)
}
