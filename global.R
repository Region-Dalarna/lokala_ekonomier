## Globala inställningar för Shinyappen: lokala_ekonomier

# Ladda nödvändiga paket
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ggiraph)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(rdshinyappar)

options(shiny.sanitize.errors = TRUE)

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
  list(
    omraden = omraden, flytt_summering = flytt, utbildning = utbildning,
    distrikt = NULL, kommun = NULL, exempeldata = TRUE,
    kalla = "Syntetiska exempeldata – inte statistik om Dalarna"
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
  data
}

# Inmatade objekt ska redan vara sekretessgranskade och anpassade till kontraktet.
# Ingen anslutning eller filinläsning sker här.
appdata <- validera_appdata(getOption(
  "lokala_ekonomier.data", default = skapa_exempeldata()
))
indikatorer <- c(
  "Befolkning" = "befolkning", "Inflyttade" = "inflyttad",
  "Utflyttade" = "utflyttad", "Flyttnetto" = "flyttnetto"
)
omradesval <- setNames(
  appdata$omraden$omrade,
  paste0(appdata$omraden$namn, " (", appdata$omraden$omrade, ")")
)
arval <- sort(unique(c(appdata$flytt_summering$ar, appdata$utbildning$ar)))
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
