## Hämtar färdiga, röjandegranskade tabeller ur Postgres (schema `ale`) och
## mappar dem till appens datakontrakt. Läser endast; skriver aldrig.
## Anslutning sker med rdshinyappar::shiny_uppkoppling_las() (användare shiny_las,
## lösenord via rdshinyappar::shiny_get_password()).

ale_inst <- list(
  db_name = "sekretess",   # KONTROLLERA: databasen där schemat `ale` ligger
  db_user = "shiny_las_sekretess",
  schema = "ale",
  tabell_flytt = "flytt_sum",
  tabell_utbildning = "utbildning",
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
  con <- rdshinyappar::shiny_uppkoppling_las(db_name = inst$db_name, db_user = inst$db_name)
  if (is.null(con)) stop("Kunde inte ansluta till databasen.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  con_geo <- shiny_uppkoppling_las("geodata")
  if (is.null(con_geo)) stop("Kunde inte ansluta till databasen.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  las <- function(tabell) {
    df <- rdpostgres::postgres_tabell_till_df(con, inst$schema, tabell)
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
  if ("kon" %in% names(f)) flytt <- flytt[f$kon %in% c("Totalt", NA), ]

  # --- Utbildning (bred tabell: antal_<nivå> per kön) -------------------------
  u <- las(inst$tabell_utbildning)
  kon <- as.character(u[[intern_kol(u, "^kon$", inst$tabell_utbildning)]])
  kon[kon %in% c("Man", "Män", "man", "män")] <- "Män"
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

  # --- Geometri ---------------------------------------------------------------
  geo_distrikt <- tryCatch(
    rdgis::postgis_postgistabell_till_sf(con_geo, "karta", "distrikt",
                                         query = paste0("WHERE lankod = '", inst$lanskod, "'")),
    error = function(e) { message("Distriktsgeometri: ", conditionMessage(e)); NULL })
  geo_kommun <- tryCatch(
    rdgis::postgis_postgistabell_till_sf(con_geo, "karta", "kommun_scb",
                                         query = paste0("WHERE lanskod_tx = '", inst$lanskod, "'")),
    error = function(e) { message("Kommungeometri: ", conditionMessage(e)); NULL })

  koder <- sort(unique(c(flytt$omrade, utb$omrade)))
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

  list(
    omraden = data.frame(omrade = koder, namn = namn, stringsAsFactors = FALSE),
    flytt_summering = flytt, utbildning = utb,
    distrikt = geo_distrikt, kommun = geo_kommun, exempeldata = FALSE,
    kalla = "Region Dalarna, Samhällsanalys (databasschema ale)"
  )
}
