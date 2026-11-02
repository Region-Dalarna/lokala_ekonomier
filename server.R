shinyServer(function(input, output, session) {
  valt_omrade <- reactive({
    req(input$omrade %in% appdata$omraden$omrade)
    input$omrade
  })
  valt_ar <- reactive({
    req(input$ar %in% as.character(arval))
    as.numeric(input$ar)
  })
  utbildningsurval <- reactive({
    req(input$kon_utb %in% c("Totalt", "Kvinnor", "Män"))
    appdata$utbildning |>
      filter(omrade == valt_omrade(), ar == valt_ar(), kon == input$kon_utb)
  })
  jamforelsedata <- reactive({
    req(input$indikator %in% unname(indikatornamn))
    tabell <- indikatorkalla[[input$indikator]]
    req(!is.null(appdata[[tabell]]))
    a <- valt_ar()
    appdata[[tabell]] |>
      filter(ar == a) |>
      transmute(omrade, varde = .data[[input$indikator]]) |>
      right_join(appdata$omraden, by = "omrade") |>
      mutate(valt = omrade == valt_omrade()) |>
      arrange(desc(varde), omrade)
  })
  # ---- Flytt och befolkning (första sidan) -----------------------------------
  flyttserie <- reactive({
    o <- valt_omrade()
    appdata$flytt_summering |> filter(omrade == o) |> arrange(ar)
  })
  # Flyttnetto per åldersgrupp för valt distrikt (alla kön och år)
  flytt_alder_serie <- reactive({
    req(!is.null(appdata$flytt_alder))
    o <- valt_omrade()
    appdata$flytt_alder |> filter(omrade == o)
  })
  # Totalt beräknas som summa av män och kvinnor om totalrader saknas.
  flytt_alder_med_totalt <- reactive({
    serie <- flytt_alder_serie()
    if (any(serie$kon == "Totalt")) return(serie)
    tot <- serie |>
      group_by(ar, omrade, aldersgrupp) |>
      summarise(flyttnetto = summa_na(flyttnetto), .groups = "drop") |>
      mutate(kon = "Totalt")
    bind_rows(serie, tot)
  })
  # Vald könsdel av flytten
  flytt_alder_kon <- reactive({
    req(input$kon_flytt %in% c("Totalt", "Kvinnor", "Män"))
    d <- flytt_alder_med_totalt()
    d[d$kon == input$kon_flytt, ]
  })
  output$flytt_alder_diagram <- renderPlot({
    serie <- flytt_alder_med_totalt()
    a <- valt_ar()
    df <- flytt_alder_kon()
    df <- df[df$ar == a, ]
    validate(need(nrow(df) > 0 && any(is.finite(df$flyttnetto)),
                  "Flyttuppgift per åldersgrupp saknas för valt distrikt och år."))
    # Fast x-axel: samma gränser för alla kön och år i valt distrikt, så att Män, Kvinnor,
    # Totalt och olika år går att jämföra.
    lim <- range(c(0, serie$flyttnetto), na.rm = TRUE)
    marginal <- max(diff(lim) * 0.15, 1)
    # Yngsta åldersgruppen överst
    df$aldersgrupp <- factor(df$aldersgrupp, levels = rev(flytt_alder_ordning))
    df$positiv <- is.na(df$flyttnetto) | df$flyttnetto >= 0
    totalt <- sum(df$flyttnetto, na.rm = TRUE)
    ggplot(df, aes(x = flyttnetto, y = aldersgrupp)) +
      geom_col(aes(fill = positiv), width = 0.6, na.rm = TRUE) +
      geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
      # Åldersgruppen skrivs vid nollinjen, på motsatt sida om stapeln
      geom_label(aes(x = 0, label = aldersgrupp, hjust = ifelse(positiv, 1.1, -0.1)),
                 fill = "white", alpha = 0.85, label.size = 0, label.padding = unit(0.1, "lines"),
                 size = 3.4) +
      geom_text(aes(label = flyttnetto, hjust = ifelse(positiv, -0.25, 1.25)),
                size = 3.6, na.rm = TRUE) +
      scale_fill_manual(values = c("TRUE" = "#178571", "FALSE" = "#c65d00"), guide = "none") +
      scale_x_continuous(limits = lim + c(-marginal, marginal), labels = siffror) +
      scale_y_discrete(drop = FALSE) +
      tema_app() +
      theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(title = paste("Flyttnetto per åldersgrupp –", input$kon_flytt, a),
           subtitle = paste("Totalt flyttnetto:", siffror(totalt)),
           x = "Flyttnetto (inflyttade minus utflyttade)", y = NULL, caption = appdata$kalla)
  })
  output$flytt_alder_tabell <- renderDT({
    if (is.null(appdata$flytt_alder)) return(visa_tabell(tom_tabell()))
    bred <- flytt_alder_kon() |>
      select(aldersgrupp, ar, flyttnetto) |>
      tidyr::pivot_wider(names_from = ar, values_from = flyttnetto) |>
      arrange(match(aldersgrupp, flytt_alder_ordning))
    arskol <- setdiff(names(bred), "aldersgrupp")
    arskol <- arskol[order(as.numeric(arskol))]
    bred <- bred[c("aldersgrupp", arskol)]
    # Summan över åldersgrupper (åldersgrupperna överlappar inte varandra)
    summarad <- data.frame(aldersgrupp = "Totalt", check.names = FALSE)
    for (k in arskol) summarad[[k]] <- summa_na(bred[[k]])
    bred <- rbind(as.data.frame(bred, check.names = FALSE), summarad)
    names(bred)[1] <- "Åldersgrupp"
    dt <- visa_tabell(bred, heltal = arskol, dom = "t", ordering = FALSE, pageLength = 50)
    dt <- DT::formatStyle(dt, "Åldersgrupp", target = "row",
                          fontWeight = DT::styleEqual("Totalt", "bold"))
    if (as.character(valt_ar()) %in% arskol) {
      dt <- DT::formatStyle(dt, as.character(valt_ar()), backgroundColor = "#d7ebe6")
    }
    dt
  })
  output$flytttabell <- renderDT({
    df <- flyttserie() |>
      transmute(År = ar, Befolkning = befolkning, Inflyttade = inflyttad,
                Utflyttade = utflyttad, Flyttnetto = flyttnetto)
    dt <- visa_tabell(df, heltal = c("Befolkning", "Inflyttade", "Utflyttade", "Flyttnetto"),
                      dom = "t", ordering = FALSE, pageLength = 50)
    # Valt år markeras, och negativt flyttnetto färgas orange.
    dt <- DT::formatStyle(dt, "År", target = "row",
                          backgroundColor = DT::styleEqual(valt_ar(), "#d7ebe6"),
                          fontWeight = DT::styleEqual(valt_ar(), "bold"))
    DT::formatStyle(dt, "Flyttnetto", color = DT::styleInterval(-0.5, c("#c65d00", "inherit")))
  })

  # Fast y-axel: störst värde bland alla kön och år i valt distrikt, så att staplarna
  # går att jämföra mellan Totalt, Kvinnor och Män.
  utbildning_ymax <- reactive({
    o <- valt_omrade()
    u <- appdata$utbildning[appdata$utbildning$omrade == o, names(utbildningsnivaer), drop = FALSE]
    mx <- suppressWarnings(max(unlist(u), na.rm = TRUE))
    if (is.finite(mx)) mx * 1.05 else NA_real_
  })
  output$utbildning <- renderPlot({
    df <- utbildningsurval() |>
      select(all_of(names(utbildningsnivaer))) |>
      pivot_longer(everything(), names_to = "niva", values_to = "antal")
    validate(need(nrow(df) > 0 && any(is.finite(df$antal)),
                  "Utbildningsuppgift saknas för valt urval."))
    df$niva <- factor(df$niva, levels = names(utbildningsnivaer),
                      labels = unname(utbildningsnivaer))
    p <- skapa_diagram(df, "niva", "antal",
                       paste("Utbildningsnivå –", input$kon_utb, valt_ar()))
    if (is.finite(utbildning_ymax())) p <- p + coord_cartesian(ylim = c(0, utbildning_ymax()))
    p
  })
  output$jamforelsestatus <- renderText({
    df <- jamforelsedata()
    if (!any(df$valt & is.finite(df$varde))) {
      return("Valt distrikt saknar värde för detta år och mått. Saknat är inte noll.")
    }
    paste("Valt distrikt:", appdata$omraden$namn[
      match(valt_omrade(), appdata$omraden$omrade)
    ], "–", sum(is.finite(df$varde)), "distrikt har värden.")
  })
  output$jamforelse <- renderPlot({
    df <- jamforelsedata() |> filter(is.finite(varde))
    validate(need(nrow(df) > 0, "Inga jämförelsevärden finns för valt år."))
    req(input$rangordning %in% c("hogst", "lagst"))
    rang <- if (input$rangordning == "hogst") head(df$omrade, 15) else tail(df$omrade, 15)
    df <- df |> filter(omrade %in% rang | valt)
    df$etikett <- factor(
      paste(df$namn, df$omrade, sep = " – "),
      levels = rev(paste(df$namn, df$omrade, sep = " – "))
    )
    ggplot(df, aes(etikett, varde, fill = valt)) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = c("FALSE" = "#178571", "TRUE" = "#c65d00"),
                        guide = "none") +
      theme_minimal(base_size = 12) +
      labs(title = names(indikatornamn)[match(input$indikator, indikatornamn)],
           x = NULL, y = if (input$indikator %in% c("medel_ink", "median_ink")) "Kronor" else "Antal",
           caption = appdata$kalla)
  })
  output$jamforelsetabell <- renderDT({
    df <- jamforelsedata() |>
      transmute(Distriktskod = omrade, Distrikt = namn, Värde = varde,
                `Valt distrikt` = ifelse(valt, "Ja", ""))
    datatable(df, rownames = FALSE, escape = TRUE, selection = "single",
              options = list(pageLength = 15, order = list(), language = list(
                search = "Sök:", emptyTable = "Inga uppgifter", zeroRecords = "Inga träffar"
              )))
  }, server = TRUE)
  observeEvent(input$jamforelsetabell_rows_selected, {
    rad <- input$jamforelsetabell_rows_selected
    df <- jamforelsedata()
    if (length(rad) == 1 && rad >= 1 && rad <= nrow(df)) {
      updateSelectInput(session, "omrade", selected = df$omrade[rad])
    }
  })
  output$utbildningstabell <- renderDT({
    o <- valt_omrade(); a <- valt_ar()
    # Underlaget visar alla könsrader för valt distrikt och år
    datatable(filter(appdata$utbildning, omrade == o, ar == a), rownames = FALSE, escape = TRUE,
              options = list(dom = "t"))
  })

  output$kartstatus <- renderUI({
    if (is.null(appdata$distrikt)) {
      div(class = "alert alert-info",
          "Kartan väntar på distriktsgränser från karta.distrikt.",
          " Inga syntetiska gränser visas.")
    }
  })
  # Kartan: OpenStreetMap som bakgrund, distrikt som klickbara ytor och kommungränser som linjer.
  # Geometrin projiceras en gång till WGS84 (EPSG:4326), som leaflet kräver.
  karta_distrikt <- if (is.null(appdata$distrikt)) NULL else
    sf::st_transform(appdata$distrikt, 4326)
  karta_kommun <- if (is.null(appdata$kommun)) NULL else
    sf::st_transform(appdata$kommun, 4326)
  if (!is.null(karta_distrikt)) {
    karta_distrikt$namn <- appdata$omraden$namn[match(karta_distrikt$omrade, appdata$omraden$omrade)]
  }
  # Lager läggs i egna kartpaneler så att ordningen (distrikt, valt, kommun) alltid är densamma,
  # även när distrikten ritas om vid ändrad opacitet.
  rita_distrikt <- function(karta, opacitet) {
    addPolygons(karta, data = karta_distrikt, layerId = ~omrade, group = "distrikt",
                label = ~paste0(namn, " (", omrade, ")"),
                weight = 0.5, color = "white", opacity = 1,
                fillColor = "#178571", fillOpacity = opacitet,
                highlightOptions = highlightOptions(weight = 1, color = "#464d48",
                                                    fillOpacity = min(1, opacitet + 0.2),
                                                    bringToFront = FALSE),
                options = pathOptions(pane = "pane_distrikt"))
  }
  rita_valt <- function(karta, omrade, opacitet) {
    valt <- karta_distrikt[karta_distrikt$omrade == omrade, ]
    if (nrow(valt) != 1) return(karta)
    addPolygons(karta, data = valt, group = "valt", fillColor = "#c65d00",
                fillOpacity = min(1, opacitet + 0.35), weight = 1.5, color = "#c65d00", opacity = 1,
                label = ~paste0(namn, " (", omrade, ")"),
                options = pathOptions(pane = "pane_valt", interactive = FALSE))
  }
  # Opacitetsreglaget ligger i kartan och byggs i JavaScript. Opacitetsändringen görs direkt i
  # webbläsaren (setStyle på lagren), utan omväg via servern, så att kartan inte flimrar.
  # Rutan "Visa kommungränser" sitter i samma kontroll och skickar input$visa_kommun.
  # Värdet skickas till Shiny först när reglaget släpps (input$karta_opacitet), så att senare
  # omritning av valt distrikt använder rätt opacitet.
  opacitetsreglage_js <- "
    function(el, x) {
      var karta = this;
      var kontroll = L.control({position: 'topleft'});
      kontroll.onAdd = function() {
        var div = L.DomUtil.create('div', 'karta-kontroll');
        div.innerHTML =
          '<div class=\"karta-kontroll__rad\" title=\"Ändra hur mycket färgen täcker kartan\">' +
          '<span class=\"karta-kontroll__ikon\">&#9680;</span>' +
          '<input type=\"range\" class=\"karta-opacitet\" min=\"0\" max=\"1\" step=\"0.05\" value=\"' + START + '\"></div>' +
          '<label class=\"karta-kontroll__rad\"><input type=\"checkbox\" class=\"karta-kommun\"> Visa kommungränser</label>';
        // Hindra att kartan panoreras, zoomas eller klickas när reglaget används
        L.DomEvent.disableClickPropagation(div);
        L.DomEvent.disableScrollPropagation(div);
        ['pointerdown', 'mousedown', 'touchstart', 'dblclick', 'wheel'].forEach(function(h) {
          div.addEventListener(h, function(e) { e.stopPropagation(); });
        });
        var inp = div.querySelector('.karta-opacitet');
        var kommun = div.querySelector('.karta-kommun');
        kommun.addEventListener('change', function() {
          if (window.Shiny) Shiny.setInputValue('visa_kommun', this.checked);
        });
        inp.addEventListener('input', function() {
          var v = parseFloat(this.value);
          [['distrikt', v], ['valt', Math.min(1, v + 0.35)]].forEach(function(g) {
            var grupp = karta.layerManager.getLayerGroup(g[0]);
            if (grupp) grupp.eachLayer(function(l) { l.setStyle({fillOpacity: g[1]}); });
          });
        });
        inp.addEventListener('change', function() {
          if (window.Shiny) Shiny.setInputValue('karta_opacitet', parseFloat(this.value));
        });
        return div;
      };
      kontroll.addTo(karta);
    }"
  rita_kommun <- function(karta) {
    addPolygons(karta, data = karta_kommun, group = "kommun", fill = FALSE,
                weight = 1.2, color = "#464d48", opacity = 0.9,
                options = pathOptions(pane = "pane_kommun", interactive = FALSE))
  }
  output$karta <- renderLeaflet({
    req(!is.null(karta_distrikt))
    # Startläget läses med isolate så att kartan inte ritas om när reglagen ändras;
    # ändringar hanteras av leafletProxy nedan.
    opacitet <- isolate(input$karta_opacitet %||% 0.35)
    ram <- as.list(sf::st_bbox(karta_distrikt))
    k <- leaflet(options = leafletOptions(minZoom = 5)) |>
      addMapPane("pane_distrikt", zIndex = 410) |>
      addMapPane("pane_valt", zIndex = 420) |>
      addMapPane("pane_kommun", zIndex = 430) |>
      addTiles(attribution = paste0("&copy; <a href='https://www.openstreetmap.org/copyright'>",
                                    "OpenStreetMap</a>-bidragsgivare"),
               options = tileOptions(className = "mattad-bakgrund")) |>
      rita_distrikt(opacitet) |>
      rita_valt(isolate(valt_omrade()), opacitet)
    # Kommungränserna ritas inte från början, bara om rutan i kartan kryssas i
    if (!is.null(karta_kommun) && isTRUE(isolate(input$visa_kommun))) k <- rita_kommun(k)
    k <- fitBounds(k, ram$xmin, ram$ymin, ram$xmax, ram$ymax)
    htmlwidgets::onRender(k, sub("START", opacitet, opacitetsreglage_js, fixed = TRUE))
  })
  # Nytt val: bara det valda distriktet ritas om
  observeEvent(valt_omrade(), {
    req(!is.null(karta_distrikt))
    leafletProxy("karta") |> clearGroup("valt") |>
      rita_valt(valt_omrade(), input$karta_opacitet %||% 0.35)
  }, ignoreInit = TRUE)
  # Rita eller ta bort kommungränserna när rutan kryssas i eller ur
  observeEvent(input$visa_kommun, {
    req(!is.null(karta_kommun))
    p <- leafletProxy("karta") |> clearGroup("kommun")
    if (isTRUE(input$visa_kommun)) rita_kommun(p)
  }, ignoreInit = TRUE)
  # Klick på ett distrikt väljer det, som ett klick på en rad i jämförelsetabellen.
  observeEvent(input$karta_shape_click, {
    id <- input$karta_shape_click$id
    if (!is.null(id) && id %in% appdata$omraden$omrade) {
      updateSelectInput(session, "omrade", selected = id)
    }
  })

  # ---- Inkomst, hushållsinkomst, ekonomisk standard och sysselsättning ------
  # Meddelande om en tabell saknas eller inte kunde läsas
  for (namn in names(tabellkontrakt)) local({
    n <- namn
    output[[paste0("status_", n)]] <- renderUI({
      if (is.null(appdata[[n]])) {
        orsak <- appdata$tabellfel[n]
        div(class = "alert alert-warning",
            paste0("Tabellen ", n, " saknas eller kunde inte läsas."),
            if (!is.null(orsak) && !is.na(orsak)) paste(" Orsak:", orsak))
      }
    })
  })
  siffror <- function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE)
  tom_tabell <- function() data.frame(Uppgift = "Tabellen saknas")

  # Inkomst per åldersgrupp för valt distrikt, år och kön
  inkomsturval <- reactive({
    req(!is.null(appdata$inkomst), input$kon_ink %in% c("Totalt", "Kvinnor", "Män"))
    o <- valt_omrade(); a <- valt_ar()
    df <- appdata$inkomst |> filter(omrade == o, ar == a)
    if (input$kon_ink == "Totalt") {
      if (any(df$kon == "Totalt")) {
        df <- filter(df, kon == "Totalt")
      } else {
        # Totalen beräknas ur könsuppdelningen. Medianen går inte att härleda.
        df <- df |>
          group_by(ar, omrade, aldersgrupp) |>
          summarise(antal_ind = summa_na(antal_ind), summa_ink = summa_na(summa_ink),
                    .groups = "drop") |>
          mutate(kon = "Totalt",
                 medel_ink = ifelse(antal_ind > 0, round(summa_ink / antal_ind), NA_real_),
                 median_ink = NA_real_)
      }
    } else {
      df <- filter(df, kon == input$kon_ink)
    }
    df |>
      select(any_of(c("ar", "omrade", "kon", "aldersgrupp",
                      "antal_ind", "median_ink", "summa_ink", "medel_ink"))) |>
      mutate(aldersgrupp = factor(aldersgrupp, levels = aldersgruppsordning)) |>
      arrange(aldersgrupp) |>
      mutate(aldersgrupp = as.character(aldersgrupp))
  })
  output$inkomst_diagram <- renderPlot({
    req(input$ink_matt %in% unname(inkomstmatt))
    df <- inkomsturval()
    validate(need(nrow(df) > 0 && any(is.finite(df[[input$ink_matt]])),
                  "Inkomstuppgift saknas för valt urval. För Totalt finns ingen median om totalrader saknas."))
    # Fasta axlar: alla åldersgrupper visas alltid på x-axeln, och y-axeln går till störst
    # värde bland alla kön och år i valt distrikt, så Män, Kvinnor och Totalt går att jämföra.
    df$aldersgrupp <- factor(df$aldersgrupp, levels = aldersgruppsordning)
    ink <- appdata$inkomst[appdata$inkomst$omrade == valt_omrade(), input$ink_matt]
    ymax <- suppressWarnings(max(ink, na.rm = TRUE))
    ggplot(df, aes(aldersgrupp, .data[[input$ink_matt]])) +
      geom_col(fill = "#178571", na.rm = TRUE) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(labels = siffror) +
      coord_cartesian(ylim = c(0, if (is.finite(ymax)) ymax * 1.05 else NA)) +
      tema_app() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste(names(inkomstmatt)[match(input$ink_matt, inkomstmatt)],
                         "per åldersgrupp –", input$kon_ink, valt_ar()),
           x = NULL, y = "Kronor", caption = appdata$kalla)
  })
  output$inkomst_tabell <- renderDT({
    if (is.null(appdata$inkomst)) return(visa_tabell(tom_tabell()))
    visa_tabell(inkomsturval(), heltal = c("antal_ind", "median_ink", "summa_ink", "medel_ink"))
  })

  # Hushållens disponibla inkomst över åren för valt distrikt
  hushallurval <- reactive({
    req(!is.null(appdata$hushall_inkomst))
    appdata$hushall_inkomst |> filter(omrade == valt_omrade()) |> arrange(ar)
  })
  output$hushall_diagram <- renderPlot({
    df <- hushallurval()
    validate(need(nrow(df) > 0 && any(is.finite(c(df$medel_ink, df$median_ink))),
                  "Hushållsinkomst saknas för valt distrikt."))
    ar_med_data <- sort(unique(df$ar))
    df <- df |>
      select(ar, medel_ink, median_ink) |>
      pivot_longer(-ar, names_to = "matt", values_to = "kronor") |>
      mutate(matt = factor(matt, levels = c("medel_ink", "median_ink"),
                           labels = c("Medelinkomst", "Medianinkomst")))
    ggplot(df, aes(ar, kronor, colour = matt, linetype = matt)) +
      geom_line(na.rm = TRUE) + geom_point(na.rm = TRUE) +
      geom_vline(xintercept = valt_ar(), linetype = "dotted") +
      scale_colour_manual(values = c(Medelinkomst = "#178571", Medianinkomst = "#464d48"), name = NULL) +
      scale_linetype_manual(values = c(Medelinkomst = "solid", Medianinkomst = "dashed"), name = NULL) +
      scale_x_continuous(breaks = ar_med_data) +
      scale_y_continuous(labels = siffror) +
      tema_app() + theme(legend.position = "bottom") +
      labs(title = "Hushållens disponibla inkomst", x = NULL, y = "Kronor",
           caption = appdata$kalla)
  })
  output$hushall_tabell <- renderDT({
    if (is.null(appdata$hushall_inkomst)) return(visa_tabell(tom_tabell()))
    visa_tabell(hushallurval(),
                heltal = c("antal_hus", "summa_ink", "medel_ink", "median_ink"))
  })

  # Ekonomisk standard: andel hushåll per kvartil, valt distrikt och år
  ekstandardurval <- reactive({
    req(!is.null(appdata$ekonomisk_standard))
    o <- valt_omrade(); a <- valt_ar()
    appdata$ekonomisk_standard |>
      filter(omrade == o, ar == a) |>
      arrange(kvartil_ink)
  })
  output$ekstandard_diagram <- renderPlot({
    df <- ekstandardurval()
    validate(need(nrow(df) > 0 && any(is.finite(df$andel)),
                  "Uppgift om ekonomisk standard saknas för valt urval."))
    etiketter <- c("kvartil 1" = "Kvartil 1 (lägst)", "kvartil 2" = "Kvartil 2",
                   "kvartil 3" = "Kvartil 3", "kvartil 4" = "Kvartil 4 (högst)")
    df$kvartil <- ifelse(is.na(etiketter[df$kvartil_ink]), df$kvartil_ink,
                         unname(etiketter[df$kvartil_ink]))
    df$kvartil <- factor(df$kvartil, levels = unique(df$kvartil))
    df$etikett <- ifelse(is.finite(df$andel),
                         paste0(formatC(100 * df$andel, format = "f", digits = 1, decimal.mark = ","), " %"),
                         "")
    ggplot(df, aes(kvartil, andel)) +
      geom_col(fill = "#178571", na.rm = TRUE) +
      geom_text(aes(label = etikett), vjust = -0.4, na.rm = TRUE) +
      geom_hline(yintercept = 0.25, linetype = "dashed", colour = "#464d48") +
      scale_y_continuous(labels = function(x) paste0(100 * x, " %"),
                         expand = expansion(mult = c(0, 0.12))) +
      tema_app() +
      labs(title = paste("Ekonomisk standard – andel hushåll per kvartil", valt_ar()),
           x = NULL, y = "Andel hushåll",
           caption = paste0("Kvartilgränser för hela riket. Streckad linje: 25 %. ", appdata$kalla))
  })
  output$ekstandard_tabell <- renderDT({
    if (is.null(appdata$ekonomisk_standard)) return(visa_tabell(tom_tabell()))
    visa_tabell(ekstandardurval(), heltal = c("antal_hus", "tot_hus"),
                procent = "andel", dom = "t")
  })

  # Sysselsättning: valt block (näringsgren, sektor eller totalt), distrikt och år
  syssurval <- function(tabell, block) {
    req(!is.null(appdata[[tabell]]), block %in% unname(blockval))
    o <- valt_omrade(); a <- valt_ar()
    appdata[[tabell]] |>
      filter(omrade == o, ar == a, block == !!block) |>
      mutate(etikett = etikettera_rad(rad))
  }
  blocketikett <- function(block) names(blockval)[match(block, blockval)]

  syss_alder_urval <- reactive(syssurval("sysselsattning_alder", input$syss_block_alder))
  output$syss_alder_diagram <- renderPlot({
    df <- syss_alder_urval()
    validate(need(nrow(df) > 0 && length(summakolumner) > 0,
                  "Sysselsättningsuppgift saknas för valt urval."))
    # Åldersgrupperna är uteslutande inom en rad, så de får summeras per rad.
    df$summa <- rowSums(df[summakolumner], na.rm = TRUE)
    df <- df[order(df$summa), ]
    df$etikett <- factor(df$etikett, levels = unique(df$etikett))
    ggplot(df, aes(etikett, summa)) +
      geom_col(fill = "#178571") + coord_flip() +
      scale_y_continuous(labels = siffror, expand = expansion(mult = c(0, 0.05))) +
      tema_app() +
      labs(title = paste("Förvärvsarbetande –", tolower(blocketikett(input$syss_block_alder)), valt_ar()),
           x = NULL, y = "Antal personer",
           caption = paste("Rader kan överlappa; summera inte över rader.", appdata$kalla))
  })
  output$syss_alder_tabell <- renderDT({
    if (is.null(appdata$sysselsattning_alder)) return(visa_tabell(tom_tabell()))
    df <- syss_alder_urval()
    ut <- data.frame(Rad = df$etikett, df[alderskolumner], check.names = FALSE,
                     stringsAsFactors = FALSE)
    if (length(summakolumner)) ut$Summa <- rowSums(df[summakolumner], na.rm = TRUE)
    visa_tabell(ut, heltal = setdiff(names(ut), "Rad"))
  })

  syss_kon_urval <- reactive(syssurval("sysselsattning_kon", input$syss_block_kon))
  output$syss_kon_diagram <- renderPlot({
    df <- syss_kon_urval()
    validate(need(nrow(df) > 0, "Sysselsättningsuppgift saknas för valt urval."))
    df$summa <- rowSums(df[c("man", "kvinnor")], na.rm = TRUE)
    ordning <- df$etikett[order(df$summa)]
    df <- df |>
      select(etikett, man, kvinnor) |>
      pivot_longer(c(man, kvinnor), names_to = "kon", values_to = "antal") |>
      mutate(kon = factor(kon, levels = c("kvinnor", "man"), labels = c("Kvinnor", "Män")),
             etikett = factor(etikett, levels = unique(ordning)))
    ggplot(df, aes(etikett, antal, fill = kon)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
      coord_flip() +
      scale_fill_manual(values = c(Kvinnor = "#178571", Män = "#464d48"), name = NULL) +
      scale_y_continuous(labels = siffror, expand = expansion(mult = c(0, 0.05))) +
      tema_app() + theme(legend.position = "bottom") +
      labs(title = paste("Förvärvsarbetande efter kön –", tolower(blocketikett(input$syss_block_kon)), valt_ar()),
           x = NULL, y = "Antal personer",
           caption = paste("Rader kan överlappa; summera inte över rader.", appdata$kalla))
  })
  output$syss_kon_tabell <- renderDT({
    if (is.null(appdata$sysselsattning_kon)) return(visa_tabell(tom_tabell()))
    df <- syss_kon_urval()
    ut <- data.frame(Rad = df$etikett, Män = df$man, Kvinnor = df$kvinnor,
                     Totalt = rowSums(df[c("man", "kvinnor")], na.rm = TRUE),
                     check.names = FALSE, stringsAsFactors = FALSE)
    visa_tabell(ut, heltal = c("Män", "Kvinnor", "Totalt"))
  })
})
