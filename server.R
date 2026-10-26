shinyServer(function(input, output, session) {
  valt_omrade <- reactive({
    req(input$omrade %in% appdata$omraden$omrade)
    input$omrade
  })
  valt_ar <- reactive({
    req(input$ar %in% as.character(arval))
    as.numeric(input$ar)
  })
  flytturval <- reactive({
    appdata$flytt_summering |>
      filter(omrade == valt_omrade(), ar == valt_ar())
  })
  utbildningsurval <- reactive({
    req(input$kon %in% c("Totalt", "Kvinnor", "Män"))
    appdata$utbildning |>
      filter(omrade == valt_omrade(), ar == valt_ar(), kon == input$kon)
  })
  jamforelsedata <- reactive({
    req(input$indikator %in% unname(indikatorer))
    appdata$flytt_summering |>
      filter(ar == valt_ar()) |>
      transmute(omrade, varde = .data[[input$indikator]]) |>
      right_join(appdata$omraden, by = "omrade") |>
      mutate(valt = omrade == valt_omrade()) |>
      arrange(desc(varde), omrade)
  })
  output$befolkning <- renderPlot({
    df <- appdata$flytt_summering |>
      filter(omrade == valt_omrade()) |>
      arrange(ar)
    validate(need(nrow(df) > 0 && any(is.finite(df$befolkning)),
                  "Befolkningsuppgift saknas för valt distrikt."))
    # Saknade år läggs in som NA för att inte rita en obruten tidsserie.
    df <- tidyr::complete(df, ar = seq(min(ar), max(ar), by = 1))
    if (all(is.finite(df$befolkning))) {
      return(skapa_diagram(df, "ar", "befolkning",
                           "Befolkning vid årets slut", linje = TRUE) +
               geom_vline(xintercept = valt_ar(), linetype = "dotted"))
    }
    p <- ggplot(df, aes(ar, befolkning)) +
      geom_line(aes(group = cumsum(is.na(befolkning))), colour = "#178571", na.rm = TRUE) +
      geom_point(colour = "#178571", na.rm = TRUE) +
      geom_vline(xintercept = valt_ar(), linetype = "dotted") +
      theme_minimal(base_size = 12) +
      labs(title = "Befolkning vid årets slut", x = "År", y = "Personer",
           caption = appdata$kalla)
    p
  })
  output$flyttningar <- renderPlot({
    df <- flytturval() |>
      select(inflyttad, utflyttad, flyttnetto) |>
      pivot_longer(everything(), names_to = "matt", values_to = "antal")
    validate(need(nrow(df) > 0 && any(is.finite(df$antal)),
                  "Flyttuppgift saknas för valt urval."))
    df$matt <- factor(df$matt, levels = c("inflyttad", "utflyttad", "flyttnetto"),
                      labels = c("Inflyttade", "Utflyttade", "Flyttnetto"))
    skapa_diagram(df, "matt", "antal", paste("Flyttningar", valt_ar()))
  })
  output$utbildning <- renderPlot({
    df <- utbildningsurval() |>
      select(all_of(names(utbildningsnivaer))) |>
      pivot_longer(everything(), names_to = "niva", values_to = "antal")
    validate(need(nrow(df) > 0 && any(is.finite(df$antal)),
                  "Utbildningsuppgift saknas för valt urval."))
    df$niva <- factor(df$niva, levels = names(utbildningsnivaer),
                      labels = unname(utbildningsnivaer))
    skapa_diagram(df, "niva", "antal",
                  paste("Utbildningsnivå –", input$kon, valt_ar()))
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
      labs(title = names(indikatorer)[match(input$indikator, indikatorer)],
           x = NULL, y = "Antal", caption = appdata$kalla)
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
  output$flyttabell <- renderDT({
    datatable(flytturval(), rownames = FALSE, escape = TRUE,
              options = list(dom = "t"))
  })
  output$utbildningstabell <- renderDT({
    datatable(utbildningsurval(), rownames = FALSE, escape = TRUE,
              options = list(dom = "t"))
  })

  output$kartstatus <- renderUI({
    if (is.null(appdata$distrikt)) {
      div(class = "alert alert-info",
          "Kartan väntar på riktiga distriktsgränser från karta.distrikt.",
          " Ingen databasanslutning görs och inga syntetiska gränser visas.")
    }
  })
  output$karta <- renderPlot({
    req(!is.null(appdata$distrikt))
    geo <- appdata$distrikt
    geo$valt <- geo$omrade == valt_omrade()
    p <- ggplot() +
      geom_sf(data = geo, aes(fill = valt), colour = "white", linewidth = 0.2) +
      scale_fill_manual(values = c("FALSE" = "#d7ebe6", "TRUE" = "#c65d00"),
                        guide = "none")
    if (!is.null(appdata$kommun)) {
      p <- p + geom_sf(data = appdata$kommun, fill = NA,
                       colour = "#464d48", linewidth = 0.4)
    }
    p + theme_void() + labs(title = "Dalarna – distrikt",
                            caption = "Valt distrikt markeras i orange")
  })
})
