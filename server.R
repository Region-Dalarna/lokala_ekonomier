shinyServer(function(input, output, session) {
  valt_omrade <- reactive({
    req(input$omrade %in% appdata$omraden$omrade)
    input$omrade
  })
  valt_ar <- reactive({
    req(input$ar %in% as.character(arval))
    as.numeric(input$ar)
  })
  # Alla diagram ritas som SVG med ggiraph, så att de blir skarpa i alla skärmstorlekar.
  # bredd och hojd (tum) styr textstorleken: ett större mått ger relativt mindre text.
  # Gemensamma inställningar för alla diagram: hover, ingen markering och popup i appens typsnitt
  giraf_val <- list(
    opts_hover(css = "opacity:0.85;"),
    opts_selection(type = "none"),
    opts_tooltip(css = paste0("font-family: 'Poppins', var(--rd-font-sans), sans-serif; font-size: 13px; ",
                              "background: #fff; color: #424242; padding: 6px 10px; border-radius: 6px; ",
                              "box-shadow: 0 1px 5px rgba(0,0,0,.25);"))
  )
  renderSvg <- function(expr, bredd = 10, hojd = 4) {
    f <- shiny::exprToFunction(expr, parent.frame(), FALSE)
    renderGirafe({
      girafe_alt(ggobj = f(), width_svg = bredd, height_svg = hojd,
                 options = giraf_val)
    })
  }

  # Valet "Totalt" eller "Uppdelat på kön" används i alla sektioner med könsuppgift
  delat <- function(x) identical(x, "Uppdelat på kön")
  kon_ordning <- c("Kvinnor", "Män")
  kon_farger <- c(Kvinnor = rd_farg$kvinnor, Män = rd_farg$man)
  siffror <- function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE)
  # Värde för popup: saknat värde skrivs ut som text
  vt <- function(x) ifelse(is.finite(x), siffror(x), "uppgift saknas")
  tom_tabell <- function() data.frame(Uppgift = "Tabellen saknas")

  # ---- Översikt: nyckeltal för valt distrikt och år --------------------------
  output$nyckeltal <- renderUI({
    o <- valt_omrade(); a <- valt_ar()
    hamta <- function(tabell, kolumn) {
      d <- appdata[[tabell]]
      if (is.null(d) || !kolumn %in% names(d)) return(NA_real_)
      v <- d[[kolumn]][d$omrade == o & d$ar == a]
      if (length(v)) as.numeric(v[1]) else NA_real_
    }
    kpi <- function(etikett, varde, enhet = "") {
      div(class = "rd-kpi", div(class = "rd-kpi__label", etikett),
          div(class = "rd-kpi__value",
              if (is.finite(varde)) paste0(siffror(round(varde)), enhet) else "–"))
    }
    div(class = "rd-kpi-row",
        style = "display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 12px; margin-bottom: 16px;",
        kpi("Befolkning", hamta("flytt_summering", "befolkning")),
        kpi("Flyttnetto", hamta("flytt_summering", "flyttnetto")),
        kpi("Antal hushåll", hamta("hushall_inkomst", "antal_hus")),
        kpi("Medelinkomst, hushåll", hamta("hushall_inkomst", "medel_ink"), " kr"))
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
  # Totalt, eller uppdelat på kvinnor och män
  flytt_kon_urval <- function(val) {
    req(val %in% visaval)
    d <- flytt_alder_med_totalt()
    d <- if (delat(val)) d[d$kon %in% kon_ordning, ] else d[d$kon == "Totalt", ]
    d$kon <- factor(d$kon, levels = c("Totalt", kon_ordning))
    d
  }
  flytt_alder_kon <- reactive(flytt_kon_urval(input$vis_flytt))
  output$flytt_alder_diagram <- renderSvg(bredd = 9, hojd = 6.6, {
    serie <- flytt_alder_med_totalt()
    a <- valt_ar()
    df <- flytt_alder_kon()
    df <- df[df$ar == a, ]
    df$kon <- droplevels(df$kon)
    validate(need(nrow(df) > 0 && any(is.finite(df$flyttnetto)),
                  "Flyttuppgift per åldersgrupp saknas för valt distrikt och år."))
    # Fast x-axel: samma gränser för alla kön och år i valt distrikt, så att Män, Kvinnor,
    # Totalt och olika år går att jämföra.
    lim <- range(c(0, serie$flyttnetto), na.rm = TRUE)
    marginal <- max(diff(lim) * 0.22, 1)
    # Avstånd (i dataenheter) mellan stapelns ände eller nollinjen och texten
    luft <- max(diff(lim) * 0.035, 0.4)
    # Yngsta åldersgruppen överst
    df$aldersgrupp <- factor(df$aldersgrupp, levels = rev(flytt_alder_ordning))
    df$positiv <- is.na(df$flyttnetto) | df$flyttnetto >= 0
    delsummor <- tapply(df$flyttnetto, df$kon, summa_na)
    # Uppdelat på kön får staplarna könets färg; annars visar färgen om flyttnettot är positivt
    df$farg <- if (delat(input$vis_flytt)) as.character(df$kon) else ifelse(df$positiv, "pos", "neg")
    df$popup <- paste0(df$aldersgrupp, "<br/>", if (delat(input$vis_flytt)) paste0(df$kon, ": ") else "",
                       "Flyttnetto ", vt(df$flyttnetto))
    ggplot(df, aes(x = flyttnetto, y = aldersgrupp)) +
      geom_col_interactive(aes(fill = farg, tooltip = popup, data_id = paste(kon, aldersgrupp)),
                           width = 0.82, na.rm = TRUE) +
      geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
      # Åldersgruppen skrivs vid nollinjen, på motsatt sida om stapeln
      geom_label(aes(x = ifelse(positiv, -luft, luft), label = aldersgrupp, hjust = ifelse(positiv, 1, 0)),
                 fill = "white", alpha = 0.85, label.size = 0, label.padding = unit(0.1, "lines"),
                 size = 4.2) +
      geom_text(aes(x = flyttnetto + ifelse(positiv, luft, -luft), label = flyttnetto,
                    hjust = ifelse(positiv, 0, 1)),
                size = 4.4, na.rm = TRUE) +
      facet_wrap(vars(kon), nrow = 1) +
      scale_fill_manual(values = c(pos = rd_farg$primar, neg = rd_farg$gra_mork, kon_farger), guide = "none") +
      scale_x_continuous(limits = lim + c(-marginal, marginal), labels = siffror) +
      scale_y_discrete(drop = FALSE) +
      tema_app() +
      theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", size = 13),
            axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 12)) +
      labs(title = paste("Flyttnetto per åldersgrupp", a),
           subtitle = paste0("Totalt flyttnetto – ",
                             paste(names(delsummor), siffror(delsummor), sep = ": ", collapse = ", ")),
           x = "Flyttnetto (inflyttade minus utflyttade)", y = NULL, caption = appdata$kalla)
  })
  output$flytt_alder_tabell <- renderDT({
    if (is.null(appdata$flytt_alder)) return(visa_tabell(tom_tabell()))
    d <- flytt_kon_urval(input$vis_flytt_tabell)
    d$kon <- as.character(d$kon)
    bred <- d |>
      select(kon, aldersgrupp, ar, flyttnetto) |>
      tidyr::pivot_wider(names_from = ar, values_from = flyttnetto) |>
      as.data.frame(check.names = FALSE)
    arskol <- setdiff(names(bred), c("kon", "aldersgrupp"))
    arskol <- arskol[order(as.numeric(arskol))]
    bred <- bred[c("kon", "aldersgrupp", arskol)]
    bred <- bred[order(match(bred$kon, c("Totalt", kon_ordning)),
                       match(bred$aldersgrupp, flytt_alder_ordning)), ]
    # En totalrad per kön (åldersgrupperna överlappar inte varandra)
    ut <- do.call(rbind, lapply(unique(bred$kon), function(kv) {
      x <- bred[bred$kon == kv, , drop = FALSE]
      summarad <- data.frame(kon = kv, aldersgrupp = "Totalt", check.names = FALSE)
      for (k in arskol) summarad[[k]] <- summa_na(x[[k]])
      rbind(x, summarad)
    }))
    if (delat(input$vis_flytt_tabell)) {
      # Kvinnor och Män som kolumngrupper bredvid varandra, med åren under
      ag <- unique(ut$aldersgrupp)
      konv <- intersect(kon_ordning, unique(ut$kon))
      grupper <- lapply(setNames(konv, konv), function(k) {
        x <- ut[ut$kon == k, , drop = FALSE]
        x[match(ag, x$aldersgrupp), arskol, drop = FALSE]
      })
      return(grupperad_tabell(data.frame(Åldersgrupp = ag), grupper, markera = as.character(valt_ar()),
                              fet_rad = c("Åldersgrupp", "Totalt"), dom = "t", ordering = FALSE, pageLength = 50))
    }
    names(ut)[1:2] <- c("Kön", "Åldersgrupp")
    ut <- ut[-1]
    dt <- visa_tabell(ut, heltal = arskol, dom = "t", ordering = FALSE, pageLength = 50)
    dt <- DT::formatStyle(dt, "Åldersgrupp", target = "row",
                          fontWeight = DT::styleEqual("Totalt", "bold"))
    if (as.character(valt_ar()) %in% arskol) {
      dt <- DT::formatStyle(dt, as.character(valt_ar()), backgroundColor = rd_farg$mark)
    }
    dt
  })
  # Befolkningsutveckling över flera år. Serien läses ur flytt_serie_ar; saknas den används
  # flytt_summering. Valt år markeras om det finns i serien.
  # Serien för befolkning: Totalt (kolumnen befolkning), eller uppdelat på kön (kolumnerna man och
  # kvinnor). Saknas serien används flytt_summering. Inflyttade och utflyttade har ingen uppdelning
  # på kön och använder alltid Totalt.
  befolkning_kalla <- function() {
    if (!is.null(appdata$flytt_serie_ar)) appdata$flytt_serie_ar else appdata$flytt_summering
  }
  befolkning_alla <- function(val) {
    req(val %in% visaval)
    d <- befolkning_kalla()
    if (delat(val)) {
      validate(need(all(c("man", "kvinnor") %in% names(d)),
                    paste0("Befolkningsserien saknar kolumnerna man och kvinnor. Kolumner: ",
                           paste(names(d), collapse = ", "))))
      d <- rbind(
        data.frame(ar = d$ar, omrade = d$omrade, kon = "Män", befolkning = d$man,
                   stringsAsFactors = FALSE),
        data.frame(ar = d$ar, omrade = d$omrade, kon = "Kvinnor", befolkning = d$kvinnor,
                   stringsAsFactors = FALSE))
    } else {
      d$kon <- "Totalt"
    }
    d$kon <- factor(d$kon, levels = c("Totalt", kon_ordning))
    d
  }
  befolkning_urval <- function(val) {
    alla <- befolkning_alla(val)
    d <- alla[alla$omrade == valt_omrade(), , drop = FALSE]
    fallback <- FALSE
    # Saknar serien valt distrikt används flytt_sum (ett år), om det finns där
    if (nrow(d) == 0 && !delat(val) && !is.null(appdata$flytt_summering) &&
        !is.null(appdata$flytt_serie_ar)) {
      f <- appdata$flytt_summering
      f <- f[f$omrade == valt_omrade(), , drop = FALSE]
      if (nrow(f) > 0) { d <- transform(f, kon = "Totalt"); fallback <- TRUE }
    }
    validate(need(nrow(d) > 0, paste0(
      "Befolkningsuppgift saknas för valt distrikt. Serien har ",
      length(unique(befolkning_kalla()$omrade)), " distrikt",
      if (nrow(befolkning_kalla())) paste0(", år ", min(befolkning_kalla()$ar), "–", max(befolkning_kalla()$ar)),
      ".")))
    d$kon <- factor(as.character(d$kon), levels = c("Totalt", kon_ordning))
    d <- d[order(d$ar, d$kon), , drop = FALSE]
    attr(d, "fallback") <- fallback
    d
  }
  # Y-axeln. Valet "Samma skala i alla distrikt" ger lika lång axel (i antal personer) i alla distrikt,
  # så att samma förändring ser lika stor ut oavsett distrikt. Axeln behöver inte börja på noll:
  # fönstret placeras kring valt distrikts egna värden, men får aldrig gå under noll.
  # Axelns längd sätts av en percentil av distriktens spridning (skala_percentil) i stället för det
  # största distriktet, annars blir alla andra distrikt platta. Har valt distrikt större spridning än
  # så förlängs axeln just där, och diagrammet säger det i undertexten. Sätt skala_percentil till 1
  # för att alltid ha exakt samma längd (då styr det mest föränderliga distriktet).
  # Valet "Anpassad efter distriktet" zoomar in på valt distrikts värden.
  # Rutnätet har ett streck per steg; siffror skrivs ut med glesare intervall när det blir fler än tio.
  skala_percentil <- 0.9
  befolkning_yaxel <- function(val, kolumner, steg) {
    d <- befolkning_urval(val)
    r <- suppressWarnings(range(unlist(d[kolumner]), na.rm = TRUE))
    if (!all(is.finite(r))) return(NULL)
    gemensam <- !identical(input$bef_skala, "Anpassad efter distriktet")
    forlangd <- FALSE
    if (gemensam) {
      alla <- befolkning_alla(val)
      spann <- vapply(split(alla, alla$omrade), function(x) {
        rr <- suppressWarnings(range(unlist(x[kolumner]), na.rm = TRUE))
        if (all(is.finite(rr))) diff(rr) else NA_real_
      }, numeric(1))
      spann <- spann[is.finite(spann)]
      gemensam_spann <- if (length(spann)) as.numeric(stats::quantile(spann, skala_percentil, names = FALSE)) else diff(r)
      langd <- max(ceiling(gemensam_spann / steg) * steg + 2 * steg, 6 * steg)
      if (diff(r) > langd - 2 * steg) {            # valt distrikt får inte plats i den gemensamma längden
        langd <- ceiling(diff(r) / steg) * steg + 2 * steg
        forlangd <- TRUE
      }
      lo <- max(0, floor((mean(r) - langd / 2) / steg) * steg)
      hi <- lo + langd
    } else {
      lo <- max(0, floor(r[1] / steg) * steg - steg)
      hi <- ceiling(r[2] / steg) * steg + steg
    }
    # Siffror på glesare intervall (steg, 2 steg, 5 steg ...) tills det är högst tio
    m <- 1
    for (kand in c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)) {
      m <- kand
      if ((hi - lo) / (steg * m) <= 10) break
    }
    sifferteg <- steg * m
    list(ylim = c(lo, hi), langd = hi - lo, forlangd = forlangd,
         breaks = seq(ceiling(lo / sifferteg) * sifferteg, hi, by = sifferteg),
         minor = seq(lo, hi, by = steg))
  }
  # Undertext under diagramrubriken: tom i normalfallet, men en rad finns alltid (ett mellanslag) så att
  # de två diagrammens ytor hamnar på samma höjd. Texten används bara för varningar:
  # förlängd y-axel eller serie som saknas.
  undertext <- function(...) {
    x <- c(...); x <- x[!is.na(x) & nzchar(x)]
    if (length(x)) paste(x, collapse = " ") else " "
  }
  axel_not <- function(yax) {
    if (!is.null(yax) && isTRUE(yax$forlangd))
      "Y-axeln är förlängd här: större förändring än i de flesta distrikt."
    else ""
  }
  # Text under diagrammet: visar om serien saknas och i så fall varför
  serie_not <- function(df = NULL) {
    if (isTRUE(attr(df, "fallback")))
      return("Valt distrikt finns inte i flytt_serie_ar; bara ett år visas ur flytt_sum.")
    if (!is.null(appdata$flytt_serie_ar)) return("")
    orsak <- appdata$tabellfel["flytt_serie_ar"]
    paste0("Bara ett år visas: flytt_serie_ar saknas eller kunde inte läsas.",
           if (!is.null(orsak) && !is.na(orsak)) paste(" Orsak:", orsak))
  }
  output$befolkning_diagram <- renderSvg(bredd = 9.5, hojd = 4.6, {
    df <- befolkning_urval(input$vis_bef)
    validate(need("befolkning" %in% names(df) && any(is.finite(df$befolkning)),
                  "Befolkningsuppgift saknas för valt distrikt."))
    df$kon <- droplevels(df$kon)
    delad <- delat(input$vis_bef)
    yax <- befolkning_yaxel(input$vis_bef, "befolkning", 50)
    df$popup <- paste0(df$ar, "<br/>", if (delad) paste0(df$kon, ": ") else "", vt(df$befolkning), " invånare")
    p <- ggplot(df, aes(ar, befolkning, colour = kon, group = kon)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point_interactive(aes(tooltip = popup, data_id = paste(kon, ar)), size = 2.6, na.rm = TRUE) +
      geom_point_interactive(aes(tooltip = popup, data_id = paste(kon, ar, "valt")),
                             data = df[df$ar == valt_ar(), ], size = 4.4, na.rm = TRUE) +
      scale_colour_manual(values = c(Totalt = rd_farg$accent, kon_farger), name = NULL,
                          guide = "legend") +
      scale_x_continuous(breaks = sort(unique(df$ar))) +
      scale_y_continuous(labels = siffror, breaks = yax$breaks, minor_breaks = yax$minor) +
      coord_cartesian(ylim = yax$ylim, expand = FALSE) +
      tema_app() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
      labs(title = if (delad) "Befolkningsutveckling, uppdelat på kön" else "Befolkningsutveckling",
           subtitle = undertext(serie_not(df), axel_not(yax)), x = NULL, y = "Antal invånare",
           caption = appdata$kalla)
    p
  })
  output$flytt_trend_diagram <- renderSvg(bredd = 9.5, hojd = 4.6, {
    df <- befolkning_urval("Totalt")
    validate(need(all(c("inflyttad", "utflyttad") %in% names(df)), "Flyttuppgift saknas för valt distrikt."))
    yax <- befolkning_yaxel("Totalt", c("inflyttad", "utflyttad"), 10)
    lang <- df |>
      select(ar, Inflyttade = inflyttad, Utflyttade = utflyttad) |>
      pivot_longer(c(Inflyttade, Utflyttade), names_to = "typ", values_to = "antal")
    lang$popup <- paste0(lang$ar, "<br/>", tolower(lang$typ), ": ", vt(lang$antal))
    ggplot(lang, aes(ar, antal, colour = typ, group = typ)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point_interactive(aes(tooltip = popup, data_id = paste(typ, ar)), size = 2.4, na.rm = TRUE) +
      geom_vline(xintercept = valt_ar(), linetype = "dotted") +
      scale_colour_manual(values = c(Inflyttade = rd_farg$primar, Utflyttade = rd_farg$gra_mork), name = NULL) +
      scale_x_continuous(breaks = sort(unique(df$ar))) +
      scale_y_continuous(labels = siffror, breaks = yax$breaks, minor_breaks = yax$minor) +
      coord_cartesian(ylim = yax$ylim, expand = FALSE) +
      tema_app() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Inflyttade och utflyttade",
           subtitle = undertext(axel_not(yax)),
           x = NULL, y = "Antal personer", caption = appdata$kalla)
  })
  output$flytttabell <- renderDT({
    df <- befolkning_urval("Totalt")
    kol <- c(ar = "År", befolkning = "Befolkning", man = "Män", kvinnor = "Kvinnor",
             inflyttad = "Inflyttade", utflyttad = "Utflyttade", flyttnetto = "Flyttnetto")
    kol <- kol[names(kol) %in% names(df)]
    df <- setNames(as.data.frame(df)[names(kol)], unname(kol))
    dt <- visa_tabell(df, heltal = setdiff(names(df), "År"),
                      dom = "t", ordering = FALSE, pageLength = 50)
    # Valt år markeras
    DT::formatStyle(dt, "År", target = "row",
                    backgroundColor = DT::styleEqual(valt_ar(), rd_farg$mark),
                    fontWeight = DT::styleEqual(valt_ar(), "bold"))
  })
  utbildning_kon <- reactive({
    req(input$vis_utb %in% visaval)
    if (delat(input$vis_utb)) kon_ordning else "Totalt"
  })
  utbildningsurval <- reactive({
    o <- valt_omrade(); a <- valt_ar(); k <- utbildning_kon()
    appdata$utbildning |> filter(omrade == o, ar == a, kon %in% k)
  })
  # Fast y-axel: störst värde bland valda kön och alla år i valt distrikt, så att staplarna
  # går att jämföra mellan år.
  utbildning_ymax <- reactive({
    o <- valt_omrade(); k <- utbildning_kon()
    u <- appdata$utbildning[appdata$utbildning$omrade == o & appdata$utbildning$kon %in% k,
                            names(utbildningsnivaer), drop = FALSE]
    mx <- suppressWarnings(max(unlist(u), na.rm = TRUE))
    if (is.finite(mx)) mx * 1.05 else NA_real_
  })
  output$utbildning <- renderSvg(bredd = 10.5, hojd = 4.4, {
    lang <- utbildningsurval() |>
      select(kon, all_of(names(utbildningsnivaer))) |>
      pivot_longer(-kon, names_to = "niva", values_to = "antal")
    validate(need(nrow(lang) > 0 && any(is.finite(lang$antal)),
                  "Utbildningsuppgift saknas för valt urval."))
    lang$niva <- factor(lang$niva, levels = names(utbildningsnivaer),
                        labels = unname(utbildningsnivaer))
    lang$popup <- paste0(lang$niva, "<br/>", lang$kon, ": ", vt(lang$antal))
    if (delat(input$vis_utb)) {
      lang$kon <- factor(lang$kon, levels = kon_ordning)
      p <- ggplot(lang, aes(niva, antal, fill = kon)) +
        geom_col_interactive(aes(tooltip = popup, data_id = paste(niva, kon)),
                             position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
        scale_fill_manual(values = kon_farger, name = NULL) +
        scale_y_continuous(labels = siffror) +
        tema_app() + theme(legend.position = "bottom") +
        labs(title = paste("Utbildningsnivå, uppdelat på kön", valt_ar()),
             x = NULL, y = "Antal", caption = appdata$kalla)
    } else {
      p <- ggplot(lang, aes(niva, antal)) +
        geom_col_interactive(aes(tooltip = popup, data_id = niva), fill = rd_farg$primar, na.rm = TRUE) +
        scale_y_continuous(labels = siffror) +
        tema_app() +
        labs(title = paste("Utbildningsnivå – Totalt", valt_ar()),
             x = NULL, y = "Antal", caption = appdata$kalla)
    }
    if (is.finite(utbildning_ymax())) p <- p + coord_cartesian(ylim = c(0, utbildning_ymax()))
    p
  })
  # Översikt över vilka tabeller som lästs in, och orsaken när en tabell saknas
  output$tabellstatus <- renderDT({
    namn <- unique(c("flytt_summering", "utbildning", names(tabellkontrakt), "bilar_kommun"))
    rad <- lapply(namn, function(n) {
      d <- appdata[[n]]
      orsak <- appdata$tabellfel[n]
      data.frame(Tabell = n, Status = if (is.null(d)) "Saknas" else "Inläst",
                 Rader = if (is.null(d)) NA_integer_ else nrow(d),
                 År = if (is.null(d) || !nrow(d)) "" else paste0(min(d$ar), "–", max(d$ar)),
                 Distrikt = if (is.null(d)) NA_integer_ else length(unique(d$omrade)),
                 Kolumner = if (is.null(d)) "" else paste(names(d), collapse = ", "),
                 Orsak = if (is.null(orsak) || is.na(orsak)) "" else unname(orsak),
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    visa_tabell(do.call(rbind, rad), heltal = c("Rader", "Distrikt"), dom = "t", ordering = FALSE,
                pageLength = 50)
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
  output$jamforelse <- renderSvg(bredd = 19, hojd = 6.3, {
    df <- jamforelsedata() |> filter(is.finite(varde))
    validate(need(nrow(df) > 0, "Inga jämförelsevärden finns för valt år."))
    req(input$rangordning %in% c("hogst", "lagst"))
    rang <- if (input$rangordning == "hogst") head(df$omrade, 15) else tail(df$omrade, 15)
    df <- df |> filter(omrade %in% rang | valt)
    df$etikett <- factor(
      paste(df$namn, df$omrade, sep = " – "),
      levels = rev(paste(df$namn, df$omrade, sep = " – "))
    )
    df$popup <- paste0(df$namn, " (", df$omrade, ")<br/>", vt(df$varde))
    ggplot(df, aes(etikett, varde, fill = valt)) +
      geom_col_interactive(aes(tooltip = popup, data_id = omrade)) + coord_flip() +
      scale_fill_manual(values = c("FALSE" = rd_farg$ljus, "TRUE" = rd_farg$bla_djup),
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
  # ---- Bilinnehav -------------------------------------------------------------
  # Personbilar i trafik per drivmedel. Distrikten är i fokus; kommun, län och riket är jämförelse.
  bilar_distrikt_valt <- reactive({
    req(!is.null(appdata$bilar_distrikt))
    d <- appdata$bilar_distrikt
    d <- d[d$omrade == valt_omrade(), , drop = FALSE]
    d[order(d$ar), , drop = FALSE]
  })
  # Typ av rad i bilar_kommun: kommun, län (Dalarna) eller riket
  bilar_kommun_typ <- function(x) {
    k <- tolower(trimws(x))
    ifelse(grepl("^(dalarna|dalarnas)( län)?$", k), "lan",
           ifelse(grepl("^(riket|hela riket|sverige)$", k), "riket", "kommun"))
  }
  bilar_kommun_visning <- function(x) {
    # Kommunkod visas som namn när kartans kommungeometri finns, annars som den är
    ut <- x
    kod <- grepl("^\\d{4}$", x)
    if (any(kod) && !is.null(karta_kommun)) {
      namn <- karta_kommun$kommunnamn[match(x[kod], karta_kommun$kommunkod)]
      ut[kod] <- ifelse(is.na(namn), x[kod], namn)
    }
    ut
  }
  # Valt distrikts kommun i tabellen bilar_kommun (matchar kod eller namn), annars NA
  bilar_valt_kommun <- reactive({
    req(!is.null(appdata$bilar_kommun))
    kod <- if ("kommunkod" %in% names(appdata$omraden))
      appdata$omraden$kommunkod[match(valt_omrade(), appdata$omraden$omrade)] else NA_character_
    if (is.na(kod)) return(NA_character_)
    vals <- unique(appdata$bilar_kommun$kommun)
    if (kod %in% vals) return(kod)
    namn <- if (!is.null(karta_kommun)) karta_kommun$kommunnamn[match(kod, karta_kommun$kommunkod)] else NA_character_
    if (!is.na(namn)) {
      traff <- vals[normalisera_namn(vals) == normalisera_namn(namn)]
      if (length(traff)) return(traff[1])
    }
    NA_character_
  })
  # Utveckling per drivmedel som linjer, från första året fram till valt år. Urvalet i bilar_urval
  # styr vilka drivmedel som visas; andelen räknas alltid av alla personbilar, även när bara några
  # drivmedel är valda.
  bilar_linjer_data <- reactive({
    d <- bilar_distrikt_valt()
    d <- d[d$ar <= valt_ar(), , drop = FALSE]
    kol <- intersect(bilar_kolumner, names(d))
    lang <- d |>
      select(ar, all_of(kol)) |>
      pivot_longer(all_of(kol), names_to = "drivmedel", values_to = "antal") |>
      mutate(drivmedel = unname(bilar_drivmedel[sub("^personbilar_", "", drivmedel)])) |>
      group_by(ar) |> mutate(andel = antal / sum(antal, na.rm = TRUE) * 100) |> ungroup()
    lang <- lang[lang$drivmedel %in% input$bilar_urval, , drop = FALSE]
    lang$drivmedel <- factor(lang$drivmedel, levels = unname(bilar_drivmedel))
    lang
  })
  bilar_linjer_plot <- function(andel) {
    validate(need(length(input$bilar_urval) > 0, "Välj minst ett drivmedel."))
    d <- bilar_linjer_data()
    validate(need(nrow(d) > 0, "Bilinnehav saknas för valt distrikt och år."))
    d$y <- if (andel) d$andel else d$antal
    d$popup <- paste0(d$drivmedel, "<br/>", d$ar, ": ", vt(d$antal), " bilar (",
                      format(round(d$andel, 1), decimal.mark = ",", nsmall = 1), " %)")
    ggplot(d, aes(ar, y, colour = drivmedel, group = drivmedel)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point_interactive(aes(tooltip = popup, data_id = paste(drivmedel, ar)), size = 2.6, na.rm = TRUE) +
      scale_colour_manual(values = bilar_farger, name = NULL, drop = TRUE) +
      scale_x_continuous(breaks = sort(unique(d$ar))) +
      scale_y_continuous(labels = if (andel) function(x) paste0(format(x, decimal.mark = ",", trim = TRUE), " %") else siffror,
                         limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
      tema_app() + theme(legend.position = "bottom") +
      labs(title = if (andel) "Andel av personbilarna per drivmedel" else "Antal personbilar per drivmedel",
           x = NULL, y = if (andel) "Andel av personbilarna" else "Antal personbilar", caption = appdata$kalla)
  }
  bilar_linjer_tabell_dt <- function(andel) {
    req(length(input$bilar_urval) > 0)
    d <- bilar_linjer_data()
    d$y <- if (andel) d$andel else d$antal
    bred <- d |> select(Drivmedel = drivmedel, ar, y) |>
      tidyr::pivot_wider(names_from = ar, values_from = y) |> as.data.frame(check.names = FALSE)
    bred$Drivmedel <- as.character(bred$Drivmedel)
    ar_kol <- setdiff(names(bred), "Drivmedel")
    bred <- bred[c("Drivmedel", ar_kol[order(as.numeric(ar_kol))])]
    dt <- visa_tabell(bred, dom = "t", ordering = FALSE, pageLength = 20)
    DT::formatRound(dt, setdiff(names(bred), "Drivmedel"), digits = if (andel) 1 else 0, mark = " ", dec.mark = ",")
  }
  output$bilar_linjer_antal <- renderSvg(bredd = 9.5, hojd = 4.6, bilar_linjer_plot(FALSE))
  output$bilar_linjer_andel <- renderSvg(bredd = 9.5, hojd = 4.6, bilar_linjer_plot(TRUE))
  output$bilar_antal_tabell <- renderDT(bilar_linjer_tabell_dt(FALSE))
  output$bilar_andel_tabell <- renderDT(bilar_linjer_tabell_dt(TRUE))
  # År för bilar: valt år om det finns i bilartabellerna, annars närmast tidigare år (eller första)
  bilar_ar <- reactive({
    ar <- sort(unique(c(appdata$bilar_distrikt$ar, appdata$bilar_kommun$ar)))
    req(length(ar) > 0)
    a <- valt_ar()
    if (a %in% ar) a else if (any(ar < a)) max(ar[ar < a]) else min(ar)
  })
  # Jämförelse för valt år: distrikt (15 högsta och valt) eller kommuner
  bilar_jamfor_data <- reactive({
    req(input$bilar_matt %in% unname(bilar_matt))
    m <- input$bilar_matt; a <- bilar_ar()
    if (identical(input$bilar_niva, "Kommuner")) {
      validate(need(!is.null(appdata$bilar_kommun), "Tabellen bilar_kommun saknas."))
      bk <- appdata$bilar_kommun[appdata$bilar_kommun$ar == a, , drop = FALSE]
      validate(need(nrow(bk) > 0, "Inga kommunuppgifter för valt år."))
      typ <- bilar_kommun_typ(bk$kommun)
      valt_k <- bilar_valt_kommun()
      data.frame(namn = ifelse(typ == "kommun", bilar_kommun_visning(bk$kommun),
                               ifelse(typ == "lan", "Dalarnas län", "Riket")),
                 kod = bk$kommun, andel = bilar_andel(bk, m),
                 markerad = ifelse(typ != "kommun", "annan", ifelse(bk$kommun %in% valt_k, "valt", "ovriga")),
                 stringsAsFactors = FALSE)
    } else {
      d <- appdata$bilar_distrikt[appdata$bilar_distrikt$ar == a, , drop = FALSE]
      validate(need(nrow(d) > 0, "Inga distriktsuppgifter för valt år."))
      ut <- data.frame(namn = appdata$omraden$namn[match(d$omrade, appdata$omraden$omrade)],
                       kod = d$omrade, andel = bilar_andel(d, m),
                       markerad = ifelse(d$omrade == valt_omrade(), "valt", "ovriga"),
                       stringsAsFactors = FALSE)
      ut <- ut[is.finite(ut$andel), , drop = FALSE]
      ut <- ut[order(-ut$andel), , drop = FALSE]
      ut[head(seq_len(nrow(ut)), 15) |> union(which(ut$markerad == "valt")), , drop = FALSE]
    }
  })
  output$bilar_jamfor_diagram <- renderSvg(bredd = 9.5, hojd = 5.2, {
    d <- bilar_jamfor_data()
    d <- d[is.finite(d$andel), , drop = FALSE]
    validate(need(nrow(d) > 0, "Inga värden för valt mått och år."))
    d <- d[order(d$andel), , drop = FALSE]
    d$namn <- factor(make.unique(d$namn), levels = make.unique(d$namn))
    d$popup <- paste0(d$namn, "<br/>", format(round(d$andel, 1), decimal.mark = ","), " % av personbilarna")
    mn <- names(bilar_matt)[match(input$bilar_matt, bilar_matt)]
    ggplot(d, aes(namn, andel, fill = markerad)) +
      geom_col_interactive(aes(tooltip = popup, data_id = paste(kod, namn))) + coord_flip() +
      scale_fill_manual(values = c(valt = rd_farg$primar_mork, ovriga = rd_farg$ljus, annan = rd_farg$gra),
                        guide = "none") +
      scale_y_continuous(labels = function(x) paste0(format(x, decimal.mark = ",", trim = TRUE), " %"),
                         expand = expansion(mult = c(0, 0.05))) +
      tema_app() +
      labs(title = paste0(mn, ", andel av personbilarna ", bilar_ar()), x = NULL, y = "Andel",
           caption = appdata$kalla)
  })
  output$bilar_jamfor_tabell <- renderDT({
    d <- bilar_jamfor_data()
    ut <- data.frame(Namn = d$namn, Andel = d$andel, check.names = FALSE)
    ut <- ut[order(-ut$Andel), , drop = FALSE]
    dt <- visa_tabell(ut, dom = "t", ordering = FALSE, pageLength = 50)
    DT::formatRound(dt, "Andel", digits = 1, mark = " ", dec.mark = ",")
  })

  # ---- Jämför områden: befolkningsförändring över år ---------------------------
  # Varje distrikt jämförs med sitt första år i serien (= 0). Valt distrikt är markerat.
  bef_forandring_data <- reactive({
    d <- befolkning_alla("Totalt")
    d <- d[is.finite(d$befolkning), , drop = FALSE]
    validate(need(nrow(d) > 0, "Befolkningsuppgift saknas."))
    d |>
      arrange(omrade, ar) |>
      group_by(omrade) |>
      mutate(bas = first(befolkning), forandring = befolkning - bas,
             procent = ifelse(bas > 0, forandring / bas * 100, NA_real_)) |>
      ungroup() |>
      left_join(appdata$omraden, by = "omrade") |>
      mutate(namn = ifelse(is.na(namn), omrade, namn), valt = omrade == valt_omrade())
  })
  bef_jamfor_procent <- reactive(!identical(input$bef_jamfor_matt, "Förändring i antal personer"))
  # Förändring skrivs med tecken: +2,3 % eller -45
  bef_jamfor_text <- function(y, procent) {
    ifelse(!is.finite(y), "uppgift saknas",
           if (procent) paste0(ifelse(y > 0, "+", ""), format(round(y, 1), decimal.mark = ",", nsmall = 1), " %")
           else paste0(ifelse(y > 0, "+", ""), siffror(round(y))))
  }
  output$bef_jamfor_diagram <- renderSvg(bredd = 19, hojd = 6.3, {
    d <- bef_forandring_data()
    pr <- bef_jamfor_procent()
    d$y <- if (pr) d$procent else d$forandring
    d <- d[is.finite(d$y), , drop = FALSE]
    validate(need(nrow(d) > 0, "Inga värden för befolkningsförändring."))
    d$popup <- paste0(d$namn, " (", d$omrade, ")<br/>", d$ar, ": ", bef_jamfor_text(d$y, pr))
    ovriga <- d[!d$valt, , drop = FALSE]; valt <- d[d$valt, , drop = FALSE]
    ggplot(d, aes(ar, y, group = omrade)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = rd_farg$gra) +
      geom_line_interactive(aes(tooltip = popup, data_id = omrade), data = ovriga,
                            colour = rd_farg$gra, alpha = 0.55, linewidth = 0.6) +
      geom_point_interactive(aes(tooltip = popup, data_id = paste(omrade, ar)), data = ovriga,
                             colour = rd_farg$gra, alpha = 0.55, size = 1.6) +
      geom_line_interactive(aes(tooltip = popup, data_id = omrade), data = valt,
                            colour = rd_farg$primar_mork, linewidth = 1.4) +
      geom_point_interactive(aes(tooltip = popup, data_id = paste(omrade, ar)), data = valt,
                             colour = rd_farg$primar_mork, size = 3) +
      scale_x_continuous(breaks = sort(unique(d$ar))) +
      scale_y_continuous(labels = if (pr) function(x) paste0(format(x, decimal.mark = ",", trim = TRUE), " %") else siffror) +
      tema_app() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = if (pr) "Befolkningsförändring sedan första året" else "Befolkningsförändring i antal personer sedan första året",
           subtitle = paste0(unique(valt$namn)[1], " är mörkblått, övriga distrikt är grå"),
           x = NULL, y = if (pr) "Förändring, procent" else "Förändring, antal personer",
           caption = appdata$kalla)
  })
  output$bef_jamfor_tabell <- renderDT({
    d <- bef_forandring_data()
    pr <- bef_jamfor_procent()
    d$y <- if (pr) d$procent else d$forandring
    bred <- d |> select(Distrikt = namn, ar, y) |>
      tidyr::pivot_wider(names_from = ar, values_from = y) |> as.data.frame(check.names = FALSE)
    arskol <- setdiff(names(bred), "Distrikt")
    bred <- bred[c("Distrikt", arskol[order(as.numeric(arskol))])]
    arskol <- setdiff(names(bred), "Distrikt")
    dt <- visa_tabell(bred, dom = "t", ordering = FALSE, pageLength = 50)
    dt <- DT::formatRound(dt, arskol, digits = if (pr) 1 else 0, mark = " ", dec.mark = ",")
    valtnamn <- unique(d$namn[d$valt])
    DT::formatStyle(dt, "Distrikt", target = "row",
                    backgroundColor = DT::styleEqual(valtnamn, rep(rd_farg$mark, length(valtnamn))),
                    fontWeight = DT::styleEqual(valtnamn, rep("bold", length(valtnamn))))
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
                fillColor = rd_farg$accent, fillOpacity = opacitet,
                highlightOptions = highlightOptions(weight = 1, color = rd_farg$gra_mork,
                                                    fillOpacity = min(1, opacitet + 0.2),
                                                    bringToFront = FALSE),
                options = pathOptions(pane = "pane_distrikt"))
  }
  rita_valt <- function(karta, omrade, opacitet) {
    valt <- karta_distrikt[karta_distrikt$omrade == omrade, ]
    if (nrow(valt) != 1) return(karta)
    addPolygons(karta, data = valt, group = "valt", fillColor = rd_farg$bla_djup,
                fillOpacity = min(1, opacitet + 0.35), weight = 1.5, color = rd_farg$bla_djup, opacity = 1,
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
      var kontroll = L.control({position: 'topright'});
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
      // Zoomknapparna flyttas också till övre högra hörnet, där kartan är tom (Gävleborg)
      if (karta.zoomControl) karta.zoomControl.setPosition('topright');
    }"
  rita_kommun <- function(karta) {
    addPolygons(karta, data = karta_kommun, group = "kommun", fill = FALSE,
                weight = 1.2, color = rd_farg$gra_mork, opacity = 0.9,
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
  for (namn in c(names(tabellkontrakt), "bilar_kommun")) local({
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
  # Inkomst per åldersgrupp för valt distrikt och år: totalt eller uppdelat på kön
  inkomsturval <- reactive({
    req(!is.null(appdata$inkomst), input$vis_ink %in% visaval)
    o <- valt_omrade(); a <- valt_ar()
    df <- appdata$inkomst |> filter(omrade == o, ar == a)
    if (!delat(input$vis_ink)) {
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
      df <- filter(df, kon %in% kon_ordning)
    }
    df |>
      select(any_of(c("ar", "omrade", "kon", "aldersgrupp",
                      "antal_ind", "median_ink", "summa_ink", "medel_ink"))) |>
      mutate(aldersgrupp = factor(aldersgrupp, levels = aldersgruppsordning),
             kon = factor(kon, levels = c("Totalt", kon_ordning))) |>
      arrange(aldersgrupp, kon) |>
      mutate(aldersgrupp = as.character(aldersgrupp), kon = as.character(kon))
  })
  output$inkomst_diagram <- renderSvg(bredd = 9.5, hojd = 3.8, {
    req(input$ink_matt %in% unname(inkomstmatt))
    df <- inkomsturval()
    validate(need(nrow(df) > 0 && any(is.finite(df[[input$ink_matt]])),
                  "Inkomstuppgift saknas för valt urval. För Totalt finns ingen median om totalrader saknas."))
    # Fasta axlar: alla åldersgrupper visas alltid på x-axeln, och y-axeln går till störst
    # värde bland alla kön och år i valt distrikt, så Totalt och uppdelning på kön går att jämföra.
    df$aldersgrupp <- factor(df$aldersgrupp, levels = aldersgruppsordning)
    ink <- appdata$inkomst[appdata$inkomst$omrade == valt_omrade(), input$ink_matt]
    ymax <- suppressWarnings(max(ink, na.rm = TRUE))
    titel <- paste(names(inkomstmatt)[match(input$ink_matt, inkomstmatt)], "per åldersgrupp –",
                   if (delat(input$vis_ink)) "uppdelat på kön" else "Totalt", valt_ar())
    df$popup <- paste0(df$aldersgrupp, "<br/>", if (delat(input$vis_ink)) paste0(df$kon, ": ") else "",
                       vt(df[[input$ink_matt]]), " kr")
    p <- if (delat(input$vis_ink)) {
      df$kon <- factor(df$kon, levels = kon_ordning)
      ggplot(df, aes(aldersgrupp, .data[[input$ink_matt]], fill = kon)) +
        geom_col_interactive(aes(tooltip = popup, data_id = paste(aldersgrupp, kon)),
                             position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
        scale_fill_manual(values = kon_farger, name = NULL)
    } else {
      ggplot(df, aes(aldersgrupp, .data[[input$ink_matt]])) +
        geom_col_interactive(aes(tooltip = popup, data_id = aldersgrupp), fill = rd_farg$primar, na.rm = TRUE)
    }
    p + scale_x_discrete(drop = FALSE) +
      scale_y_continuous(labels = siffror) +
      coord_cartesian(ylim = c(0, if (is.finite(ymax)) ymax * 1.05 else NA)) +
      tema_app() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = if (delat(input$vis_ink)) "bottom" else "none") +
      labs(title = titel, x = NULL, y = "Kronor", caption = appdata$kalla)
  })
  output$inkomst_tabell <- renderDT({
    if (is.null(appdata$inkomst)) return(visa_tabell(tom_tabell()))
    d <- inkomsturval()
    if (delat(input$vis_ink)) {
      # Kvinnor och Män som kolumngrupper bredvid varandra, med inkomstmåtten under
      kol <- c(Antal = "antal_ind", Median = "median_ink", Summa = "summa_ink", Medel = "medel_ink")
      kol <- kol[kol %in% names(d)]
      ag <- unique(d$aldersgrupp)
      konv <- intersect(kon_ordning, unique(d$kon))
      grupper <- lapply(setNames(konv, konv), function(k) {
        x <- d[d$kon == k, , drop = FALSE]
        setNames(x[match(ag, x$aldersgrupp), unname(kol), drop = FALSE], names(kol))
      })
      return(grupperad_tabell(data.frame(Åldersgrupp = ag), grupper, dom = "t", ordering = FALSE, pageLength = 50))
    }
    visa_tabell(d, heltal = c("antal_ind", "median_ink", "summa_ink", "medel_ink"))
  })

  # Hushållens disponibla inkomst över åren för valt distrikt
  hushallurval <- reactive({
    req(!is.null(appdata$hushall_inkomst))
    appdata$hushall_inkomst |> filter(omrade == valt_omrade()) |> arrange(ar)
  })
  output$hushall_diagram <- renderSvg(bredd = 9.5, hojd = 3.4, {
    df <- hushallurval()
    validate(need(nrow(df) > 0 && any(is.finite(c(df$medel_ink, df$median_ink))),
                  "Hushållsinkomst saknas för valt distrikt."))
    ar_med_data <- sort(unique(df$ar))
    df <- df |>
      select(ar, medel_ink, median_ink) |>
      pivot_longer(-ar, names_to = "matt", values_to = "kronor") |>
      mutate(matt = factor(matt, levels = c("medel_ink", "median_ink"),
                           labels = c("Medelinkomst", "Medianinkomst")))
    df$popup <- paste0(df$ar, "<br/>", df$matt, ": ", vt(df$kronor), " kr")
    ggplot(df, aes(ar, kronor, colour = matt, linetype = matt)) +
      geom_line(na.rm = TRUE) +
      geom_point_interactive(aes(tooltip = popup, data_id = paste(matt, ar)), size = 2.4, na.rm = TRUE) +
      geom_vline(xintercept = valt_ar(), linetype = "dotted") +
      scale_colour_manual(values = c(Medelinkomst = rd_farg$primar, Medianinkomst = rd_farg$gra_mork), name = NULL) +
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
  output$ekstandard_diagram <- renderSvg(bredd = 9.5, hojd = 3.4, {
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
    df$popup <- paste0(df$kvartil, "<br/>", df$etikett, " (", vt(df$antal_hus), " av ", vt(df$tot_hus), " hushåll)")
    ggplot(df, aes(kvartil, andel)) +
      geom_col_interactive(aes(tooltip = popup, data_id = kvartil), fill = rd_farg$primar, na.rm = TRUE) +
      geom_text(aes(label = etikett), vjust = -0.4, na.rm = TRUE) +
      geom_hline(yintercept = 0.25, linetype = "dashed", colour = rd_farg$gra_mork) +
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
  # Totalraden (block totalt) för valt distrikt, alla år
  totalrad <- function(tabell) {
    req(!is.null(appdata[[tabell]]))
    o <- valt_omrade()
    d <- appdata[[tabell]] |> filter(omrade == o, block == "totalt")
    if ("totalt" %in% d$rad) d <- d[d$rad == "totalt", ]
    d |> group_by(ar) |> slice(1) |> ungroup() |> arrange(ar)
  }

  # ---- Arbetsmarknad: totalraden per åldersgrupp och per kön ----------------
  output$arbm_alder_diagram <- renderSvg(bredd = 10.5, hojd = 3.4, {
    a <- valt_ar()
    d <- totalrad("sysselsattning_alder")
    d <- d[d$ar == a, ]
    validate(need(nrow(d) > 0 && length(summakolumner) > 0,
                  "Sysselsättningsuppgift per åldersgrupp saknas för valt år."))
    lang <- data.frame(alder = factor(summakolumner, levels = summakolumner, labels = snygg_alder(summakolumner)),
                       antal = as.numeric(unlist(d[1, summakolumner])))
    lang$popup <- paste0(lang$alder, "<br/>", vt(lang$antal), " personer")
    ggplot(lang, aes(alder, antal)) +
      geom_col_interactive(aes(tooltip = popup, data_id = alder), fill = rd_farg$primar, na.rm = TRUE) +
      scale_y_continuous(labels = siffror, expand = expansion(mult = c(0, 0.05))) +
      tema_app() +
      labs(title = paste("Förvärvsarbetande efter åldersgrupp", a),
           x = NULL, y = "Antal personer", caption = appdata$kalla)
  })
  output$arbm_alder_tabell <- renderDT({
    if (is.null(appdata$sysselsattning_alder)) return(visa_tabell(tom_tabell()))
    d <- totalrad("sysselsattning_alder")
    d <- d[d$ar == valt_ar(), ]
    validate(need(nrow(d) > 0, "Uppgift saknas för valt år."))
    ut <- data.frame(Åldersgrupp = snygg_alder(summakolumner),
                     Antal = as.numeric(unlist(d[1, summakolumner])), check.names = FALSE)
    visa_tabell(ut, heltal = "Antal", dom = "t")
  })
  arbm_kon_data <- reactive({
    req(input$vis_arbm %in% visaval)
    d <- totalrad("sysselsattning_kon")
    validate(need(nrow(d) > 0, "Sysselsättningsuppgift saknas för valt distrikt."))
    d$totalt <- rowSums(d[c("man", "kvinnor")], na.rm = TRUE)
    d
  })
  output$arbm_kon_diagram <- renderSvg(bredd = 10.5, hojd = 3.4, {
    d <- arbm_kon_data()
    d$valt <- d$ar == valt_ar()
    if (delat(input$vis_arbm)) {
      lang <- d |>
        select(ar, valt, man, kvinnor) |>
        pivot_longer(c(man, kvinnor), names_to = "kon", values_to = "antal") |>
        mutate(kon = factor(kon, levels = c("kvinnor", "man"), labels = kon_ordning),
               ar = factor(ar))
      lang$popup <- paste0(lang$ar, "<br/>", lang$kon, ": ", vt(lang$antal), " personer")
      p <- ggplot(lang, aes(ar, antal, fill = kon, alpha = valt)) +
        geom_col_interactive(aes(tooltip = popup, data_id = paste(ar, kon)),
                             position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
        scale_fill_manual(values = kon_farger, name = NULL)
    } else {
      d$ar <- factor(d$ar)
      d$popup <- paste0(d$ar, "<br/>Totalt: ", vt(d$totalt), " personer")
      p <- ggplot(d, aes(ar, totalt, alpha = valt)) +
        geom_col_interactive(aes(tooltip = popup, data_id = ar), fill = rd_farg$primar, width = 0.7)
    }
    p + scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.55), guide = "none") +
      scale_y_continuous(labels = siffror, expand = expansion(mult = c(0, 0.05))) +
      tema_app() +
      theme(legend.position = if (delat(input$vis_arbm)) "bottom" else "none") +
      labs(title = paste("Förvärvsarbetande per år –",
                         if (delat(input$vis_arbm)) "uppdelat på kön" else "Totalt"),
           x = NULL, y = "Antal personer", caption = appdata$kalla)
  })
  output$arbm_kon_tabell <- renderDT({
    if (is.null(appdata$sysselsattning_kon)) return(visa_tabell(tom_tabell()))
    d <- arbm_kon_data()
    ut <- if (delat(input$vis_arbm)) {
      data.frame(År = d$ar, Män = d$man, Kvinnor = d$kvinnor, Totalt = d$totalt, check.names = FALSE)
    } else data.frame(År = d$ar, Totalt = d$totalt)
    dt <- visa_tabell(ut, heltal = setdiff(names(ut), "År"), dom = "t", ordering = FALSE)
    DT::formatStyle(dt, "År", target = "row",
                    backgroundColor = DT::styleEqual(valt_ar(), rd_farg$mark))
  })

  # ---- Pendling (Arbetsmarknad) ---------------------------------------------
  pend_etiketter <- c(borarbetar = "Bor och arbetar i området", inpendling = "Inpendling",
                      utpendling = "Utpendling", dagbef = "Dagbefolkning", nattbef = "Nattbefolkning")
  # Pendlingen för valt distrikt, alla år: Totalt, eller uppdelat på kvinnor och män.
  # Saknas totalrader beräknas Totalt som summan av kvinnor och män.
  pend_urval <- function(val) {
    req(!is.null(appdata$pendling), val %in% visaval)
    d <- appdata$pendling[appdata$pendling$omrade == valt_omrade(), , drop = FALSE]
    kol <- intersect(names(pend_etiketter), names(d))
    validate(need(all(c("borarbetar", "inpendling", "utpendling") %in% kol),
                  paste("Pendlingstabellen saknar kolumner. Finns:", paste(names(d), collapse = ", "))))
    if (delat(val)) {
      d <- d[d$kon %in% kon_ordning, , drop = FALSE]
    } else if (any(d$kon == "Totalt")) {
      d <- d[d$kon == "Totalt", , drop = FALSE]
    } else {
      d <- d |> group_by(ar, omrade) |>
        summarise(across(all_of(kol), summa_na), .groups = "drop") |> mutate(kon = "Totalt")
    }
    d <- d[c("ar", "kon", kol)] |>
      pivot_longer(all_of(kol), names_to = "matt", values_to = "antal")
    d$kon <- factor(d$kon, levels = c("Totalt", kon_ordning))
    d$matt <- factor(d$matt, levels = names(pend_etiketter), labels = unname(pend_etiketter))
    d[order(d$ar, d$kon, d$matt), ]
  }
  output$pendling_diagram <- renderSvg(bredd = 7.5, hojd = 2.9, {
    alla <- pend_urval(input$vis_pend)
    d <- alla[alla$ar == valt_ar() & alla$matt %in% pend_etiketter[c("borarbetar", "inpendling", "utpendling")], ]
    validate(need(nrow(d) > 0 && any(is.finite(d$antal)),
                  "Pendlingsuppgift saknas för valt distrikt och år."))
    # Fast axel över alla år och kön i valt distrikt
    xmax <- max(alla$antal[alla$matt %in% d$matt], na.rm = TRUE) * 1.1
    d$matt <- factor(d$matt, levels = rev(unname(pend_etiketter[c("borarbetar", "inpendling", "utpendling")])))
    d$popup <- paste0(d$matt, "<br/>", if (delat(input$vis_pend)) paste0(d$kon, ": ") else "", vt(d$antal), " personer")
    p <- if (delat(input$vis_pend)) {
      d$kon <- factor(d$kon, levels = kon_ordning)
      ggplot(d, aes(matt, antal, fill = kon)) +
        geom_col_interactive(aes(tooltip = popup, data_id = paste(matt, kon)),
                             position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
        scale_fill_manual(values = kon_farger, name = NULL) + theme(legend.position = "bottom")
    } else {
      ggplot(d, aes(matt, antal)) +
        geom_col_interactive(aes(tooltip = popup, data_id = matt), fill = rd_farg$primar, width = 0.6, na.rm = TRUE) +
        geom_text(aes(label = siffror(antal)), hjust = -0.2, size = 3.6, na.rm = TRUE)
    }
    p + coord_flip(ylim = c(0, xmax)) +
      scale_y_continuous(labels = siffror) +
      tema_app() +
      theme(legend.position = if (delat(input$vis_pend)) "bottom" else "none") +
      labs(title = paste("Pendling", valt_ar()), x = NULL, y = "Antal personer", caption = appdata$kalla)
  })
  # ---- Största utpendlingsriktningar på karta -------------------------------
  riktning_farger <- c(rd_farg$primar_mork, rd_farg$accent, rd_farg$ljus)   # rang 1, 2 och 3 (stilmallens blå toner)
  distrikt_farg <- rd_farg$gra_mork   # valt distrikt: neutral mörkgrå, rött är reserverat för varning
  normalisera_namn <- function(x) trimws(sub(" kommun$", "", tolower(trimws(as.character(x)))))
  riktningar <- reactive({
    req(!is.null(appdata$riktning_storst_pendling))
    d <- appdata$riktning_storst_pendling
    # Tabell utan distriktskolumn gäller hela regionen och filtreras då bara på år
    if (!all(d$omrade == "(alla)")) d <- d[d$omrade == valt_omrade(), , drop = FALSE]
    d <- d[d$ar == valt_ar() & d$rang <= 3, , drop = FALSE]
    validate(need(nrow(d) > 0, "Uppgift om största pendlingsriktningar saknas för valt distrikt och år."))
    d <- d[order(d$rang), , drop = FALSE]
    # Koppla till kommungeometrin via kommunkod, annars via namn
    d$idx <- NA_integer_
    if (!is.null(karta_kommun)) {
      pa_kod <- !is.na(d$kommunkod) & d$kommunkod %in% karta_kommun$kommunkod
      d$idx[pa_kod] <- match(d$kommunkod[pa_kod], karta_kommun$kommunkod)
      namn_kartan <- normalisera_namn(karta_kommun$kommunnamn)
      rest <- is.na(d$idx) & !is.na(d$kommunnamn)
      d$idx[rest] <- match(normalisera_namn(d$kommunnamn[rest]), namn_kartan)
    }
    d$visning <- ifelse(!is.na(d$kommunnamn), d$kommunnamn,
                        ifelse(!is.na(d$idx) & !is.na(karta_kommun$kommunnamn[d$idx]),
                               karta_kommun$kommunnamn[d$idx], d$kommunkod))
    d
  })
  output$pendling_karta_status <- renderUI({
    if (is.null(appdata$riktning_storst_pendling)) return(NULL)   # statusrutan för tabellen visas separat
    if (is.null(karta_kommun) || is.null(karta_distrikt)) {
      return(div(class = "alert alert-info", "Kartan väntar på kommun- och distriktsgränser från geodata."))
    }
    if (all(appdata$riktning_storst_pendling$omrade == "(alla)")) {
      return(div(class = "alert alert-info",
                 "Tabellen riktning_storst_pendling saknar kolumn för distrikt, så samma tre största riktningar visas för alla distrikt."))
    }
    d <- tryCatch(riktningar(), error = function(e) NULL)
    if (!is.null(d) && any(is.na(d$idx))) {
      div(class = "alert alert-info", paste0(
        "Kunde inte placera på kartan: ", paste(d$visning[is.na(d$idx)], collapse = ", "),
        ". Kommunen ligger utanför Dalarna eller går inte att koppla till kommungeometrin."))
    }
  })
  output$pendling_karta <- renderLeaflet({
    req(!is.null(karta_kommun), !is.null(karta_distrikt))
    d <- riktningar()
    ram <- as.list(sf::st_bbox(karta_kommun))
    # Utsnittet beskärs något så att Dalarna fyller kartan (zoom och panorering går att ändra)
    dx <- ram$xmax - ram$xmin; dy <- ram$ymax - ram$ymin
    ram$xmin <- ram$xmin + 0.03 * dx; ram$xmax <- ram$xmax - 0.03 * dx
    ram$ymin <- ram$ymin + 0.03 * dy; ram$ymax <- ram$ymax - 0.03 * dy
    valt <- karta_distrikt[karta_distrikt$omrade == valt_omrade(), ]
    k <- leaflet(options = leafletOptions(minZoom = 5)) |>
      addTiles(attribution = paste0("&copy; <a href='https://www.openstreetmap.org/copyright'>",
                                    "OpenStreetMap</a>-bidragsgivare"),
               options = tileOptions(className = "mattad-bakgrund")) |>
      addPolygons(data = karta_kommun, fillColor = rd_farg$gra, fillOpacity = 0.12,
                  weight = 1, color = rd_farg$gra, opacity = 0.8,
                  label = ~kommunnamn)
    # De tre största riktningarna, i rangordning
    for (r in which(!is.na(d$idx))) {
      k <- addPolygons(k, data = karta_kommun[d$idx[r], ], fillColor = riktning_farger[d$rang[r]],
                       fillOpacity = 0.8, weight = 2, color = rd_farg$primar_mork, opacity = 1,
                       label = paste0(d$rang[r], ". ", d$visning[r],
                                      if (is.finite(d$antal[r])) paste0(": ", siffror(d$antal[r]), " personer") else ""))
    }
    # Valt distrikt
    if (nrow(valt) == 1) {
      k <- addPolygons(k, data = valt, fillColor = distrikt_farg, fillOpacity = 0.85, weight = 2,
                       color = rd_farg$gra_mork, opacity = 1,
                       label = paste0(appdata$omraden$namn[match(valt_omrade(), appdata$omraden$omrade)],
                                      " (valt distrikt)"))
    }
    etiketter <- paste0(d$rang, ". ", d$visning,
                        ifelse(is.finite(d$antal), paste0(" (", siffror(d$antal), ")"), ""),
                        ifelse(is.na(d$idx), " – utanför kartan", ""))
    k <- addLegend(k, position = "topright", colors = c(distrikt_farg, riktning_farger[d$rang]),
                   labels = c("Valt distrikt", etiketter), opacity = 1,
                   title = "Största utpendling")
    fitBounds(k, ram$xmin, ram$ymin, ram$xmax, ram$ymax)
  })
  output$pendling_riktning_tabell <- renderDT({
    if (is.null(appdata$riktning_storst_pendling)) return(visa_tabell(tom_tabell()))
    d <- riktningar()
    ut <- data.frame(Rang = d$rang, Kommun = d$visning, Antal = d$antal, check.names = FALSE)
    visa_tabell(ut, heltal = "Antal", dom = "t", ordering = FALSE)
  })

  output$pendling_tabell <- renderDT({
    if (is.null(appdata$pendling)) return(visa_tabell(tom_tabell()))
    d <- pend_urval(input$vis_pend)
    d$kon <- as.character(d$kon); d$matt <- as.character(d$matt)
    bred <- d |> tidyr::pivot_wider(names_from = ar, values_from = antal) |>
      as.data.frame(check.names = FALSE)
    arskol <- setdiff(names(bred), c("kon", "matt"))
    bred <- bred[c("kon", "matt", arskol[order(as.numeric(arskol))])]
    # Nettopendling (in minus ut) per kön
    netto <- do.call(rbind, lapply(unique(bred$kon), function(kv) {
      x <- bred[bred$kon == kv, , drop = FALSE]
      r <- data.frame(kon = kv, matt = "Nettopendling (in minus ut)", check.names = FALSE)
      for (k in arskol) r[[k]] <- x[[k]][x$matt == "Inpendling"] - x[[k]][x$matt == "Utpendling"]
      r
    }))
    bred <- rbind(bred, netto)
    bred <- bred[order(match(bred$kon, c("Totalt", kon_ordning)), seq_len(nrow(bred))), ]
    if (delat(input$vis_pend)) {
      # Kvinnor och Män som kolumngrupper bredvid varandra, med åren under
      matt_ordning <- unique(bred$matt)
      grupper <- lapply(setNames(intersect(kon_ordning, unique(bred$kon)), intersect(kon_ordning, unique(bred$kon))),
                        function(k) {
                          x <- bred[bred$kon == k, , drop = FALSE]
                          x[match(matt_ordning, x$matt), arskol, drop = FALSE]
                        })
      return(grupperad_tabell(data.frame(Mått = matt_ordning), grupper, markera = as.character(valt_ar()),
                              dom = "t", ordering = FALSE, pageLength = 50))
    }
    names(bred)[1:2] <- c("Kön", "Mått")
    bred <- bred[-1]
    dt <- visa_tabell(bred, heltal = arskol, dom = "t", ordering = FALSE, pageLength = 50)
    if (as.character(valt_ar()) %in% arskol)
      dt <- DT::formatStyle(dt, as.character(valt_ar()), backgroundColor = rd_farg$mark)
    dt
  })

  # ---- Näringsliv: näringsgren eller sektor, totalt eller uppdelat på kön ----
  blocketikett <- function(block) names(blockval)[match(block, blockval)]

  syss_alder_urval <- reactive(syssurval("sysselsattning_alder", input$syss_block))
  output$syss_alder_tabell <- renderDT({
    if (is.null(appdata$sysselsattning_alder)) return(visa_tabell(tom_tabell()))
    df <- syss_alder_urval()
    ut <- data.frame(Rad = df$etikett, df[alderskolumner], check.names = FALSE,
                     stringsAsFactors = FALSE)
    if (length(summakolumner)) ut$Summa <- rowSums(df[summakolumner], na.rm = TRUE)
    visa_tabell(ut, heltal = setdiff(names(ut), "Rad"))
  })

  syss_kon_urval <- reactive({
    req(input$vis_syss %in% visaval)
    syssurval("sysselsattning_kon", input$syss_block)
  })
  # Läsbara åldersetiketter: 16_24_ar blir "16–24 år" och 66_ar blir "66+ år"
  snygg_alder <- function(x) {
    x <- sub("^x", "", as.character(x))
    ifelse(grepl("^\\d+_\\d+_ar$", x), sub("^(\\d+)_(\\d+)_ar$", "\\1–\\2 år", x),
           ifelse(grepl("^\\d+_ar$", x), sub("^(\\d+)_ar$", "\\1+ år", x), gsub("_", " ", x)))
  }

  # ---- Näringsliv: färg per rad och fördelning på rader (andelsstaplar) ----------
  # Varje rad (näringsgren eller sektor) har samma färg i alla diagram. Störst rad först.
  kat_farger <- c("#0f7090", "#54a1bd", "#f5c64f", "#5e4079", "#e98b49", "#96cfc0",
                  "#8edded", "#a63a5c", "#969696", "#0074a2", "#c9b458", "#7a9e3f")
  rad_farger <- reactive({
    df <- syss_kon_urval()
    summa <- rowSums(df[c("man", "kvinnor")], na.rm = TRUE)
    rader <- df$etikett[order(-summa)]
    farger <- setNames(rep_len(kat_farger, length(rader)), rader)
    # Näringsgrenar: färgkodning ur Bransch_Gxx_farger.xlsx; rader som inte går att koppla behåller sin färg
    if (identical(input$syss_block, "naringsgren")) {
      bf <- bransch_farg_for(rader)
      farger[!is.na(bf)] <- bf[!is.na(bf)]
    }
    farger
  })
  # Färger för en uppsättning rader; rader som saknas i färgtabellen blir grå
  farg_for <- function(rader) {
    f <- rad_farger()[rader]
    f[is.na(f)] <- "#969696"
    setNames(unname(f), rader)
  }
  fordelning_data <- reactive({
    req(input$vis_fordelning %in% c("Åldersgrupp", "Kön"))
    if (input$vis_fordelning == "Åldersgrupp") {
      df <- syss_alder_urval()
      validate(need(nrow(df) > 0 && length(summakolumner) > 0, "Uppgift saknas för valt urval."))
      df[summakolumner] |>
        mutate(etikett = df$etikett) |>
        pivot_longer(-etikett, names_to = "stapel", values_to = "antal") |>
        mutate(stapel = factor(stapel, levels = summakolumner, labels = snygg_alder(summakolumner)))
    } else {
      df <- syss_kon_urval()
      validate(need(nrow(df) > 0, "Uppgift saknas för valt urval."))
      data.frame(etikett = df$etikett, Kvinnor = df$kvinnor, Män = df$man,
                 Totalt = rowSums(df[c("man", "kvinnor")], na.rm = TRUE), check.names = FALSE) |>
        pivot_longer(-etikett, names_to = "stapel", values_to = "antal") |>
        mutate(stapel = factor(stapel, levels = c("Totalt", "Män", "Kvinnor")))
    }
  })
  output$fordelning_diagram <- renderGirafe({
    d <- fordelning_data()
    d$antal[is.na(d$antal)] <- 0
    ordning <- names(rad_farger())
    ordning <- c(ordning[ordning %in% d$etikett], setdiff(unique(d$etikett), ordning))
    d$etikett <- factor(d$etikett, levels = ordning)
    d <- d |> group_by(stapel) |> mutate(andel = antal / sum(antal)) |> ungroup()
    validate(need(any(is.finite(d$andel)), "Uppgift saknas för valt urval."))
    # Popup vid hovring: rad, andel och antal
    d$popup <- paste0(d$etikett, "<br/>", d$stapel, ": ",
                      formatC(100 * d$andel, format = "f", digits = 1, decimal.mark = ","), " %",
                      " (", siffror(d$antal), " personer)")
    p <- ggplot(d, aes(andel, stapel, fill = etikett)) +
      geom_col_interactive(aes(tooltip = popup, data_id = paste(stapel, etikett)), width = 0.7) +
      scale_fill_manual(values = farg_for(ordning), name = NULL, labels = scales::label_wrap(34)) +
      scale_x_continuous(labels = function(x) paste0(100 * x, " %"), expand = c(0, 0)) +
      scale_y_discrete(limits = rev) +
      guides(fill = guide_legend(ncol = 3, byrow = TRUE)) +
      tema_app() +
      theme(legend.position = "bottom", panel.grid.major.y = element_blank(),
            legend.text = element_text(size = 9.5, margin = margin(l = 4, r = 18)),
            legend.spacing.x = unit(10, "pt")) +
      labs(title = paste("Fördelning på", tolower(blocketikett(input$syss_block)), valt_ar()),
           x = "Andel av förvärvsarbetande", y = NULL,
           caption = paste("Andelarna räknas på summan av raderna; rader som överlappar ger skev fördelning.",
                           appdata$kalla))
    girafe_alt(ggobj = p, width_svg = 19, height_svg = 3.2 + 0.42 * ceiling(length(ordning) / 3),
               options = giraf_val)
  })
  # Staplar med näringsgrenens namn vid axeln. Webbläsarens typsnitt (Poppins) är bredare än det
  # textmått diagrammet räknar med, så texten får ett fast avstånd till staplarna (margin r) och
  # radbryts tidigt, vilket håller överskjutet litet.
  output$syss_kon_diagram <- renderGirafe({
    df <- syss_kon_urval()
    validate(need(nrow(df) > 0, "Sysselsättningsuppgift saknas för valt urval."))
    df$summa <- rowSums(df[c("man", "kvinnor")], na.rm = TRUE)
    ordning <- df$etikett[order(df$summa)]
    titel <- paste("Förvärvsarbetande –", tolower(blocketikett(input$syss_block)), valt_ar())
    bildtext <- paste("Rader kan överlappa; summera inte över rader.", appdata$kalla)
    farger <- farg_for(unique(ordning))
    if (delat(input$vis_syss)) {
      df <- df |>
        select(etikett, man, kvinnor) |>
        pivot_longer(c(man, kvinnor), names_to = "kon", values_to = "antal") |>
        mutate(kon = factor(kon, levels = c("kvinnor", "man"), labels = kon_ordning),
               etikett = factor(etikett, levels = unique(ordning)))
      df$popup <- paste0(df$etikett, "<br/>", df$kon, ": ", siffror(df$antal), " personer")
      # Raden ger färgen; Män visas ljusare än Kvinnor
      p <- ggplot(df, aes(etikett, antal, fill = etikett, alpha = kon)) +
        geom_col_interactive(aes(tooltip = popup, data_id = paste(etikett, kon)),
                             position = position_dodge(width = 0.7), width = 0.66, na.rm = TRUE) +
        coord_flip() +
        scale_x_discrete(labels = scales::label_wrap(30)) +
        scale_fill_manual(values = farger, guide = "none") +
        scale_alpha_manual(values = c(Kvinnor = 1, Män = 0.5), name = NULL,
                           guide = guide_legend(override.aes = list(fill = rd_farg$gra_mork))) +
        scale_y_continuous(labels = siffror, expand = expansion(mult = c(0, 0.05))) +
        tema_app() + theme(legend.position = "bottom", axis.text.y = element_text(size = 9.5, margin = margin(r = 38)), plot.margin = margin(8, 14, 8, 14)) +
        labs(title = paste(titel, "– uppdelat på kön"), x = NULL, y = "Antal personer",
             caption = bildtext)
    } else {
      df$etikett <- factor(df$etikett, levels = unique(ordning))
      df$popup <- paste0(df$etikett, "<br/>", siffror(df$summa), " personer")
      p <- ggplot(df, aes(etikett, summa, fill = etikett)) +
        geom_col_interactive(aes(tooltip = popup, data_id = etikett), width = 0.68) + coord_flip() +
        scale_x_discrete(labels = scales::label_wrap(30)) +
        scale_fill_manual(values = farger, guide = "none") +
        scale_y_continuous(labels = siffror, expand = expansion(mult = c(0, 0.05))) +
        tema_app() + theme(axis.text.y = element_text(size = 9.5, margin = margin(r = 38)), plot.margin = margin(8, 14, 8, 14)) +
        labs(title = paste(titel, "– Totalt"), x = NULL, y = "Antal personer", caption = bildtext)
    }
    girafe_alt(ggobj = p, width_svg = 16, height_svg = max(4.5, 1.6 + 0.52 * nlevels(factor(df$etikett)) * (if (delat(input$vis_syss)) 1.2 else 1)),
               options = giraf_val)
  })
  # Cirkeldiagram (packed circles) över samma data som staplarna, ritat med skapa_packed_circles()
  # ur rddiagram. Funktionen returnerar ett statiskt ggplot-objekt (inte girafe), så diagrammet
  # saknar hovringsruta. Filsparning, logga och hämtning av färgnyckel från GitHub stängs av här:
  # färgerna kommer från appens egen färgkodning (samma som staplarna).
  output$syss_cirkel_diagram <- renderPlot({
    validate(need(requireNamespace("rddiagram", quietly = TRUE) &&
                    requireNamespace("packcircles", quietly = TRUE) &&
                    requireNamespace("ggforce", quietly = TRUE),
                  "Cirkeldiagrammet kräver paketen rddiagram, packcircles och ggforce."))
    df <- syss_kon_urval()
    validate(need(nrow(df) > 0, "Sysselsättningsuppgift saknas för valt urval."))
    farger <- farg_for(df$etikett)
    if (delat(input$vis_syss)) {
      ut <- rbind(
        data.frame(bransch = df$etikett, antal = df$kvinnor, kon = kon_ordning[1], stringsAsFactors = FALSE),
        data.frame(bransch = df$etikett, antal = df$man, kon = kon_ordning[2], stringsAsFactors = FALSE))
      grupp <- "kon"
    } else {
      ut <- data.frame(bransch = df$etikett, antal = rowSums(df[c("man", "kvinnor")], na.rm = TRUE),
                       stringsAsFactors = FALSE)
      grupp <- NULL
    }
    ut$farg <- unname(farger[ut$bransch])
    ut <- ut[!is.na(ut$antal) & ut$antal > 0, , drop = FALSE]
    validate(need(nrow(ut) > 0, "Inga värden att visa."))
    p <- tryCatch(
      rddiagram::skapa_packed_circles(
        ut, grupp_kol = grupp, bransch_kol = "bransch", antal_kol = "antal", farg_kol = "farg",
        visa_antal = TRUE, autokoppla_branschnamn_farg = FALSE,
        spara_bildfil = FALSE, logga = FALSE,
        autoladda_typsnitt = FALSE, font = statisk_font(),
        legend_radbryt = 30, storlek_legend = 10,
        diagram_caption = appdata$kalla),
      error = function(e) validate(paste("Cirkeldiagrammet kunde inte ritas:", conditionMessage(e))))
    # Funktionen sorterar legenden alfabetiskt. Här ersätts färgskalan så att förklaringsrutorna
    # ligger i storleksordning (störst totalt antal först, kön sammanräknade), med samma
    # färger och radbrytning som i funktionen.
    ordning <- names(sort(tapply(ut$antal, ut$bransch, sum), decreasing = TRUE))
    palett <- setNames(ut$farg[match(ordning, ut$bransch)], ordning)
    etiketter <- setNames(vapply(ordning, function(x) paste(strwrap(x, width = 30), collapse = "\n"),
                                 character(1)), ordning)
    suppressMessages(p + ggplot2::scale_fill_manual(values = palett, name = NULL,
                                                    breaks = ordning, labels = etiketter))
  }, res = 96)
  output$syss_kon_tabell <- renderDT({
    if (is.null(appdata$sysselsattning_kon)) return(visa_tabell(tom_tabell()))
    df <- syss_kon_urval()
    ut <- if (delat(input$vis_syss)) {
      data.frame(Rad = df$etikett, Män = df$man, Kvinnor = df$kvinnor,
                 check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      data.frame(Rad = df$etikett, Totalt = rowSums(df[c("man", "kvinnor")], na.rm = TRUE),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }
    visa_tabell(ut, heltal = setdiff(names(ut), "Rad"))
  })
})
