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
      summarise(across(any_of(c("flyttnetto", "inflyttad", "utflyttad")), summa_strikt), .groups = "drop") |>
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
  # Översikten: inflyttade, utflyttade och flyttnetto per åldersgrupp för valt år, totalt eller uppdelat på kön.
  # Totalraden är en summa av åldersgrupperna och är NA (–) om någon grupp är dold.
  output$flytt_alder_oversikt_tabell <- renderDT({
    if (is.null(appdata$flytt_alder)) return(visa_tabell(tom_tabell()))
    d <- flytt_alder_kon()
    d <- d[d$ar == valt_ar(), , drop = FALSE]
    d$kon <- as.character(d$kon)
    validate(need(nrow(d) > 0, "Flyttuppgift per åldersgrupp saknas för valt distrikt och år."))
    kol <- c(Inflyttade = "inflyttad", Utflyttade = "utflyttad", Flyttnetto = "flyttnetto")
    kol <- kol[kol %in% names(d)]
    ag <- flytt_alder_ordning[flytt_alder_ordning %in% d$aldersgrupp]
    per_kon <- lapply(setNames(unique(d$kon), unique(d$kon)), function(k) {
      x <- d[d$kon == k, , drop = FALSE]
      x <- x[match(ag, x$aldersgrupp), unname(kol), drop = FALSE]
      tot <- as.data.frame(lapply(x, summa_strikt))
      out <- rbind(x, tot)
      setNames(out, names(kol))
    })
    ordning <- intersect(c("Totalt", kon_ordning), names(per_kon))
    grupper <- if (delat(input$vis_flytt)) per_kon[intersect(kon_ordning, ordning)] else per_kon["Totalt"]
    grupperad_tabell(data.frame(Åldersgrupp = c(ag, "Totalt")), grupper,
                     fet_rad = c("Åldersgrupp", "Totalt"), dom = "t", ordering = FALSE, pageLength = 50)
  })
  # Valt år: inflyttade, utflyttade och flyttnetto som tre kolumner per kön, en rad per åldersgrupp.
  output$flytt_alder_valt_ar <- renderDT({
    if (is.null(appdata$flytt_alder)) return(visa_tabell(tom_tabell()))
    d <- flytt_kon_urval(input$vis_flytt_tabell)
    d$kon <- as.character(d$kon)
    d <- d[d$ar == valt_ar(), , drop = FALSE]
    matt <- c(Inflyttade = "inflyttad", Utflyttade = "utflyttad", Flyttnetto = "flyttnetto")
    validate(need(all(matt %in% names(d)),
                  "Tabellen flytt_alder saknar inflyttad eller utflyttad per åldersgrupp."),
             need(nrow(d) > 0, "Inga uppgifter för valt år."))
    ag <- c(flytt_alder_ordning[flytt_alder_ordning %in% d$aldersgrupp],
            setdiff(unique(d$aldersgrupp), flytt_alder_ordning))
    konv <- intersect(c("Totalt", kon_ordning), unique(d$kon))
    grupper <- lapply(setNames(konv, konv), function(k) {
      x <- d[d$kon == k, , drop = FALSE]
      x <- x[match(ag, x$aldersgrupp), , drop = FALSE]
      ut <- as.data.frame(lapply(matt, function(m) x[[m]]), check.names = FALSE)
      names(ut) <- names(matt)
      # Totalrad: NA (–) om någon åldersgrupp är dold
      rbind(ut, as.data.frame(lapply(ut, summa_strikt), check.names = FALSE))
    })
    grupperad_tabell(data.frame(Åldersgrupp = c(ag, "Totalt")), grupper,
                     fet_rad = c("Åldersgrupp", "Totalt"), dom = "t", ordering = FALSE, pageLength = 50)
  })
  output$flytt_alder_tabell <- renderDT({
    if (is.null(appdata$flytt_alder)) return(visa_tabell(tom_tabell()))
    d <- flytt_kon_urval(input$vis_flytt_tabell)
    d$kon <- as.character(d$kon)
    m <- if (input$flytt_tabell_matt %in% names(d)) input$flytt_tabell_matt else "flyttnetto"
    bred <- d |>
      select(kon, aldersgrupp, ar, all_of(m)) |>
      tidyr::pivot_wider(names_from = ar, values_from = all_of(m)) |>
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
      for (k in arskol) summarad[[k]] <- summa_strikt(x[[k]])   # NA om någon åldersgrupp är dold
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
             kvarboende = "Kvarboende", inflyttad = "Inflyttade", utflyttad = "Utflyttade",
             flyttnetto = "Flyttnetto", fodda = "Födda", doda = "Döda",
             fodelsenetto = "Födelsenetto", befolkningsforandring = "Befolkningsförändring")
    kol <- kol[names(kol) %in% names(df)]
    df <- setNames(as.data.frame(df)[names(kol)], unname(kol))
    dt <- visa_tabell_dold(df, heltal = setdiff(names(df), "År"),
                           dom = "t", ordering = FALSE, pageLength = 50)
    # Valt år markeras
    DT::formatStyle(dt, "År", target = "row",
                    backgroundColor = DT::styleEqual(valt_ar(), rd_farg$mark),
                    fontWeight = DT::styleEqual(valt_ar(), "bold"))
  })
  # Befolkningens förändring över åren, uppdelad i delar: antingen födelsenetto och flyttnetto
  # (som tillsammans är befolkningsförändringen), eller vilka som utgör årets befolkning
  # (kvarboende, inflyttade och födda). Dolda värden ger ingen stapel.
  output$bef_delar_diagram <- renderSvg(bredd = 9.5, hojd = 4.2, {
    req(input$bef_delar %in% c("netto", "sammansattning"))
    df <- befolkning_urval("Totalt")
    netto <- identical(input$bef_delar, "netto")
    kol <- if (netto) c(Födelsenetto = "fodelsenetto", Flyttnetto = "flyttnetto")
    else c(Kvarboende = "kvarboende", Inflyttade = "inflyttad", Födda = "fodda")
    validate(need(all(kol %in% names(df)),
                  paste0("Uppgiften saknas i befolkningsserien. Kolumner som behövs: ", paste(kol, collapse = ", "))))
    lang <- do.call(rbind, lapply(names(kol), function(n)
      data.frame(ar = df$ar, del = n, antal = df[[kol[[n]]]], stringsAsFactors = FALSE)))
    lang$del <- factor(lang$del, levels = names(kol))
    validate(need(any(is.finite(lang$antal)), "Alla värden är dolda för valt distrikt."))
    lang$popup <- paste0(lang$ar, "<br/>", lang$del, ": ", vt(lang$antal))
    farger <- if (netto) c(Födelsenetto = rd_farg$gra_mork, Flyttnetto = rd_farg$primar)
    else c(Kvarboende = rd_farg$gra, Inflyttade = rd_farg$primar, Födda = rd_farg$primar_mork)
    p <- ggplot(lang, aes(ar, antal, fill = del)) +
      geom_col_interactive(aes(tooltip = popup, data_id = paste(del, ar)), width = 0.7, na.rm = TRUE) +
      geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +
      geom_vline(xintercept = valt_ar(), linetype = "dotted")
    if (netto && "befolkningsforandring" %in% names(df)) {
      tot <- data.frame(ar = df$ar, antal = df$befolkningsforandring)
      tot$popup <- paste0(tot$ar, "<br/>Befolkningsförändring: ", vt(tot$antal))
      p <- p + geom_point_interactive(data = tot, aes(ar, antal, tooltip = popup, data_id = paste("tot", ar)),
                                      inherit.aes = FALSE, shape = 23, size = 3, fill = "white", colour = "black",
                                      na.rm = TRUE)
    }
    p + scale_fill_manual(values = farger, name = NULL) +
      scale_x_continuous(breaks = sort(unique(df$ar))) +
      scale_y_continuous(labels = siffror) +
      tema_app() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = if (netto) "Födelsenetto och flyttnetto" else "Befolkningens sammansättning",
           subtitle = if (netto) "Ruten är befolkningsförändringen (födelsenetto + flyttnetto). Dolda värden ger ingen stapel eller ruta."
           else "Kvarboende + inflyttade + födda = befolkningen. Dolda värden ger ingen stapel.",
           x = NULL, y = "Antal personer", caption = appdata$kalla)
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
  # Antal eller andel för utbildning. Andelarna är röjandegranskade i pipelinen (andel_<nivå>, 0–1)
  # och visas i procent; appen räknar inga andelar själv.
  utb_andel <- reactive(identical(input$vis_utb_enhet, "Andel"))
  utb_kolumner <- reactive({
    if (utb_andel()) sub("^antal_", "andel_", names(utbildningsnivaer)) else names(utbildningsnivaer)
  })
  utb_finns_andel <- reactive(all(sub("^antal_", "andel_", names(utbildningsnivaer)) %in% names(appdata$utbildning)))
  output$utbildning <- renderSvg(bredd = 10.5, hojd = 4.4, {
    andel <- utb_andel()
    validate(need(!andel || utb_finns_andel(),
                  "Andelar saknas i tabellen. Kör om pipelinen (roj_alla) och läs in ale.utbildning på nytt."))
    kol <- utb_kolumner()
    lang <- utbildningsurval() |>
      select(kon, all_of(kol)) |>
      rename_with(~ sub("^andel_", "antal_", .x), all_of(kol)) |>
      pivot_longer(-kon, names_to = "niva", values_to = "antal")
    if (andel) lang$antal <- lang$antal * 100
    validate(need(nrow(lang) > 0 && any(is.finite(lang$antal)),
                  "Utbildningsuppgift saknas för valt urval."))
    lang$niva <- factor(lang$niva, levels = names(utbildningsnivaer),
                        labels = unname(utbildningsnivaer))          # Uppgift saknas ligger sist
    lang$popup <- paste0(lang$niva, "<br/>", lang$kon, ": ",
                         if (andel) kart_format(lang$antal, list(dec = 1, enhet = "%")) else vt(lang$antal))
    y_titel <- if (andel) "Andel (%)" else "Antal"
    y_skala <- if (andel) scale_y_continuous(labels = function(x) paste0(x, " %"), limits = c(0, NA),
                                             expand = expansion(mult = c(0, 0.08))) else scale_y_continuous(labels = siffror)
    if (delat(input$vis_utb)) {
      lang$kon <- factor(lang$kon, levels = kon_ordning)
      p <- ggplot(lang, aes(niva, antal, fill = kon)) +
        geom_col_interactive(aes(tooltip = popup, data_id = paste(niva, kon)),
                             position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
        scale_fill_manual(values = kon_farger, name = NULL) +
        y_skala +
        tema_app() + theme(legend.position = "bottom") +
        labs(title = paste("Utbildningsnivå, uppdelat på kön", valt_ar()),
             x = NULL, y = y_titel, caption = appdata$kalla)
    } else {
      p <- ggplot(lang, aes(niva, antal)) +
        geom_col_interactive(aes(tooltip = popup, data_id = niva), fill = rd_farg$primar, na.rm = TRUE) +
        y_skala +
        tema_app() +
        labs(title = paste("Utbildningsnivå – Totalt", valt_ar()),
             x = NULL, y = y_titel, caption = appdata$kalla)
    }
    if (!andel && is.finite(utbildning_ymax())) p <- p + coord_cartesian(ylim = c(0, utbildning_ymax()))
    p
  })
  # Översikt över vilka tabeller som lästs in, och orsaken när en tabell saknas
  output$tabellstatus <- renderDT({
    namn <- unique(c("flytt_summering", "utbildning", names(alla_kontrakt), "bilar_kommun"))
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
    andel <- utb_andel()
    validate(need(!andel || utb_finns_andel(),
                  "Andelar saknas i tabellen. Kör om pipelinen (roj_alla) och läs in ale.utbildning på nytt."))
    # Alla könsrader för valt distrikt och år; dolda värden visas som –
    d <- filter(appdata$utbildning, omrade == o, ar == a)
    d <- d[order(match(d$kon, c("Totalt", kon_ordning))), , drop = FALSE]
    kol <- utb_kolumner()
    ut <- data.frame(Kön = d$kon, stringsAsFactors = FALSE)
    for (i in seq_along(kol)) ut[[unname(utbildningsnivaer)[i]]] <- if (andel) d[[kol[i]]] * 100 else d[[kol[i]]]
    if (!andel) ut$Totalt <- d$totalt
    if (andel) names(ut)[-1] <- paste0(names(ut)[-1], " (%)")
    visa_tabell_dold(ut, heltal = setdiff(names(ut), "Kön"), decimaler = if (andel) 1 else 0, dom = "t", ordering = FALSE)
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
    # Hela länet ska få plats i kartan: utsnittet utvidgas med en marginal (andel av bredd och höjd),
    # så att länsgränsen inte skärs av. zoomSnap 0.1 gör att utsnittet passar nära i stället för
    # att runda till hela zoomsteg. Öka marginalen om kanten ligger för nära, minska den för att zooma in.
    marginal_karta <- 0.05
    dx <- ram$xmax - ram$xmin; dy <- ram$ymax - ram$ymin
    ram$xmin <- ram$xmin - marginal_karta * dx; ram$xmax <- ram$xmax + marginal_karta * dx
    ram$ymin <- ram$ymin - marginal_karta * dy; ram$ymax <- ram$ymax + marginal_karta * dy
    k <- leaflet(options = leafletOptions(minZoom = 5, zoomSnap = 0.1)) |>
      addMapPane("pane_distrikt", zIndex = 410) |>
      addMapPane("pane_valt", zIndex = 420) |>
      addMapPane("pane_kommun", zIndex = 430) |>
      # Ljus bakgrundskarta (rdshinyappar >= 0.0.0.9005): bara ortnamn, tonade. Fler orter syns när man zoomar in.
      lagg_till_bakgrundskarta(etiketter = "orter", etikett_opacitet = 0.6) |>
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
  for (namn in c(names(alla_kontrakt), "bilar_kommun")) local({
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
      # Totalen läses ur tabellen. Appen summerar inte över kön eller rader (röjandeskydd);
      # saknas totalrader visas ingen total.
      df <- filter(df, kon == "Totalt")
    } else {
      df <- filter(df, kon %in% kon_ordning)
    }
    df |>
      select(any_of(c("ar", "omrade", "kon", "aldersgrupp",
                      "antal_ind", "median_ink", "medel_ink"))) |>
      mutate(aldersgrupp = factor(aldersgrupp, levels = aldersgruppsordning),
             kon = factor(kon, levels = c("Totalt", kon_ordning))) |>
      arrange(aldersgrupp, kon) |>
      mutate(aldersgrupp = as.character(aldersgrupp), kon = as.character(kon))
  })
  output$inkomst_diagram <- renderSvg(bredd = 9.5, hojd = 3.8, {
    req(input$ink_matt %in% unname(inkomstmatt))
    df <- inkomsturval()
    validate(need(nrow(df) > 0 && any(is.finite(df[[input$ink_matt]])),
                  "Inkomstuppgift saknas för valt urval. Totalt visas bara om tabellen har totalrader (kön = totalt). Kör om pipelinen eller välj Uppdelat på kön."))
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
      kol <- c(Antal = "antal_ind", Median = "median_ink", Medel = "medel_ink")
      kol <- kol[kol %in% names(d)]
      ag <- unique(d$aldersgrupp)
      konv <- intersect(kon_ordning, unique(d$kon))
      grupper <- lapply(setNames(konv, konv), function(k) {
        x <- d[d$kon == k, , drop = FALSE]
        setNames(x[match(ag, x$aldersgrupp), unname(kol), drop = FALSE], names(kol))
      })
      return(grupperad_tabell(data.frame(Åldersgrupp = ag), grupper, dom = "t", ordering = FALSE, pageLength = 50))
    }
    visa_tabell(d, heltal = c("antal_ind", "median_ink", "medel_ink"))
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
    # Två staplar per år bredvid varandra: medel i huvudfärgen, median i en ljusare nyans
    ggplot(df, aes(ar, kronor, fill = matt)) +
      geom_col_interactive(aes(tooltip = popup, data_id = paste(matt, ar)),
                           position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
      geom_vline(xintercept = valt_ar(), linetype = "dotted") +
      scale_fill_manual(values = c(Medelinkomst = rd_farg$primar_mork, Medianinkomst = rd_farg$ljus), name = NULL) +
      scale_x_continuous(breaks = ar_med_data) +
      scale_y_continuous(labels = siffror, expand = expansion(mult = c(0, 0.05))) +
      tema_app() + theme(legend.position = "bottom") +
      labs(title = "Hushållens disponibla inkomst", x = NULL, y = "Kronor",
           caption = appdata$kalla)
  })
  output$hushall_tabell <- renderDT({
    if (is.null(appdata$hushall_inkomst)) return(visa_tabell(tom_tabell()))
    visa_tabell_dold(hushallurval(), heltal = c("antal_hus", "medel_ink", "median_ink"))
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
      mutate(etikett = etikettera_rad(rad)) |>
      (\(x) x[order(ar_okand(x$rad)), , drop = FALSE])()      # Okänd längst ned
  }
  # Totalraden (block totalt) för valt distrikt, alla år
  totalrad <- function(tabell) {
    req(!is.null(appdata[[tabell]]))
    o <- valt_omrade()
    d <- appdata[[tabell]] |> filter(omrade == o, block == "totalt")
    if ("totalt" %in% d$rad) d <- d[d$rad == "totalt", ]
    d |> group_by(ar) |> slice(1) |> ungroup() |> arrange(ar)
  }

  # Totalraden (block totalt) för valt distrikt och år, eller NULL om den saknas
  total_valt_ar <- function(tabell) {
    d <- tryCatch(totalrad(tabell), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) return(NULL)
    d <- d[d$ar == valt_ar(), , drop = FALSE]
    if (nrow(d)) d[1, , drop = FALSE] else NULL
  }
  # Bortfallet: det som inte syns bland delarna när någon del är dold, räknat som totalen minus de synliga
  # delarna. NA när inget är dolt eller när totalen själv är dold (då går bortfallet inte att visa).
  # Det går redan att räkna fram ur de publicerade värdena, så staplen avslöjar inget nytt.
  dolt_rest <- function(total, delar) {
    if (length(total) != 1 || !is.finite(total) || !anyNA(delar)) return(NA_real_)
    r <- total - sum(delar, na.rm = TRUE)
    if (r > 0) r else NA_real_
  }
  # Bortfallet i totalvyn delat i två: celler som är dolda, och synliga personer på rader där bara ena
  # könet är dolt (radens summa kan inte visas, så de räknas inte bland raderna). Dolda + delvis = totalbortfallet.
  # Båda delarna går att räkna ut ur publicerade värden (totalraden och synliga celler).
  bortfall_total <- function(tot, df) {
    summa <- plus_strikt(df$man, df$kvinnor)
    rest <- dolt_rest(plus_strikt(tot$man, tot$kvinnor), summa)
    if (!is.finite(rest)) return(list(dolt = NA_real_, delvis = NA_real_))
    delvis <- sum(df$man[is.na(df$kvinnor) & !is.na(df$man)]) + sum(df$kvinnor[is.na(df$man) & !is.na(df$kvinnor)])
    dolt <- rest - delvis
    list(dolt = if (dolt > 0) dolt else NA_real_, delvis = if (delvis > 0) delvis else NA_real_)
  }
  # Strikt summa av två tal: NA om något är dolt
  plus_strikt <- function(a, b) ifelse(is.na(a) | is.na(b), NA_real_, a + b)

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
    visa_tabell_dold(ut, heltal = "Antal", dom = "t")
  })
  arbm_kon_data <- reactive({
    req(input$vis_arbm %in% visaval)
    d <- totalrad("sysselsattning_kon")
    validate(need(nrow(d) > 0, "Sysselsättningsuppgift saknas för valt distrikt."))
    d$totalt <- plus_strikt(d$man, d$kvinnor)       # NA om män eller kvinnor är dolda
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
    dt <- visa_tabell_dold(ut, heltal = setdiff(names(ut), "År"), dom = "t", ordering = FALSE)
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
    # Utsnittet utvidgas med en marginal så att hela länet får plats (zoom och panorering går att ändra)
    dx <- ram$xmax - ram$xmin; dy <- ram$ymax - ram$ymin
    ram$xmin <- ram$xmin - 0.05 * dx; ram$xmax <- ram$xmax + 0.05 * dx
    ram$ymin <- ram$ymin - 0.05 * dy; ram$ymax <- ram$ymax + 0.05 * dy
    valt <- karta_distrikt[karta_distrikt$omrade == valt_omrade(), ]
    k <- leaflet(options = leafletOptions(minZoom = 5, zoomSnap = 0.1)) |>
      # Ljus bakgrundskarta (rdshinyappar >= 0.0.0.9005): bara ortnamn, tonade. Fler orter syns när man zoomar in.
      lagg_till_bakgrundskarta(etiketter = "orter", etikett_opacitet = 0.6) |>
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
    # Summa är NA (–) om någon åldersgrupp är dold
    if (length(summakolumner)) ut$Summa <- apply(df[summakolumner], 1, summa_strikt)
    visa_tabell_dold(ut, heltal = setdiff(names(ut), "Rad"))
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
  # Färger för raderna i ett block (näringsgren eller sektor) ur sysselsättningstabellen.
  # Används av diagrammen över förvärvsarbetande och av företagsdiagrammen, så att en rad har samma färg överallt.
  rad_farger_block <- function(block) {
    df <- syssurval("sysselsattning_kon", block)
    summa <- rowSums(df[c("man", "kvinnor")], na.rm = TRUE)      # bara för färgordning; dolda räknas som 0
    rader <- df$etikett[order(-summa)]
    farger <- setNames(rep_len(kat_farger, length(rader)), rader)
    # Näringsgrenar: färgkodning ur Bransch_Gxx_farger.xlsx; rader som inte går att koppla behåller sin färg
    if (identical(block, "naringsgren")) {
      bf <- bransch_farg_for(rader)
      farger[!is.na(bf)] <- bf[!is.na(bf)]
    }
    farger
  }
  rad_farger <- reactive({
    req(input$vis_syss %in% visaval)
    rad_farger_block(input$syss_block)
  })
  # Färg per etikett för företagsdiagram: samma färg som raden har i diagrammen över förvärvsarbetande.
  # Näringsgrenar får färg ur färgtabellen, sektorer ur sektorblockets färger; övrigt grått.
  farger_for_etiketter <- function(etiketter) {
    etiketter <- as.character(etiketter)
    f <- setNames(bransch_farg_for(etiketter), etiketter)
    alla <- c(tryCatch(rad_farger_block("naringsgren"), error = function(e) NULL),
              tryCatch(rad_farger_block("sektor"), error = function(e) NULL))
    saknas <- is.na(f)
    f[saknas] <- unname(alla[etiketter[saknas]])
    f[is.na(f)] <- "#969696"
    f[!duplicated(names(f))]
  }
  # Färger för en uppsättning rader; rader som saknas i färgtabellen blir grå
  farg_for <- function(rader) {
    f <- rad_farger()[rader]
    f[is.na(f)] <- "#969696"
    f[rader == dolt_etikett] <- dolt_farg
    f[rader == delvis_etikett] <- delvis_farg
    setNames(unname(f), rader)
  }
  fordelning_data <- reactive({
    req(input$vis_fordelning %in% c("Åldersgrupp", "Kön"))
    if (input$vis_fordelning == "Åldersgrupp") {
      df <- syss_alder_urval()
      validate(need(nrow(df) > 0 && length(summakolumner) > 0, "Uppgift saknas för valt urval."))
      lang <- df[summakolumner] |>
        mutate(etikett = df$etikett) |>
        pivot_longer(-etikett, names_to = "stapel", values_to = "antal") |>
        mutate(stapel = factor(stapel, levels = summakolumner, labels = snygg_alder(summakolumner)))
      # Bortfall per åldersgrupp: totalen minus de synliga raderna, som en egen kategori
      tot <- total_valt_ar("sysselsattning_alder")
      if (!is.null(tot)) {
        extra <- do.call(rbind, lapply(summakolumner, function(k) {
          r <- dolt_rest(as.numeric(tot[[k]]), df[[k]])
          if (is.finite(r)) data.frame(etikett = dolt_etikett, stapel = snygg_alder(k), antal = r, stringsAsFactors = FALSE)
        }))
        if (!is.null(extra)) {
          extra$stapel <- factor(extra$stapel, levels = levels(lang$stapel))
          lang <- rbind(as.data.frame(lang), extra)
        }
      }
      lang
    } else {
      df <- syss_kon_urval()
      validate(need(nrow(df) > 0, "Uppgift saknas för valt urval."))
      lang <- data.frame(etikett = df$etikett, Kvinnor = df$kvinnor, Män = df$man,
                         Totalt = plus_strikt(df$man, df$kvinnor), check.names = FALSE) |>
        pivot_longer(-etikett, names_to = "stapel", values_to = "antal") |>
        mutate(stapel = factor(stapel, levels = c("Totalt", "Män", "Kvinnor")))
      tot <- total_valt_ar("sysselsattning_kon")
      if (!is.null(tot)) {
        totaler <- c(Kvinnor = tot$kvinnor, Män = tot$man, Totalt = plus_strikt(tot$man, tot$kvinnor))
        delar <- list(Kvinnor = df$kvinnor, Män = df$man, Totalt = plus_strikt(df$man, df$kvinnor))
        extra <- do.call(rbind, lapply(names(totaler), function(k) {
          r <- dolt_rest(as.numeric(totaler[[k]]), delar[[k]])
          if (is.finite(r)) data.frame(etikett = dolt_etikett, stapel = k, antal = r, stringsAsFactors = FALSE)
        }))
        if (!is.null(extra)) {
          extra$stapel <- factor(extra$stapel, levels = levels(lang$stapel))
          lang <- rbind(as.data.frame(lang), extra)
        }
      }
      lang
    }
  })
  output$fordelning_diagram <- renderGirafe({
    d <- fordelning_data()
    d$antal[is.na(d$antal)] <- 0
    ordning <- names(rad_farger())
    ordning <- okand_sist(c(ordning[ordning %in% d$etikett], setdiff(setdiff(unique(d$etikett), ordning), dolt_etikett)))
    if (dolt_etikett %in% d$etikett) ordning <- c(ordning, dolt_etikett)      # bortfallet sist
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
                           "Dolda värden visas sammanlagt som en grå stapeldel.", appdata$kalla))
    girafe_alt(ggobj = p, width_svg = 19, height_svg = 3.2 + 0.42 * ceiling(length(ordning) / 3),
               options = giraf_val)
  })
  # Staplar med näringsgrenens namn vid axeln. Webbläsarens typsnitt (Poppins) är bredare än det
  # textmått diagrammet räknar med, så texten får ett fast avstånd till staplarna (margin r) och
  # radbryts tidigt, vilket håller överskjutet litet.
  output$syss_kon_diagram <- renderGirafe({
    df <- syss_kon_urval()
    validate(need(nrow(df) > 0, "Sysselsättningsuppgift saknas för valt urval."))
    df$summa <- plus_strikt(df$man, df$kvinnor)
    tot <- total_valt_ar("sysselsattning_kon")
    # Bortfall: det som inte syns bland raderna, en egen stapel längst ned
    bf <- if (is.null(tot)) list(dolt = NA_real_, delvis = NA_real_) else bortfall_total(tot, df)
    rest_kv <- if (is.null(tot)) NA_real_ else dolt_rest(tot$kvinnor, df$kvinnor)
    rest_man <- if (is.null(tot)) NA_real_ else dolt_rest(tot$man, df$man)
    ordning <- df$etikett[order(df$summa, na.last = FALSE)]          # dolda rader längst ned
    ordning <- rev(okand_sist(rev(ordning)))                         # Okänd nedanför övriga rader
    if (any(is.finite(c(bf$dolt, bf$delvis, rest_kv, rest_man)))) {
      # Längst ned: dolda värden, därefter (bara i totalvyn) rader med delvis dolt kön
      ordning <- c(dolt_etikett, if (!delat(input$vis_syss)) delvis_etikett, ordning)
    }
    titel <- paste("Förvärvsarbetande –", tolower(blocketikett(input$syss_block)), valt_ar())
    bildtext <- paste("Rader kan överlappa; summera inte över rader. Dolda värden visas sammanlagt i en grå stapel. I totalvyn visas även synliga personer på rader där bara ena könet är dolt som en egen, mörkare grå stapel.", appdata$kalla)
    farger <- farg_for(unique(ordning))
    if (delat(input$vis_syss)) {
      df <- df |>
        select(etikett, man, kvinnor) |>
        pivot_longer(c(man, kvinnor), names_to = "kon", values_to = "antal")
      if (is.finite(rest_kv)) df <- rbind(df, data.frame(etikett = dolt_etikett, kon = "kvinnor", antal = rest_kv))
      if (is.finite(rest_man)) df <- rbind(df, data.frame(etikett = dolt_etikett, kon = "man", antal = rest_man))
      df <- df |>
        mutate(kon = factor(kon, levels = c("kvinnor", "man"), labels = kon_ordning),
               etikett = factor(etikett, levels = unique(ordning)))
      df$popup <- paste0(df$etikett, "<br/>", df$kon, ": ", vt(df$antal), " personer")
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
      if (is.finite(bf$dolt)) df <- rbind(df[c("etikett", "summa")], data.frame(etikett = dolt_etikett, summa = bf$dolt))
      if (is.finite(bf$delvis)) df <- rbind(df[c("etikett", "summa")], data.frame(etikett = delvis_etikett, summa = bf$delvis))
      df$etikett <- factor(df$etikett, levels = unique(ordning))
      df$popup <- paste0(df$etikett, "<br/>", vt(df$summa), " personer")
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
    farger <- farg_for(c(df$etikett, dolt_etikett, delvis_etikett))
    tot <- total_valt_ar("sysselsattning_kon")
    if (delat(input$vis_syss)) {
      ut <- rbind(
        data.frame(bransch = df$etikett, antal = df$kvinnor, kon = kon_ordning[1], stringsAsFactors = FALSE),
        data.frame(bransch = df$etikett, antal = df$man, kon = kon_ordning[2], stringsAsFactors = FALSE))
      # Bortfallet som en egen grå cirkel per kön
      if (!is.null(tot)) {
        r_kv <- dolt_rest(tot$kvinnor, df$kvinnor); r_man <- dolt_rest(tot$man, df$man)
        if (is.finite(r_kv)) ut <- rbind(ut, data.frame(bransch = dolt_etikett, antal = r_kv, kon = kon_ordning[1]))
        if (is.finite(r_man)) ut <- rbind(ut, data.frame(bransch = dolt_etikett, antal = r_man, kon = kon_ordning[2]))
      }
      grupp <- "kon"
    } else {
      summa_rad <- plus_strikt(df$man, df$kvinnor)
      ut <- data.frame(bransch = df$etikett, antal = summa_rad, stringsAsFactors = FALSE)
      if (!is.null(tot)) {
        bf <- bortfall_total(tot, df)
        if (is.finite(bf$dolt)) ut <- rbind(ut, data.frame(bransch = dolt_etikett, antal = bf$dolt))
        if (is.finite(bf$delvis)) ut <- rbind(ut, data.frame(bransch = delvis_etikett, antal = bf$delvis))
      }
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
        titel = paste("Sysselsatta per", tolower(blocketikett(input$syss_block))),   # t.ex. "Sysselsatta per sektor"
        diagram_caption = appdata$kalla),
      error = function(e) validate(paste("Cirkeldiagrammet kunde inte ritas:", conditionMessage(e))))
    # Funktionen sorterar legenden alfabetiskt. Här ersätts färgskalan så att förklaringsrutorna
    # ligger i storleksordning (störst totalt antal först, kön sammanräknade), med samma
    # färger och radbrytning som i funktionen.
    ordning <- okand_sist(names(sort(tapply(ut$antal, ut$bransch, sum), decreasing = TRUE)))
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
      data.frame(Rad = df$etikett, Totalt = plus_strikt(df$man, df$kvinnor),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }
    visa_tabell_dold(ut, heltal = setdiff(names(ut), "Rad"))
  })

  # ===========================================================================
  # Tabeller med röjandegranskning: civilstånd, sammanräknad förvärvsinkomst, hushållsstorlek,
  # konsumtion samt företag. Dolda värden är NA i tabellerna och visas som "–" (orsaken visas inte).
  # Staplar för dolda värden utelämnas, och inga summor räknas ut över rader i appen.
  # ===========================================================================
  nytt_urval <- function(tabell) {
    req(!is.null(appdata[[tabell]]))
    d <- appdata[[tabell]] |> filter(omrade == valt_omrade(), ar == valt_ar())
    if ("kon" %in% names(d)) d$kon <- normalisera_kon(d$kon)
    d
  }
  dolt_not <- "– betyder att uppgiften inte visas."
  # Staplar för ett mått per rad (rader sorterade som i data, eller efter värde när sortera = TRUE)
  ny_staplar <- function(df, etikett, varde, titel, y_titel, enhet = "", vandra = FALSE, sortera = FALSE, farger = NULL) {
    df <- df[is.finite(df[[varde]]), , drop = FALSE]
    nivaer <- unique(as.character(df[[etikett]]))
    if (sortera) nivaer <- as.character(df[[etikett]])[order(df[[varde]])]
    # Okänd sist i läsordningen. Vid liggande staplar (coord_flip) ritas första nivån nederst, så där läggs Okänd först.
    nivaer <- if (vandra) rev(okand_sist(rev(nivaer))) else okand_sist(nivaer)
    df$x <- factor(as.character(df[[etikett]]), levels = nivaer)
    df$y <- df[[varde]]
    df$popup <- paste0(df$x, "<br/>", vt(df$y), enhet)
    p <- ggplot(df, aes(x, y)) +
      (if (is.null(farger)) geom_col_interactive(aes(tooltip = popup, data_id = x), fill = rd_farg$primar)
       else geom_col_interactive(aes(tooltip = popup, data_id = x, fill = x))) +
      (if (!is.null(farger)) scale_fill_manual(values = farger, guide = "none")) +
      scale_y_continuous(labels = siffror, expand = expansion(mult = c(0, 0.08))) +
      tema_app() +
      labs(title = titel, x = NULL, y = y_titel,
           caption = paste0(appdata$kalla, ". Staplar för dolda värden utelämnas."))
    if (vandra) p + coord_flip() else p + theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }

  # ---- Befolkning: civilstånd (personer 16 år och äldre) ----
  civil_lang <- reactive({
    d <- nytt_urval("flytt_civilstand")
    kol <- setdiff(names(d), c("ar", "omrade", "kon"))
    d |> pivot_longer(all_of(kol), names_to = "civil", values_to = "antal") |>
      mutate(civil = factor(civil, levels = c(intersect(names(civil_etiketter), kol), setdiff(kol, names(civil_etiketter)))),
             civil_text = ifelse(as.character(civil) %in% names(civil_etiketter),
                                 unname(civil_etiketter[as.character(civil)]), etikettera_rad(civil)))
  })
  output$civil_diagram <- renderSvg(bredd = 9.5, hojd = 3.6, {
    d <- civil_lang()
    d <- if (delat(input$vis_civil)) filter(d, kon %in% kon_ordning) else filter(d, kon == "Totalt")
    validate(need(nrow(d) > 0 && any(is.finite(d$antal)), "Civilstånd saknas för valt urval."))
    d$civil_text <- factor(d$civil_text, levels = unique(d$civil_text[order(d$civil)]))
    d$popup <- paste0(d$civil_text, "<br/>", if (delat(input$vis_civil)) paste0(d$kon, ": ") else "", vt(d$antal), " personer")
    p <- if (delat(input$vis_civil)) {
      d$kon <- factor(d$kon, levels = kon_ordning)
      ggplot(d, aes(civil_text, antal, fill = kon)) +
        geom_col_interactive(aes(tooltip = popup, data_id = paste(civil_text, kon)),
                             position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
        scale_fill_manual(values = kon_farger, name = NULL)
    } else {
      ggplot(d, aes(civil_text, antal)) +
        geom_col_interactive(aes(tooltip = popup, data_id = civil_text), fill = rd_farg$primar, na.rm = TRUE)
    }
    p + scale_y_continuous(labels = siffror, expand = expansion(mult = c(0, 0.08))) +
      tema_app() + theme(legend.position = if (delat(input$vis_civil)) "bottom" else "none") +
      labs(title = paste("Civilstånd, personer 18 år och äldre", valt_ar()), x = NULL, y = "Antal personer",
           caption = paste0(appdata$kalla, ". Staplar för dolda värden utelämnas."))
  })
  output$civil_tabell <- renderDT({
    if (is.null(appdata$flytt_civilstand)) return(visa_tabell(tom_tabell()))
    d <- civil_lang() |> filter(kon %in% c("Totalt", kon_ordning)) |>
      select(civil, civil_text, kon, antal) |>
      pivot_wider(names_from = kon, values_from = antal)
    d <- d[order(d$civil), ]
    ut <- data.frame(Civilstånd = d$civil_text, check.names = FALSE, stringsAsFactors = FALSE)
    for (k in intersect(c("Totalt", kon_ordning), names(d))) ut[[k]] <- d[[k]]
    visa_tabell_dold(ut, heltal = setdiff(names(ut), "Civilstånd"), dom = "t", ordering = FALSE, pageLength = 50)
  })

  # ---- Inkomst och ekonomi: sammanräknad förvärvsinkomst per åldersgrupp ----
  samink_urval <- reactive({
    req(input$vis_samink %in% visaval)
    d <- nytt_urval("sammanraknad_forv_inkomst")
    d <- if (delat(input$vis_samink)) filter(d, kon %in% kon_ordning) else filter(d, kon == "Totalt")
    d$aldersgrupp <- factor(d$aldersgrupp, levels = sortera_naturligt(appdata$sammanraknad_forv_inkomst$aldersgrupp))
    d |> arrange(aldersgrupp, kon) |> mutate(aldersgrupp = as.character(aldersgrupp))
  })
  samink_matt <- c("Medelinkomst" = "medel_ink", "Medianinkomst" = "median_ink", "Antal personer" = "antal_ind")
  output$samink_diagram <- renderSvg(bredd = 9.5, hojd = 3.8, {
    req(input$samink_matt %in% unname(samink_matt))
    d <- samink_urval()
    m <- input$samink_matt
    validate(need(nrow(d) > 0 && any(is.finite(d[[m]])),
                  "Uppgift saknas för valt urval. Totalt finns bara om totalrader ingår i tabellen."))
    enhet <- if (m == "antal_ind") " personer" else " kr"
    d$aldersgrupp <- factor(d$aldersgrupp, levels = unique(d$aldersgrupp))
    d$popup <- paste0(d$aldersgrupp, "<br/>", if (delat(input$vis_samink)) paste0(d$kon, ": ") else "", vt(d[[m]]), enhet)
    p <- if (delat(input$vis_samink)) {
      d$kon <- factor(d$kon, levels = kon_ordning)
      ggplot(d, aes(aldersgrupp, .data[[m]], fill = kon)) +
        geom_col_interactive(aes(tooltip = popup, data_id = paste(aldersgrupp, kon)),
                             position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
        scale_fill_manual(values = kon_farger, name = NULL)
    } else {
      ggplot(d, aes(aldersgrupp, .data[[m]])) +
        geom_col_interactive(aes(tooltip = popup, data_id = aldersgrupp), fill = rd_farg$primar, na.rm = TRUE)
    }
    p + scale_x_discrete(drop = FALSE) + scale_y_continuous(labels = siffror, expand = expansion(mult = c(0, 0.08))) +
      tema_app() + theme(legend.position = if (delat(input$vis_samink)) "bottom" else "none") +
      labs(title = paste(names(samink_matt)[match(m, samink_matt)], "– sammanräknad förvärvsinkomst", valt_ar()),
           x = NULL, y = if (m == "antal_ind") "Antal personer" else "Kronor",
           caption = paste0(appdata$kalla, ". Staplar för dolda värden utelämnas."))
  })
  output$samink_tabell <- renderDT({
    if (is.null(appdata$sammanraknad_forv_inkomst)) return(visa_tabell(tom_tabell()))
    d <- samink_urval()
    ut <- data.frame(Åldersgrupp = d$aldersgrupp, check.names = FALSE, stringsAsFactors = FALSE)
    if (delat(input$vis_samink)) ut$Kön <- d$kon
    kol <- c(Antal = "antal_ind", Median = "median_ink", Summa = "summa_forvink", Medel = "medel_ink")
    for (k in names(kol)) if (kol[[k]] %in% names(d)) ut[[k]] <- d[[kol[[k]]]]
    visa_tabell_dold(ut, heltal = intersect(names(kol), names(ut)), dom = "t", ordering = FALSE, pageLength = 50)
  })

  # ---- Inkomst och ekonomi: hushållens storlek ----
  hushallsstorlek_urval <- reactive({
    d <- nytt_urval("storlek_hushall")
    d[order(suppressWarnings(as.numeric(d$hushallsstorlek)), d$hushallsstorlek), ]
  })
  output$hushallsstorlek_diagram <- renderSvg(bredd = 9.5, hojd = 3.4, {
    d <- hushallsstorlek_urval()
    validate(need(nrow(d) > 0 && any(is.finite(d$storlek_hushall)), "Uppgift om hushållens storlek saknas för valt urval."))
    ny_staplar(d, "hushallsstorlek", "storlek_hushall", paste("Antal hushåll efter antal personer i hushållet", valt_ar()),
               "Antal hushåll", " hushåll")
  })
  output$hushallsstorlek_tabell <- renderDT({
    if (is.null(appdata$storlek_hushall)) return(visa_tabell(tom_tabell()))
    d <- hushallsstorlek_urval()
    visa_tabell_dold(data.frame(`Personer i hushållet` = d$hushallsstorlek, `Antal hushåll` = d$storlek_hushall, check.names = FALSE),
                     heltal = "Antal hushåll", dom = "t", ordering = FALSE, pageLength = 50)
  })

  # ---- Inkomst och ekonomi: konsumtionskostnader (Konsumentverket) ----
  kons_namn <- c(hushallsstorlek = "Personer i hushållet", aldersgrupp = "Åldersgrupp", kon = "Kön",
                 storlek_hushall = "Antal hushåll", antal_ind = "Antal personer",
                 kostnad_per_manad = "Kostnad per hushåll och månad (kr)", kostnad_per_ar = "Kostnad per hushåll och år (kr)",
                 kostnad_per_manad_tot = "Total kostnad per månad (kr)", kostnad_per_ar_tot = "Total kostnad per år (kr)",
                 mat_per_manad = "Mat per person och månad (kr)", mat_per_ar = "Mat per person och år (kr)",
                 hygien_per_manad = "Hygien per person och månad (kr)", hygien_per_ar = "Hygien per person och år (kr)",
                 mat_per_manad_tot = "Total mat per månad (kr)", mat_per_ar_tot = "Total mat per år (kr)",
                 hygien_per_manad_tot = "Total hygien per månad (kr)", hygien_per_ar_tot = "Total hygien per år (kr)")
  kons_tabell <- function(d) {
    d <- d[setdiff(names(d), c("ar", "omrade"))]
    names(d) <- ifelse(names(d) %in% names(kons_namn), unname(kons_namn[names(d)]), gsub("_", " ", names(d)))
    visa_tabell_dold(d, heltal = names(d)[vapply(d, is.numeric, logical(1))], dom = "t", ordering = FALSE, pageLength = 50)
  }
  output$kons_hus_tabell <- renderDT({
    if (is.null(appdata$konsumtion_hus)) return(visa_tabell(tom_tabell()))
    d <- nytt_urval("konsumtion_hus")
    kons_tabell(d[order(suppressWarnings(as.numeric(d$hushallsstorlek)), d$hushallsstorlek), ])
  })
  output$kons_ind_tabell <- renderDT({
    if (is.null(appdata$konsumtion_ind)) return(visa_tabell(tom_tabell()))
    d <- nytt_urval("konsumtion_ind")
    req(input$vis_kons %in% visaval)
    if ("kon" %in% names(d)) d <- if (delat(input$vis_kons)) filter(d, kon %in% kon_ordning) else filter(d, kon == "Totalt")
    if (!delat(input$vis_kons)) d <- d[setdiff(names(d), "kon")]
    d$aldersgrupp <- factor(d$aldersgrupp, levels = sortera_naturligt(appdata$konsumtion_ind$aldersgrupp))
    d <- d[order(d$aldersgrupp), ]
    d$aldersgrupp <- as.character(d$aldersgrupp)
    kons_tabell(d)
  })

  # ---- Näringsliv: företag (arbetsställen, anställda, omsättning) ----
  ftg_klassordning <- c("0", "1-4", "5-9", "10-19", "20 +", "okand")
  antal_anst_urval <- reactive({
    d <- nytt_urval("antal_anstallda")
    d$klass <- as.character(d$klass)
    d[order(match(d$klass, c(ftg_klassordning, "totalt"), nomatch = 99)), ]
  })
  ftg_etikett <- function(x) ifelse(x == "okand", "Okänd", ifelse(x == "totalt", "Totalt", x))
  output$antal_anst_diagram <- renderSvg(bredd = 9.5, hojd = 3.4, {
    d <- antal_anst_urval() |> filter(klass != "totalt")
    validate(need(nrow(d) > 0 && any(is.finite(d$antal_arbetsstallen)), "Uppgift om arbetsställen saknas för valt urval."))
    d$klass <- ftg_etikett(d$klass)
    ny_staplar(d, "klass", "antal_arbetsstallen", paste("Arbetsställen efter antal anställda", valt_ar()),
               "Antal arbetsställen", " arbetsställen")
  })
  output$antal_anst_tabell <- renderDT({
    if (is.null(appdata$antal_anstallda)) return(visa_tabell(tom_tabell()))
    d <- antal_anst_urval()
    visa_tabell_dold(data.frame(`Antal anställda` = ftg_etikett(d$klass), Arbetsställen = d$antal_arbetsstallen, check.names = FALSE),
                     heltal = "Arbetsställen", dom = "t", ordering = FALSE, pageLength = 50)
  })

  ftg_matt <- c("Antal arbetsställen" = "antal_arbetsstallen", "Anställda" = "anstallda")
  bransch_urval <- reactive({
    d <- nytt_urval("foretag_bransch")
    d$bransch <- as.character(d$bransch)
    d
  })
  output$bransch_diagram <- renderSvg(bredd = 9.5, hojd = 5.2, {
    req(input$bransch_matt %in% unname(ftg_matt))
    m <- input$bransch_matt
    d <- bransch_urval() |> filter(bransch != "totalt")
    validate(need(nrow(d) > 0 && any(is.finite(d[[m]])), "Uppgift saknas för valt urval."))
    d$bransch <- etikettera_rad(d$bransch)
    ny_staplar(d, "bransch", m, paste(names(ftg_matt)[match(m, ftg_matt)], "per näringsgren", valt_ar()),
               names(ftg_matt)[match(m, ftg_matt)], vandra = TRUE, sortera = TRUE,
               farger = farger_for_etiketter(d$bransch))      # samma färger som i diagrammen över förvärvsarbetande
  })
  output$bransch_tabell <- renderDT({
    if (is.null(appdata$foretag_bransch)) return(visa_tabell(tom_tabell()))
    d <- bransch_urval()
    d <- d[order(d$bransch == "totalt", d$bransch), ]
    ut <- data.frame(Näringsgren = etikettera_rad(d$bransch), Arbetsställen = d$antal_arbetsstallen,
                     Anställda = d$anstallda, check.names = FALSE, stringsAsFactors = FALSE)
    visa_tabell_dold(ut, heltal = c("Arbetsställen", "Anställda"), dom = "t", ordering = FALSE, pageLength = 50)
  })

  handel_matt <- c("Antal arbetsställen" = "antal_arbetsstallen", "Anställda" = "anstallda", "Omsättning (tkr)" = "omsattning_tkr")
  handel_urval <- reactive(nytt_urval("handel") |> mutate(grupp = as.character(grupp)))
  output$handel_diagram <- renderSvg(bredd = 9.5, hojd = 3.6, {
    req(input$handel_matt %in% unname(handel_matt))
    m <- input$handel_matt
    d <- handel_urval()
    validate(need(nrow(d) > 0 && any(is.finite(d[[m]])), "Uppgift saknas för valt urval."))
    d$grupp <- etikettera_rad(d$grupp)
    ny_staplar(d, "grupp", m, paste(names(handel_matt)[match(m, handel_matt)], "– handel och restaurang", valt_ar()),
               names(handel_matt)[match(m, handel_matt)], vandra = TRUE)
  })
  output$handel_tabell <- renderDT({
    if (is.null(appdata$handel)) return(visa_tabell(tom_tabell()))
    d <- handel_urval()
    ut <- data.frame(Grupp = etikettera_rad(d$grupp), Arbetsställen = d$antal_arbetsstallen, Anställda = d$anstallda,
                     `Omsättning (tkr)` = d$omsattning_tkr, check.names = FALSE, stringsAsFactors = FALSE)
    visa_tabell_dold(ut, heltal = setdiff(names(ut), "Grupp"), dom = "t", ordering = FALSE, pageLength = 50)
  })

  # ---- Notiser om dolda värden -----------------------------------------------
  # Visas under ett diagram eller en tabell när något värde saknas (– eller utelämnad stapel). Orsaken
  # (för få personer) är densamma överallt och visas inte per cell.
  notis_text <- "– betyder att uppgiften inte visas eftersom antalet är för litet. En stapel som saknas har samma orsak."
  har_dolda <- function(df, kol = NULL) {
    if (is.null(df) || !nrow(df)) return(FALSE)
    if (is.null(kol)) kol <- names(df)[vapply(df, is.numeric, logical(1))]
    kol <- setdiff(intersect(kol, names(df)), c("ar", "rang"))
    length(kol) > 0 && anyNA(unlist(df[kol]))
  }
  notis <- function(fn) renderUI({
    har <- tryCatch(isTRUE(fn()), error = function(e) FALSE)
    if (har) div(class = "rd-notis", notis_text)
  })
  output$notis_flytt_serie <- notis(function() har_dolda(befolkning_urval("Totalt")))
  output$notis_flytt_alder_oversikt <- notis(function() har_dolda(flytt_alder_kon(), "flyttnetto"))
  output$notis_flytt_alder_tabell <- notis(function() {
    d <- flytt_kon_urval(input$vis_flytt_tabell)
    # Totalraden är dold om någon åldersgrupp är dold, så en dold cell räcker
    m <- if (identical(input$vy_flytt_tab, "Alla år") && input$flytt_tabell_matt %in% names(d))
      input$flytt_tabell_matt else intersect(c("inflyttad", "utflyttad", "flyttnetto"), names(d))
    har_dolda(if (identical(input$vy_flytt_tab, "Alla år")) d else d[d$ar == valt_ar(), , drop = FALSE], m)
  })
  output$notis_civil <- notis(function() har_dolda(civil_lang(), "antal"))
  output$notis_utb <- notis(function() {
    d <- utbildningsurval()
    har_dolda(d, if (utb_andel() && utb_finns_andel()) utb_kolumner() else c(names(utbildningsnivaer), "totalt"))
  })
  output$notis_pend <- notis(function() har_dolda(pend_urval(input$vis_pend)))
  output$notis_arbm_alder <- notis(function() {
    d <- total_valt_ar("sysselsattning_alder"); !is.null(d) && anyNA(unlist(d[summakolumner]))
  })
  output$notis_arbm_kon <- notis(function() har_dolda(arbm_kon_data(), c("man", "kvinnor", "totalt")))
  output$notis_syss_alder <- notis(function() har_dolda(syss_alder_urval(), summakolumner))
  output$notis_syss_kon <- notis(function() har_dolda(syss_kon_urval(), c("man", "kvinnor")))
  output$notis_ink <- notis(function() har_dolda(inkomsturval(), c("antal_ind", "median_ink", "medel_ink")))
  output$notis_hushall <- notis(function() har_dolda(hushallurval(), c("antal_hus", "medel_ink", "median_ink")))
  output$notis_ekstandard <- notis(function() har_dolda(ekstandardurval(), c("antal_hus", "tot_hus", "andel")))
  output$notis_samink <- notis(function() har_dolda(samink_urval(), c("antal_ind", "median_ink", "medel_ink", "summa_forvink")))
  output$notis_hushallsstorlek <- notis(function() har_dolda(hushallsstorlek_urval(), "storlek_hushall"))
  output$notis_kons <- notis(function() har_dolda(nytt_urval("konsumtion_hus")) || har_dolda(nytt_urval("konsumtion_ind")))
  output$notis_antal_anst <- notis(function() har_dolda(antal_anst_urval(), "antal_arbetsstallen"))
  output$notis_bransch <- notis(function() har_dolda(bransch_urval(), c("antal_arbetsstallen", "anstallda")))
  output$notis_handel <- notis(function() har_dolda(handel_urval(), c("antal_arbetsstallen", "anstallda", "omsattning_tkr")))

  # ---- Jämför på karta --------------------------------------------------------
  # Distrikten färgsätts efter värdet på valt mått (valt år). Dolda eller saknade värden är grå.
  # Alla mått är medelvärden, medianer, andelar eller kvoter (se kartindikatorer i global.R).
  # Värdena räknas med NA-säkra operationer: är någon ingående cell dold blir resultatet NA.
  kart_varden <- reactive({
    req(input$kart_indikator %in% unlist(kartindikatorer))
    a <- valt_ar(); id <- input$kart_indikator
    alla <- appdata$omraden["omrade"]
    ut <- switch(id,
                 k_medel_ink = , k_median_ink = {
                   validate(need(!is.null(appdata$hushall_inkomst), "Tabellen hushall_inkomst saknas."))
                   d <- appdata$hushall_inkomst; d <- d[d$ar == a, , drop = FALSE]
                   data.frame(omrade = d$omrade, varde = d[[sub("^k_", "", id)]])
                 },
                 k_flyttnetto_per = {
                   d <- appdata$flytt_summering; d <- d[d$ar == a, , drop = FALSE]
                   data.frame(omrade = d$omrade, varde = ifelse(d$befolkning > 0, d$flyttnetto / d$befolkning * 1000, NA_real_))
                 },
                 k_bef_forandring = {
                   validate(need(!is.null(appdata$flytt_serie_ar), "Tabellen flytt_serie_ar saknas."))
                   d <- appdata$flytt_serie_ar
                   fg <- d[d$ar == a - 1, c("omrade", "befolkning")]; names(fg)[2] <- "foregaende"
                   nu <- d[d$ar == a, c("omrade", "befolkning")]
                   m <- merge(nu, fg, by = "omrade")
                   data.frame(omrade = m$omrade, varde = ifelse(m$foregaende > 0, (m$befolkning / m$foregaende - 1) * 100, NA_real_))
                 },
                 k_dag_natt = {
                   d <- appdata$pendling
                   validate(need(!is.null(d) && all(c("dagbef", "nattbef") %in% names(d)),
                                 "Pendlingstabellen saknar dagbef och nattbef."))
                   d <- d[d$ar == a & d$kon == "Totalt", , drop = FALSE]   # Totalraden används, inga kön summeras ihop
                   data.frame(omrade = d$omrade, varde = ifelse(d$nattbef > 0, d$dagbef / d$nattbef * 100, NA_real_))
                 },
                 k_eftergym = {
                   d <- appdata$utbildning
                   validate(need("andel_eftergymnasial" %in% names(d),
                                 "Andel eftergymnasial saknas. Kör om roj_alla och läs in ale.utbildning på nytt."))
                   d <- d[d$ar == a & d$kon == "Totalt", , drop = FALSE]
                   v <- d$andel_eftergymnasial * 100                 # publicerad, röjandegranskad andel
                   f <- hamta_flaggor("utbildning")                  # rang_ok = FALSE: för litet underlag för jämförelse
                   if (!is.null(f) && "rang_ok" %in% names(f)) {
                     ok <- f$rang_ok[match(paste(d$ar, d$omrade, d$kon), paste(f$ar, f$omrade, f$kon))]
                     v[!(ok %in% TRUE)] <- NA_real_
                   }
                   data.frame(omrade = d$omrade, varde = v)
                 },
                 k_bil_elektrifierade = , k_bil_laddbara = , k_bil_el = {
                   validate(need(!is.null(appdata$bilar_distrikt), "Tabellen bilar_distrikt saknas."))
                   m <- sub("^k_bil_", "", id)
                   d <- appdata$bilar_distrikt; ar <- bilar_ar(); d <- d[d$ar == ar, , drop = FALSE]
                   kol <- intersect(paste0("personbilar_", bilar_matt_kolumner[[m]]), names(d))
                   alla_k <- intersect(bilar_kolumner, names(d))
                   # rowSums utan na.rm: NA om någon drivmedelsgrupp är dold
                   tot <- rowSums(d[alla_k]); del <- rowSums(d[kol])
                   data.frame(omrade = d$omrade, varde = ifelse(tot > 0, del / tot * 100, NA_real_))
                 })
    ut$varde[!is.finite(ut$varde)] <- NA_real_
    out <- merge(appdata$omraden[c("omrade", "namn")], ut, by = "omrade", all.x = TRUE)
    out$valt <- out$omrade == valt_omrade()
    out
  })
  kart_info <- reactive(kartindikator_info[[input$kart_indikator]])
  kart_titel <- reactive(names(unlist(kartindikatorer))[match(input$kart_indikator, unlist(kartindikatorer))] |>
                           sub(pattern = "^[^.]*\\.", replacement = ""))
  kart_format <- function(x, info) {
    ifelse(is.na(x), "–", paste(formatC(x, format = "f", digits = info$dec, big.mark = " ", decimal.mark = ","),
                                if (info$enhet == "%") "%" else info$enhet))
  }
  # Palett: sammanhängande blå gradient; om värdena går över noll (flyttnetto, förändring) används en
  # skala som divergerar kring noll (orange under, blå över), så att tillväxt och minskning syns.
  # Bakgrundskartan är ljusgrå, så skalorna börjar vid en tydligt blå ton (inte nästan vit) och slutar
  # i mörkt blått. Saknade värden får en mellangrå ton som syns mot bakgrunden men inte läses som en nivå.
  kart_na_farg <- "#a6a6a6"
  kart_gransfarg <- "#4a4a4a"
  kart_palett <- function(v, reverse = FALSE) {
    v <- v[is.finite(v)]
    if (!length(v)) return(NULL)
    if (min(v) < 0 && max(v) > 0) {
      m <- max(abs(v))
      colorNumeric(c("#d98c1f", "#f4d7a1", "#fbfbfb", "#8cc3dc", "#0b5f82"), domain = c(-m, m), na.color = kart_na_farg, reverse = reverse)
    } else {
      colorNumeric(c("#a8d8ea", "#3d9cc0", "#0b5f82", "#06384f"), domain = range(v), na.color = kart_na_farg, reverse = reverse)
    }
  }
  output$jamfor_kartstatus <- renderUI({
    d <- kart_varden()
    n_dolt <- sum(is.na(d$varde))
    if (n_dolt == nrow(d)) {
      div(class = "alert alert-warning", "Inga värden finns för valt år och mått.")
    } else if (n_dolt > 0) {
      div(class = "rd-notis", paste0(n_dolt, " av ", nrow(d), " distrikt saknar värde (dolt eller saknas) och är grå på kartan."))
    }
  })
  # Ritar distrikten (fyllning efter färg, tydlig mörk gräns) och valt distrikt med kraftig kantlinje
  # Genomskinlighet på distriktsfärgerna (1 = helt täckande). Första sidans karta har 0,35; här behövs mer
  # färg för att skalan ska gå att läsa, men bakgrundskartan ska synas igenom.
  kart_fyllning <- 0.65
  jamfor_rita_ytor <- function(karta, ytor, farg, etikett, valt) {
    karta <- clearGroup(clearGroup(karta, "jamfor"), "jamfor_valt")
    karta <- addPolygons(karta, data = ytor, layerId = paste0("j_", ytor$omrade), group = "jamfor",
                         fillColor = farg, fillOpacity = kart_fyllning, weight = 0.8, color = kart_gransfarg, opacity = 0.75,
                         label = etikett,
                         highlightOptions = highlightOptions(weight = 2, color = "#000000", bringToFront = FALSE),
                         options = pathOptions(pane = "pane_jamfor"))
    v <- ytor[ytor$omrade == valt, ]
    if (nrow(v) == 1) {
      karta <- addPolygons(karta, data = v, group = "jamfor_valt", fill = FALSE, weight = 3.5,
                           color = "#ffffff", opacity = 1, options = pathOptions(pane = "pane_jamfor_valt", interactive = FALSE))
      karta <- addPolygons(karta, data = v, group = "jamfor_valt", fill = FALSE, weight = 2,
                           color = rd_farg$bla_djup, opacity = 1, dashArray = NULL,
                           options = pathOptions(pane = "pane_jamfor_valt", interactive = FALSE))
    }
    karta
  }
  output$jamfor_karta <- renderLeaflet({
    req(!is.null(karta_distrikt))
    ram <- as.list(sf::st_bbox(karta_distrikt))
    dx <- ram$xmax - ram$xmin; dy <- ram$ymax - ram$ymin
    k <- leaflet(options = leafletOptions(minZoom = 5, zoomSnap = 0.1)) |>
      addMapPane("pane_jamfor", zIndex = 410) |>
      addMapPane("pane_jamfor_valt", zIndex = 420) |>
      lagg_till_bakgrundskarta(etiketter = "orter", etikett_opacitet = 0.6)
    # Distriktsgränser och valt distrikt ritas redan från start; färgerna läggs på av observeraren nedan
    k <- jamfor_rita_ytor(k, karta_distrikt, rep(kart_na_farg, nrow(karta_distrikt)),
                          paste0(karta_distrikt$namn, " (", karta_distrikt$omrade, ")"), isolate(valt_omrade()))
    fitBounds(k, ram$xmin - 0.05 * dx, ram$ymin - 0.05 * dy, ram$xmax + 0.05 * dx, ram$ymax + 0.05 * dy)
  })
  # Färgerna ritas om via proxy när mått, år eller valt distrikt ändras (kartans läge ändras inte)
  observe({
    req(!is.null(karta_distrikt))
    d <- kart_varden(); info <- kart_info()
    pal <- kart_palett(d$varde)
    ytor <- karta_distrikt
    ytor$varde <- d$varde[match(ytor$omrade, d$omrade)]
    ytor$etikett <- paste0(ytor$namn, " (", ytor$omrade, "): ", kart_format(ytor$varde, info))
    farg <- if (is.null(pal)) rep(kart_na_farg, nrow(ytor)) else pal(ytor$varde)
    p <- leafletProxy("jamfor_karta") |> clearControls()
    p <- jamfor_rita_ytor(p, ytor, farg, ytor$etikett, valt_omrade())
    # Teckenförklaring: skalan med högsta värdet överst (leaflet lägger annars lägsta överst), och
    # "Dolt eller saknas" som en egen liten ruta under skalan i stället för bredvid den.
    vf <- ytor$varde[is.finite(ytor$varde)]
    if (anyNA(ytor$varde)) {
      p <- addLegend(p, position = "bottomright", colors = kart_na_farg, labels = "Dolt eller saknas", opacity = 0.9)
    }
    if (!is.null(pal)) {
      enhet <- if (info$enhet == "%") "%" else info$enhet
      p <- addLegend(p, position = "bottomright", pal = kart_palett(d$varde, reverse = TRUE), values = vf, opacity = 0.9,
                     bins = 5,
                     title = paste0(kart_titel(), "<br/><small>", valt_ar(), ", ", enhet, "</small>"),
                     labFormat = labelFormat(big.mark = " ", digits = info$dec,
                                             transform = function(x) sort(x, decreasing = TRUE)))
    }
  })
  observeEvent(input$jamfor_karta_shape_click, {
    id <- sub("^j_", "", input$jamfor_karta_shape_click$id %||% "")
    if (id %in% appdata$omraden$omrade) updateSelectInput(session, "omrade", selected = id)
  })
  output$notis_jamfor_karta <- notis(function() anyNA(kart_varden()$varde))
})
