# Kort med rubrik (.rd-card i stilmallen). Första ospecifika textargumentet efter titeln blir
# undertext om det är en ren sträng; övriga argument läggs in som innehåll.
kort <- function(titel, ...) {
  inneh <- list(...)
  under <- NULL
  if (length(inneh) && is.character(inneh[[1]]) && length(inneh[[1]]) == 1) {
    under <- inneh[[1]]; inneh <- inneh[-1]
  }
  div(class = "rd-card rd-card--app",
      h2(titel),
      if (!is.null(under)) div(class = "rd-subtitle", under),
      inneh)
}

# Kort där samma data visas som diagram (först) eller tabell; knapparna byter vy.
# Kontroller som gäller båda vyerna (t.ex. val av kön) läggs i ... ovanför knapparna.
vykort <- function(titel, id, diagram, tabell, ..., flikar = c("Diagram", "Tabell")) {
  kort(titel, ...,
       tabsetPanel(id = id, type = "pills",
                   tabPanel(flikar[1], diagram),
                   tabPanel(flikar[2], tabell)))
}

shinyUI(
  fluidPage(
    title = APP_TITEL,
    tags$head(
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'regiondalarna_ruf.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'app.css'),
      tags$style(HTML("
        /* Mattare och ljusare OpenStreetMap-bakgrund så att distrikten syns tydligare */
        .mattad-bakgrund { filter: grayscale(60%) saturate(70%) brightness(108%) contrast(88%); }
        .karta-kontroll { background: #fff; border-radius: var(--rd-radius-sm); padding: 6px 10px;
          box-shadow: 0 1px 5px rgba(0,0,0,.4); font-family: var(--rd-font-sans); }
        .karta-kontroll__rad { display: flex; align-items: center; gap: 8px; margin: 0;
          font-weight: normal; font-size: var(--rd-fs-small); color: var(--rd-gray-dark); cursor: pointer; }
        .karta-kontroll input { accent-color: var(--rd-primary); }
        .karta-kontroll__rad + .karta-kontroll__rad { margin-top: 6px; }
        .karta-kontroll input[type=range] { width: 110px; margin: 0; cursor: pointer; }
        .karta-kontroll input[type=checkbox] { margin: 0; cursor: pointer; }
        .karta-kontroll__ikon { font-size: 16px; line-height: 1; }
        .rd-kpi-row .rd-kpi__value { font-size: 1.4rem; }
        .rd-card--app { margin-bottom: 16px; padding: 18px 20px; }
        .rd-card--app h2 { font-size: var(--rd-fs-h3); margin-top: 0; }
        .rd-card--app .rd-subtitle { margin-bottom: 12px; }
        .rd-card--app .form-group, .rd-card--app .shiny-options-group { margin-bottom: 8px; }
        .tab-content { padding-top: 16px; }
        /* Tabeller med könen som kolumngrupper: en tunn linje där varje grupp börjar */
        table.dataTable th.grupp-start, table.dataTable td.grupp-start { border-left: 1px solid #c9c9c9; }
        /* Rad där korten ska vara lika höga (startfliken: karta och flyttnetto) */
        .rd-rad-lika { display: flex; flex-wrap: wrap; }
        .rd-rad-lika:before, .rd-rad-lika:after { display: none; }
        .rd-rad-lika > [class*='col-'] { display: flex; float: none; }
        .rd-rad-lika > [class*='col-'] > .rd-card { flex: 1; width: 100%; }
        /* Kartan på startfliken fyller kortets återstående höjd (minst 520 px) */
        .rd-rad-lika .rd-card { display: flex; flex-direction: column; }
        .rd-rad-lika #karta { flex: 1 1 auto; height: auto !important; min-height: 520px; }
        /* Test: Poppins överallt (brödtext, inmatning, tabeller, diagram, kartor). Ta bort raden
           om stilmallen anger något annat typsnitt. Stilmallens variabel är reserv. */
        body, button, input, select, textarea, table.dataTable, .selectize-input, .selectize-dropdown {
          font-family: 'Poppins', var(--rd-font-sans), sans-serif; }
        /* Appens typsnitt i diagrammens text och i kartans etiketter */
        .girafe_container_std text { font-family: 'Poppins', var(--rd-font-sans), sans-serif !important; }
        .leaflet-container, .leaflet-tooltip, .leaflet-popup-content, .leaflet-container .legend,
        .leaflet-container .info { font-family: 'Poppins', var(--rd-font-sans), sans-serif; }
        /* Knappar för att växla mellan diagram och tabell i korten */
        .rd-card--app .nav-pills { margin-bottom: 12px; }
        .rd-card--app .nav-pills > li > a { padding: 4px 16px; margin-right: 6px; border-radius: 999px;
          border: 1px solid var(--rd-primary); color: var(--rd-primary); background: #fff;
          font-size: var(--rd-fs-small); line-height: 1.4; }
        .rd-card--app .nav-pills > li + li { margin-left: 0; }
        .rd-card--app .nav-pills > li > a:hover { background: var(--rd-primary-light, #eaf5f9); }
        .rd-card--app .nav-pills > li.active > a, .rd-card--app .nav-pills > li.active > a:hover,
        .rd-card--app .nav-pills > li.active > a:focus { background: var(--rd-primary); color: #fff; }
        .rd-card--app .tab-content { padding-top: 0; }
      "))
    ),

    # ---- Header (matchar .rd-header i regiondalarna_ruf.css) --------------
    tags$div(
      class = 'rd-header',
      tags$div(class = 'rd-header__title', APP_TITEL),
      tags$a(
        class  = 'rd-header__right',
        href   = 'https://www.regiondalarna.se',
        target = '_blank',
        tags$img(src = 'logo_liggande_fri_vit.png', alt = 'Region Dalarna')
      )
    ),

    # ---- Innehåll ---------------------------------------------------------
    div(
      class = "rd-info", role = "status",
      if (appdata$exempeldata)
        "Demonstrationsläge: alla värden och distriktsnamn är syntetiska, inte statistik om Dalarna."
      else paste("Källa:", appdata$kalla),
      " Data läses från databasen vid appstart; ingen telemetri aktiveras av appen."
    ),
    fluidRow(
      column(4, selectInput("omrade", "Distrikt", choices = omradesval)),
      column(2, selectInput("ar", "År", choices = arval,
                            selected = if (length(arval)) max(arval) else NULL))
    ),
    # En enda flikrad. Varje flik har avsnitt i kort (.rd-card ur stilmallen).
    tabsetPanel(
      id = 'flikval',
      tabPanel('Översikt',
               uiOutput("nyckeltal"),
               fluidRow(class = "rd-rad-lika",
                        column(5, kort("Distriktsindelning i Dalarna",
                                       "Klicka på ett distrikt i kartan för att välja det.",
                                       uiOutput("kartstatus"),
                                       leafletOutput("karta", height = "520px"))),
                        column(7, kort("Flyttnetto per åldersgrupp",
                                       "Inflyttade minus utflyttade för valt år.",
                                       uiOutput("status_flytt_alder"),
                                       radioButtons("vis_flytt", NULL, choices = visaval, inline = TRUE),
                                       girafeOutput("flytt_alder_diagram")))
               )
      ),
      tabPanel('Befolkning',
               vykort("Befolkningsutveckling", "vy_bef",
                      diagram = tagList(
                        fluidRow(
                          column(6, radioButtons("vis_bef", "Befolkning", choices = visaval, inline = TRUE)),
                          column(6, radioButtons("bef_skala", "Y-axel",
                                                 choices = c("Samma skala i alla distrikt", "Anpassad efter distriktet"),
                                                 inline = TRUE))
                        ),
                        fluidRow(
                          column(6, girafeOutput("befolkning_diagram")),
                          column(6, girafeOutput("flytt_trend_diagram"))
                        )),
                      tabell = DTOutput("flytttabell"),
                      "Hela tidsserien för valt distrikt, valt år markerat. Uppdelningen på kön gäller befolkningen; inflyttade och utflyttade saknar uppdelning på kön och visas alltid som Totalt.",
                      uiOutput("status_flytt_serie_ar")),
               kort("Flyttnetto per åldersgrupp och år",
                    "Totalt, eller uppdelat på kvinnor och män. Valt år är markerat.",
                    radioButtons("vis_flytt_tabell", NULL, choices = visaval, inline = TRUE),
                    DTOutput("flytt_alder_tabell"))
      ),
      tabPanel('Utbildning',
               fluidRow(
                 column(7, vykort("Utbildningsnivå", "vy_utb",
                                  diagram = girafeOutput("utbildning"),
                                  tabell = DTOutput("utbildningstabell"),
                                  "Y-axeln är densamma för alla år i valt distrikt. Tabellen visar alla kön för valt år. Åldersavgränsningen behöver bekräftas före användning av riktig statistik.",
                                  radioButtons("vis_utb", NULL, choices = visaval, inline = TRUE)))
               )
      ),
      tabPanel('Arbetsmarknad',
               fluidRow(
                 column(6, vykort("Största utpendlingsriktningar", "vy_riktning",
                                  diagram = tagList(uiOutput("pendling_karta_status"),
                                                    leafletOutput("pendling_karta", height = "440px")),
                                  tabell = DTOutput("pendling_riktning_tabell"),
                                  flikar = c("Karta", "Tabell"),
                                  "De tre kommuner som flest pendlar till från valt distrikt, valt år. Valt distrikt är mörkgrått.",
                                  uiOutput("status_riktning_storst_pendling"))),
                 column(6, kort("Pendling",
                                "Antal per år.",
                                uiOutput("status_pendling"),
                                radioButtons("vis_pend", NULL, choices = visaval, inline = TRUE),
                                DTOutput("pendling_tabell")))
               ),
               fluidRow(
                 column(6, vykort("Förvärvsarbetande efter åldersgrupp", "vy_arbm_alder",
                                  diagram = girafeOutput("arbm_alder_diagram"),
                                  tabell = DTOutput("arbm_alder_tabell"),
                                  "Valt år.",
                                  uiOutput("status_sysselsattning_alder"))),
                 column(6, kort("Förvärvsarbetande per år",
                                uiOutput("status_sysselsattning_kon"),
                                radioButtons("vis_arbm", NULL, choices = visaval, inline = TRUE),
                                DTOutput("arbm_kon_tabell")))
               )
      ),
      tabPanel('Näringsliv',
               kort("Förvärvsarbetande efter näringsgren och sektor",
                    "Rader inom näringsgren och sektor kan överlappa varandra och ska inte summeras över rader.",
                    fluidRow(
                      column(4, selectInput("syss_block", "Del av tabellen", choices = blockval_syss)),
                      column(4, radioButtons("vis_syss", "Kön", choices = visaval, inline = TRUE))
                    )),
               fluidRow(
                 # Tre vyer av samma data; cirkeldiagrammet visas först. Cirklarnas yta är proportionell mot antalet.
                 column(12, kort("Antal förvärvsarbetande",
                                 tabsetPanel(id = "vy_syss", type = "pills",
                                             tabPanel("Cirkeldiagram", plotOutput("syss_cirkel_diagram", height = "620px")),
                                             tabPanel("Stapeldiagram", girafeOutput("syss_kon_diagram")),
                                             tabPanel("Tabell", DTOutput("syss_kon_tabell")))))
               ),
               kort("Fördelning",
                    "Hur förvärvsarbetande fördelar sig på näringsgrenar eller sektorer. Varje stapel är 100 %.",
                    radioButtons("vis_fordelning", "Staplar per", choices = c("Åldersgrupp", "Kön"), inline = TRUE),
                    girafeOutput("fordelning_diagram")),
               kort("Åldersgrupper per rad", DTOutput("syss_alder_tabell")),
               div(class = "rd-info", "Företag visas här när tabellen finns i databasen.")
      ),
      tabPanel('Inkomst och ekonomi',
               fluidRow(
                 column(6, vykort("Inkomst per åldersgrupp", "vy_ink",
                                  diagram = girafeOutput("inkomst_diagram"),
                                  tabell = DTOutput("inkomst_tabell"),
                                  "Disponibel inkomst avser familjen och är ingen individuell inkomst. Y-axeln är fast för alla kön och år i valt distrikt. Saknas totalrader beräknas medelinkomsten för Totalt ur summa och antal, och medianinkomst kan då inte visas.",
                                  uiOutput("status_inkomst"),
                                  fluidRow(
                                    column(6, selectInput("ink_matt", "Inkomstmått", choices = inkomstmatt)),
                                    column(6, radioButtons("vis_ink", "Kön", choices = visaval, inline = TRUE))
                                  ))),
                 column(6, vykort("Hushållsinkomst", "vy_hushall",
                                  diagram = girafeOutput("hushall_diagram"),
                                  tabell = DTOutput("hushall_tabell"),
                                  "Hushållens disponibla inkomst i kronor, ett värde per hushåll. Prickad lodrät linje markerar valt år.",
                                  uiOutput("status_hushall_inkomst")))
               ),
               fluidRow(
                 column(6, vykort("Ekonomisk standard", "vy_ekstandard",
                                  diagram = girafeOutput("ekstandard_diagram"),
                                  tabell = DTOutput("ekstandard_tabell"),
                                  "Andel hushåll per kvartil av disponibel inkomst per konsumtionsenhet. Kvartilgränserna avser hela riket, så 25 % per kvartil motsvarar rikets fördelning (streckad linje).",
                                  uiOutput("status_ekonomisk_standard")))
               )
      ),
      tabPanel('Bilinnehav',
               kort("Personbilar i trafik",
                    "Utvecklingen för valt distrikt från första året fram till valt år, en linje per drivmedel. Bocka ur de drivmedel du inte vill se.",
                    uiOutput("status_bilar_distrikt"),
                    uiOutput("status_bilar_kommun"),
                    checkboxGroupInput("bilar_urval", "Visa drivmedel",
                                       choices = unname(bilar_drivmedel),
                                       selected = unname(bilar_drivmedel), inline = TRUE)),
               fluidRow(
                 column(6, vykort("Antal personbilar", "vy_bilar_antal",
                                  diagram = girafeOutput("bilar_linjer_antal"),
                                  tabell = DTOutput("bilar_antal_tabell"))),
                 column(6, vykort("Andel av personbilarna", "vy_bilar_andel",
                                  diagram = girafeOutput("bilar_linjer_andel"),
                                  tabell = DTOutput("bilar_andel_tabell"),
                                  "Andelen räknas av alla personbilar, även när bara några drivmedel visas."))
               ),
               kort("Jämförelse, valt år",
                    "Valt distrikt, eller dess kommun, är mörkblått. Distrikt visar de 15 högsta samt valt distrikt.",
                    fluidRow(
                      column(6, selectInput("bilar_matt", "Mått", choices = bilar_matt)),
                      column(6, radioButtons("bilar_niva", "Jämför", choices = c("Distrikt", "Kommuner"), inline = TRUE))
                    )),
               fluidRow(
                 column(6, vykort("Andel, valt år", "vy_bilar_jamfor",
                                  diagram = girafeOutput("bilar_jamfor_diagram"),
                                  tabell = DTOutput("bilar_jamfor_tabell")))
               )
      ),
      tabPanel('Jämför områden',
               kort("Valt distrikt jämfört med övriga distrikt",
                    "Markerat är valt distrikt. Jämförelsen gäller valt år och mått. Antal påverkas av distriktens storlek och är inte andelar. Klicka på en tabellrad för att välja distrikt.",
                    fluidRow(
                      column(4, selectInput("indikator", "Jämförelsemått", choices = indikatorer)),
                      column(4, selectInput("rangordning", "Distrikt i diagrammet",
                                            choices = c("15 högsta samt valt distrikt" = "hogst",
                                                        "15 lägsta samt valt distrikt" = "lagst")))
                    ),
                    textOutput("jamforelsestatus")),
               vykort("Jämförelse", "vy_jamfor",
                      diagram = girafeOutput("jamforelse"),
                      tabell = DTOutput("jamforelsetabell")),
               vykort("Befolkningsförändring över år", "vy_bef_jamfor",
                      diagram = girafeOutput("bef_jamfor_diagram"),
                      tabell = DTOutput("bef_jamfor_tabell"),
                      "Varje distrikt jämförs med sitt första år i serien. Valt distrikt är mörkblått och övriga är grå.",
                      radioButtons("bef_jamfor_matt", NULL,
                                   choices = c("Procent sedan första året", "Förändring i antal personer"),
                                   inline = TRUE))
      ),
      tabPanel('Om',
               kort("Om appen",
                    p("Kartan använder geometri från geodata, karta.distrikt.",
                      " Inga påhittade gränser visas som riktiga distrikt."),
                    p("Totalrader används separat från könsdelningar.",
                      " Rader för näringsgren och sektor överlappar varandra och summeras aldrig över rader."),
                    p("Tabellerna förutsätts vara röjandegranskade innan de läses in; appen gör ingen",
                      " egen granskning."),
                    p("Före användning av riktiga data måste kodnycklar, bortfall,",
                      " åldersgränser och sekretesshantering bekräftas.")),
               kort("Tabellstatus",
                    "Vilka tabeller som lästes in vid appstart, och orsaken när en tabell saknas.",
                    DTOutput("tabellstatus"))
      )
    ),

    # ---- Footer (matchar .rd-footer i regiondalarna_ruf.css) --------------
    tags$div(
      class = 'rd-footer',
      'Samhällsanalys, Region Dalarna · ',
      tags$a(
        href = 'mailto:samhallsanalys@regiondalarna.se',
        'samhallsanalys@regiondalarna.se'
      )
    )
  )
)
