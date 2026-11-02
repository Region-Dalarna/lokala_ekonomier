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
        .karta-kontroll { background: #fff; border-radius: 4px; padding: 6px 10px;
          box-shadow: 0 1px 5px rgba(0,0,0,.4); }
        .karta-kontroll__rad { display: flex; align-items: center; gap: 8px; margin: 0;
          font-weight: normal; font-size: 13px; color: #464d48; cursor: pointer; }
        .karta-kontroll__rad + .karta-kontroll__rad { margin-top: 6px; }
        .karta-kontroll input[type=range] { width: 110px; margin: 0; cursor: pointer; }
        .karta-kontroll input[type=checkbox] { margin: 0; cursor: pointer; }
        .karta-kontroll__ikon { font-size: 16px; line-height: 1; }
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
      class = "alert alert-info", role = "status",
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
    tabsetPanel(
      id = 'flikval',
      tabPanel('Karta och diagram',
               fluidRow(
                 column(4,
                        h3("Distriktsindelning i Dalarna"),
                        p("Klicka på ett distrikt i kartan för att välja det. Valt distrikt markeras i orange."),
                        uiOutput("kartstatus"),
                        leafletOutput("karta", height = "540px")
                 ),
                 column(8,
                        uiOutput("status_flytt_alder"),
                        selectInput("kon_flytt", "Kön – flyttnetto per åldersgrupp",
                                    choices = c("Totalt", "Kvinnor", "Män")),
                        plotOutput("flytt_alder_diagram", height = "380px"),
                        h4("Flyttnetto per åldersgrupp och år"),
                        DTOutput("flytt_alder_tabell"),
                        h4("Flytt och befolkning per år – valt distrikt"),
                        p("Valt år är markerat i tabellen."),
                        DTOutput("flytttabell")
                 )
               )
      ),
      tabPanel('Jämför områden',
               h3("Valt distrikt jämfört med övriga distrikt"),
               p("Orange markerar valt distrikt. Jämförelsen gäller valt år och mått.",
                 " Mått ur flytt- och befolkningstabellen samt hushållens disponibla inkomst.",
                 " Antal påverkas av distriktens storlek och är inte andelar.",
                 " Klicka på en tabellrad för att välja distrikt."),
               fluidRow(
                 column(4, selectInput("indikator", "Jämförelsemått", choices = indikatorer)),
                 column(4, selectInput("rangordning", "Distrikt i diagrammet",
                                       choices = c("15 högsta samt valt distrikt" = "hogst",
                                                   "15 lägsta samt valt distrikt" = "lagst")))
               ),
               textOutput("jamforelsestatus"),
               plotOutput("jamforelse", height = "600px"),
               DTOutput("jamforelsetabell")
      ),
      tabPanel('Underlag',
               h3("Utbildning – valt distrikt och år"),
               p("Y-axeln är densamma för Totalt, Kvinnor och Män och för alla år i valt distrikt,",
                 " så att staplarna går att jämföra."),
               selectInput("kon_utb", "Kön", choices = c("Totalt", "Kvinnor", "Män")),
               plotOutput("utbildning", height = "450px"),
               h4("Alla kön, valt år"),
               p("Utbildningens åldersavgränsning behöver bekräftas före användning av riktig statistik."),
               DTOutput("utbildningstabell")
      ),
      tabPanel('Inkomst och sysselsättning',
               p("Tabeller ur databasschemat ale för valt distrikt och år."),
               tabsetPanel(
                 id = 'underflik',
                 tabPanel('Inkomst',
                          uiOutput("status_inkomst"),
                          fluidRow(
                            column(4, selectInput("kon_ink", "Kön", choices = c("Totalt", "Kvinnor", "Män"))),
                            column(4, selectInput("ink_matt", "Inkomstmått", choices = inkomstmatt))
                          ),
                          p("Disponibel inkomst avser familjen och är ingen individuell inkomst.",
                            " Y-axeln är fast för alla kön och år i valt distrikt, så att Män, Kvinnor och Totalt",
                            " går att jämföra.",
                            " Om tabellen saknar totalrader beräknas medelinkomsten",
                            " för Totalt ur summa och antal, och medianinkomst kan då inte visas."),
                          plotOutput("inkomst_diagram", height = "360px"),
                          DTOutput("inkomst_tabell")
                 ),
                 tabPanel('Hushållsinkomst',
                          uiOutput("status_hushall_inkomst"),
                          p("Hushållens disponibla inkomst i kronor, ett värde per hushåll. Streckad lodrät linje",
                            " markerar valt år."),
                          plotOutput("hushall_diagram", height = "360px"),
                          DTOutput("hushall_tabell")
                 ),
                 tabPanel('Ekonomisk standard',
                          uiOutput("status_ekonomisk_standard"),
                          p("Andel hushåll per kvartil av disponibel inkomst per konsumtionsenhet.",
                            " Kvartilgränserna avser hela riket, så 25 % per kvartil motsvarar rikets fördelning",
                            " (streckad linje)."),
                          plotOutput("ekstandard_diagram", height = "360px"),
                          DTOutput("ekstandard_tabell")
                 ),
                 tabPanel('Sysselsättning – ålder',
                          uiOutput("status_sysselsattning_alder"),
                          fluidRow(column(4, selectInput("syss_block_alder", "Del av tabellen", choices = blockval))),
                          p("Förvärvsarbetande efter åldersgrupp. Rader inom näringsgren och sektor kan överlappa",
                            " varandra och ska inte summeras över rader. Summan avser åldersgrupperna inom en rad."),
                          plotOutput("syss_alder_diagram", height = "480px"),
                          DTOutput("syss_alder_tabell")
                 ),
                 tabPanel('Sysselsättning – kön',
                          uiOutput("status_sysselsattning_kon"),
                          fluidRow(column(4, selectInput("syss_block_kon", "Del av tabellen", choices = blockval))),
                          p("Förvärvsarbetande efter kön. Rader inom näringsgren och sektor kan överlappa varandra",
                            " och ska inte summeras över rader."),
                          plotOutput("syss_kon_diagram", height = "480px"),
                          DTOutput("syss_kon_tabell")
                 )
               )
      ),
      tabPanel('Om',
               h3("Om skalet"),
               p("Appen följer SHB:s uppdelning i karta/diagram och områdesjämförelse.",
                 " Kartan använder geometri från geodata, karta.distrikt.",
                 " Inga påhittade gränser visas som riktiga distrikt."),
               p("Totalrader används separat från könsdelningar.",
                 " Rader för näringsgren och sektor överlappar varandra och summeras aldrig över rader."),
               p("Inkomst, hushållsinkomst, ekonomisk standard och sysselsättning visas per tabell.",
                 " Tabellerna förutsätts vara röjandegranskade innan de läses in; appen gör ingen",
                 " egen granskning."),
               p("Före användning av riktiga data måste kodnycklar, bortfall,",
                 " åldersgränser och sekretesshantering bekräftas.")
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
