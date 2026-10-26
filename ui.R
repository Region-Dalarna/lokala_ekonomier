shinyUI(
  fluidPage(
    title = APP_TITEL,
    tags$head(
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'regiondalarna_ruf.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'app.css')
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
      " Ingen databasanslutning eller telemetri aktiveras av appen."
    ),
    fluidRow(
      column(4, selectInput("omrade", "Distrikt", choices = omradesval)),
      column(2, selectInput("ar", "År", choices = arval,
                            selected = if (length(arval)) max(arval) else NULL)),
      column(3, selectInput("indikator", "Jämförelsemått", choices = indikatorer)),
      column(3, selectInput("kon", "Kön – utbildning",
                            choices = c("Totalt", "Kvinnor", "Män")))
    ),
    tabsetPanel(
      id = 'flikval',
      tabPanel('Karta och diagram',
        fluidRow(
          column(4,
            h3("Distriktsindelning i Dalarna"),
            p("Valt distrikt markeras när riktig geometri finns tillgänglig."),
            uiOutput("kartstatus"),
            plotOutput("karta", height = "540px")
          ),
          column(8,
            plotOutput("befolkning", height = "300px"),
            plotOutput("flyttningar", height = "300px"),
            plotOutput("utbildning", height = "300px")
          )
        )
      ),
      tabPanel('Jämför områden',
        h3("Valt distrikt jämfört med övriga distrikt"),
        p("Orange markerar valt distrikt. Jämförelsen gäller valt år och mått.",
          " Antal påverkas av distriktens storlek och är inte andelar.",
          " Klicka på en tabellrad för att välja distrikt."),
        selectInput("rangordning", "Distrikt i diagrammet",
                    choices = c("15 högsta samt valt distrikt" = "hogst",
                                "15 lägsta samt valt distrikt" = "lagst")),
        textOutput("jamforelsestatus"),
        plotOutput("jamforelse", height = "600px"),
        DTOutput("jamforelsetabell")
      ),
      tabPanel('Underlag',
        h3("Flytt och befolkning – valt distrikt och år"),
        DTOutput("flyttabell"),
        h3("Utbildning – valt distrikt, år och kön"),
        p("Utbildningens åldersavgränsning behöver bekräftas före användning av riktig statistik."),
        DTOutput("utbildningstabell")
      ),
      tabPanel('Om',
        h3("Om skalet"),
        p("Appen följer SHB:s uppdelning i karta/diagram och områdesjämförelse.",
          " Kartan väntar på geometri från geodata, karta.distrikt.",
          " Inga påhittade gränser visas som riktiga distrikt."),
        p("Totalrader används separat från könsdelningar.",
          " Befolkning summeras inte över år.",
          " Inkomstmått och överlappande sektor-/branschrader ingår inte i detta skal."),
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
