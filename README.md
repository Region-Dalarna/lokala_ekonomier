# lokala_ekonomier

Detta repository innehåller en Shinyapplikation (`lokala_ekonomier`) för
Samhällsanalys, Region Dalarna.

## Struktur

Appfilerna ligger **direkt i repo-roten** (så att Shiny Server kör appen utan omväg):

- `ui.R`, `server.R`, `global.R`
- `www/` för favicon, logotyp, CSS (`regiondalarna_ruf.css` + `app.css`) och `fonts/`
- `R/` för hjälpfunktioner
- `_dependencies.R` i root listar alla paket appen använder (läses av `renv::dependencies()`, körs aldrig)
- `_publicering_till_server.yml` i root styr vilken Shiny-server som är default för `shinyapp_publicera()`
- `renv.lock` + `renv/` + `.Rprofile` styr paketversioner. Kör `renv::restore()` efter klon.

## Databasfritt demonstrationsläge

Appen startar med deterministiska, syntetiska data för åtta **exempeldistrikt**.
Varken distriktsnamnen eller värdena beskriver riktiga områden i Dalarna.
Ingen databasanslutning, telemetri eller publicering initieras av appfilerna.
Starta från repots rot med Shiny i en redan förberedd R-miljö.

- **Karta och diagram:** befolkning över tid, flyttningar och utbildningsnivå.
- **Jämför områden:** de 15 högsta/lägsta värdena samt valt distrikt, med samma
  år och mått. Tabellen visar alla distrikt, även dem som saknar värde.
  Sök/sortera i tabellen och klicka på en rad för att välja distrikt.
- **Underlag:** de filtrerade rader som ligger bakom diagrammen.

Årsvalet markerar ett år i befolkningsserien; övriga diagram visar endast valt år.
Utbildning väljer en könsrad åt gången, inklusive den separata totalraden.
Saknade mått är inte noll och luckor i befolkningsserien bryter linjen.
Inga tillstånd summeras över år, inga könstotaler summeras med delgrupper,
och inga områdesflöden räknas ihop till ett påstått länsflöde.
Inkomst- och sysselsättningstabellerna ingår ännu inte.

### Inmatningspunkt för framtida data och geometri

`global.R` läser ett färdigbearbetat objekt från R-optionen
`lokala_ekonomier.data` om den är satt före appstart. Annars används exempeldata.
Appen hämtar aldrig själv data, vare sig från en fil eller databas.
Användaren ansvarar för framtida databasanslutning och sekretessgranskning.
Objektet är en lista med följande kontrakt:

| Objekt | Innehåll |
|---|---|
| `omraden` | Unik `omrade` som text och `namn` som text. |
| `flytt_summering` | Unik rad per `ar`, `omrade`; numeriska `befolkning`, `inflyttad`, `utflyttad`, `flyttnetto`. |
| `utbildning` | Unik rad per `ar`, `omrade`, `kon`; numeriska `totalt`, `antal_forgymnasial`, `antal_gymnasial`, `antal_eftergymnasial`, `antal_okand`. |
| `distrikt` | `NULL` eller polygoner som `sf` med känt CRS och unik textnyckel `omrade`, exakt samma områden som i `omraden`. |
| `kommun` | `NULL` eller kommunpolygoner som `sf` med känt CRS, redan avgränsade till Dalarna. |
| `exempeldata` | Ett logiskt värde som anger om statistiken är syntetisk. |
| `kalla` | En icke-tom källtext som visas i appen och diagrammen. |

År ska vara numeriska heltalsvärden; könsvärden är `Kvinnor`, `Män`, `Totalt`.
Mått får saknas (`NA`); antal får inte vara negativa. Områdeskoder ska
normaliseras på samma sätt i statistik och kartdata, utan att tappa inledande
nollor. Objektet ersätter hela exempelunderlaget; använd inte verklig geometri
tillsammans med demokoder som om de vore riktiga distrikt.

Geometrireferenserna är `geodata`, `karta.distrikt` och `karta.kommun_scb`.
SHB filtrerar kommunerna med `knkod` som börjar på `20`.
Distriktens verkliga nyckelkolumn och geometrikolumn måste kontrolleras lokalt;
de gissas inte i skalet. Vid framtida EWKB-konvertering kan
`rdshinyappar::df_till_sf()` återanvändas med bekräftad geometrikolumn och CRS.
Utan distriktsgeometri visas en förklaring i stället för påhittade kartgränser.

### Paket och läsreferenser

Layout och jämförelseupplägg utgår från `Region-Dalarna/shb`, `ui.R` och
`global.R`. Diagrammen använder `rddiagram::SkapaLinjeDiagram()` respektive
`rddiagram::SkapaStapelDiagram()` om paketet finns installerat, med
filskrivning och logga avstängda. Annars används befintligt `ggplot2`;
det gäller också diagram med saknade värden för att undvika att de summeras
till noll. Ingen installation görs vid appstart och paketlåset är oförändrat.
Tabeller och urval använder befintliga `DT`, `dplyr` och `tidyr`.

### Lokal verifiering före riktig användning

Kontrollera i R att appen startar utan DB, att alla filter och tabellklick
uppdaterar vyerna och att valt distrikt följer med i båda rangordningarna.
Prova även tomma urval, saknade mått, årsluckor och avvisade dubblettnycklar,
samt diagram med och utan det valfria `rddiagram`.
Riktig polygongeometri behöver testas lokalt för CRS, geografisk täckning,
kodmatchning, kommungränser och markering av valt distrikt.
Före riktig statistik krävs även beslut om sekretess, bortfall, utbildningens
åldersgräns och eventuell jämförelse av andelar i stället för storleksberoende antal.

## Deploy

Sker via GitHub Actions:

- `.github/workflows/deploy.yml` – publicerar vid push till `publicera-publik` eller `publicera-intern`
- `.github/workflows/avpublicera.yml` – tar bort appen från vald server (manuell trigger)

Appmapp på servern: `/srv/shiny-server/lokala_ekonomier`.

Publicera med `rddeploy::shinyapp_publicera("lokala_ekonomier")`.
