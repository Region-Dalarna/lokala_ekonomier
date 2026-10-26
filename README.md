# lokala_ekonomier

Detta repository innehåller en Shinyapplikation (`lokala_ekonomier`) för
Samhällsanalys, Region Dalarna.

- `.github/workflows/deploy.yml` – publicerar vid push till `publicera-publik` eller `publicera-intern`
- `.github/workflows/avpublicera.yml` – tar bort appen från vald server (manuell trigger)

Appmapp på servern: `/srv/shiny-server/lokala_ekonomier`.

Publicera med `rddeploy::shinyapp_publicera("lokala_ekonomier")`.
