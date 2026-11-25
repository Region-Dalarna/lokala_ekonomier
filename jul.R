# =============================================================================
# JULSPECIAL för Shiny-appen: snöfall, ljusslinga i sidhuvudet, tomteluva på titeln,
# en tomte i pixelgrafik som vandrar över skärmen och en gran med paket i sidfoten.
#
# Allt ritas i webbläsaren (CSS och JavaScript i en liten snutt), inga bildfiler behövs.
# Dekorationen ligger ovanpå sidan, fångar inga klick (pointer-events: none) och
# ändrar inte layouten. Den lilla knappen "Julspecial" nere till vänster slår av och på den,
# och valet sparas i webbläsaren. Den som har aktiverat "minska rörelse" i sitt system får inga animationer.
#
# Användning:
#   1. I global.R:   source("julspecial.R", encoding = "UTF-8")
#   2. I ui.R, inuti fluidPage(...):   julspecial()
#
# Som standard är den på 1 december till 6 januari. Tvinga på eller av med argumentet:
#   julspecial(aktiv = TRUE)     # alltid på (bra för att testa)
#   julspecial(aktiv = FALSE)    # helt borttagen
# Snabb avstängning utan att ändra koden: knappen "Julspecial" i sidans hörn.
# =============================================================================

# Är det jul? 1 december till 6 januari
jul_period <- function(datum = Sys.Date()) {
  m <- as.integer(format(datum, "%m")); d <- as.integer(format(datum, "%d"))
  m == 12 || (m == 1 && d <= 6)
}

julspecial <- function(aktiv = jul_period(),
                       snoflingor = 70,       # antal snöflingor (0 = ingen snö)
                       tomte = TRUE,          # tomten som vandrar över skärmen
                       ljusslinga = TRUE,     # blinkande lampor under sidhuvudet
                       gran = TRUE) {         # gran och paket i sidfoten
  if (!isTRUE(aktiv)) return(NULL)

  # Inställningar skickas till JavaScript
  inst <- sprintf("{snoflingor: %d, tomte: %s, ljusslinga: %s, gran: %s}",
                  as.integer(snoflingor), tolower(isTRUE(tomte)), tolower(isTRUE(ljusslinga)), tolower(isTRUE(gran)))

  css <- "
    #jul-knapp { position: fixed; left: 12px; bottom: 12px; z-index: 10001; padding: 4px 12px; font-size: 12px;
      font-family: inherit; border: 2px solid #424242; border-radius: 0; background: #fff; color: #424242;
      cursor: pointer; box-shadow: 3px 3px 0 #424242; line-height: 1.4; }
    #jul-knapp:hover { background: #ffec9f; }
    #jul-knapp:focus-visible { outline: 3px solid #158daf; outline-offset: 2px; }
    /* Allt annat visas bara när julen är på */
    #jul-sno, .jul-tomte, .jul-ljus, .jul-gran, .jul-luva { display: none; }
    body.jul-pa #jul-sno { display: block; }
    body.jul-pa .jul-tomte { display: block; }
    body.jul-pa .jul-ljus, body.jul-pa .jul-gran { display: block; }
    body.jul-pa .jul-luva { display: inline-block; }
    #jul-sno { position: fixed; inset: 0; width: 100%; height: 100%; pointer-events: none; z-index: 10000; }
    .jul-tomte { position: fixed; bottom: 0; left: 0; z-index: 10000; pointer-events: none; }
    .jul-tomte-i { image-rendering: pixelated; background-repeat: no-repeat; transform-origin: 50% 100%; }
    .jul-hopp { animation: jul-hopp .3s steps(1) infinite; }
    .jul-lutar { transform: skewX(-7deg); }
    @keyframes jul-hopp { 0% { transform: translateY(0); } 50% { transform: translateY(-5px); } }
    .jul-ljus { position: absolute; left: 0; right: 0; bottom: -9px; height: 10px; pointer-events: none; z-index: 2; }
    .jul-ljus i { position: absolute; top: 0; width: 6px; height: 8px; border-radius: 50% 50% 50% 50% / 30% 30% 70% 70%;
      animation: jul-blink 1.6s ease-in-out infinite; }
    .jul-ljus::before { content: ''; position: absolute; left: 0; right: 0; top: 0; height: 2px; background: #2f4f2f; opacity: .55; }
    @keyframes jul-blink { 0%, 100% { opacity: 1; } 50% { opacity: .3; } }
    .jul-gran { position: absolute; right: 56px; bottom: 100%; pointer-events: none; image-rendering: pixelated; background-repeat: no-repeat; }
    .jul-luva { vertical-align: middle; margin-right: 10px; image-rendering: pixelated; background-repeat: no-repeat; }
    #jul-notis { position: fixed; left: 12px; bottom: 58px; z-index: 10002; max-width: 270px; padding: 10px 28px 10px 12px;
      background: #fff5cc; color: #424242; border: 2px solid #424242; box-shadow: 3px 3px 0 #424242;
      font-size: 13px; line-height: 1.45; opacity: 0; transition: opacity .5s; pointer-events: none; }
    #jul-notis.jul-syns { opacity: 1; pointer-events: auto; }
    #jul-notis b { color: #c8102e; }
    #jul-notis button { position: absolute; top: 2px; right: 4px; border: 0; background: transparent; font-size: 18px;
      line-height: 1; cursor: pointer; color: #424242; padding: 2px 4px; }
    #jul-notis::after { content: ''; position: absolute; left: 24px; bottom: -10px; width: 0; height: 0;
      border-left: 8px solid transparent; border-right: 8px solid transparent; border-top: 8px solid #424242; }
    @media (prefers-reduced-motion: reduce) {
      #jul-notis { transition: none; }
      .jul-tomte { left: 24px; }
      .jul-hopp { animation: none; }
      .jul-ljus i { animation: none; }
    }
  "

  js <- paste0("
(function () {
  var INST = ", inst, ";
  var NYCKEL = 'jul_special_av';
  function lasa() { try { return localStorage.getItem(NYCKEL); } catch (e) { return null; } }
  function spara(v) { try { localStorage.setItem(NYCKEL, v); } catch (e) {} }
  var minskaRorelse = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  // ---- Pixelgrafik: ritas från textrader, en bokstav per pixel -------------------------------
  var PAL = { R: '#c8102e', W: '#ffffff', S: '#f1c9a5', K: '#2b2b2b', B: '#5a3a1e', G: '#2e7d32', L: '#1b5e20',
              Y: '#ffd378', O: '#e2a855', H: '#8edded', D: '#158daf', P: '#f8aab6', N: '#424242',
              M: '#a0693a', C: '#f1d9b5', A: '#d9b777' };
  // kontur: true ritar en mörk kantlinje runt figuren, så att vitt (skägg) syns mot en ljus bakgrund
  function rita(rader, skala, kontur) {
    var pad = kontur ? 1 : 0, br = rader[0].length + 2 * pad, hj = rader.length + 2 * pad;
    var c = document.createElement('canvas'); c.width = br * skala; c.height = hj * skala;
    var x = c.getContext('2d');
    function fylld(r, i) { return r >= 0 && r < rader.length && i >= 0 && i < rader[r].length && !!PAL[rader[r].charAt(i)]; }
    if (kontur) {
      x.fillStyle = '#424242';
      for (var r = -1; r <= rader.length; r++) for (var i = -1; i <= rader[0].length; i++) {
        if (!fylld(r, i) && (fylld(r - 1, i) || fylld(r + 1, i) || fylld(r, i - 1) || fylld(r, i + 1)))
          x.fillRect((i + pad) * skala, (r + pad) * skala, skala, skala);
      }
    }
    for (var y = 0; y < rader.length; y++) for (var j = 0; j < rader[y].length; j++) {
      var t = rader[y].charAt(j); if (PAL[t]) { x.fillStyle = PAL[t]; x.fillRect((j + pad) * skala, (y + pad) * skala, skala, skala); }
    }
    return c;
  }
  var TOMTE_A = [
    '......WW......',
    '.....RRRR.....',
    '....RRRRRR....',
    '...RRRRRRRR...',
    '..WWWWWWWWWW..',
    '...WSSSSSSW...',
    '...WSKSSKSW...',
    '...WSSSSSSW...',
    '..WWWWWWWWWW..',
    '..WWWWWWWWWW..',
    '.RRRWWWWWWRRR.',
    '.RRRRRRRRRRRR.',
    '.SRRRNNNNRRRS.',
    '..RRRRRRRRRR..',
    '..RRR....RRR..',
    '..BBB....BBB..'
  ];
  // Bygger en bildruta: ben = 'A' (stå), 'B' (isär), 'L' / 'R' (vänster / höger fot lyft, på tå), glas = solglasögon
  function tomteRuta(ben, glas) {
    var r = TOMTE_A.slice(0, 14);
    if (glas) r[6] = '...WKKSSKKW...';   // två glas med näsrygg mellan
    var ben_rader = {
      A: ['..RRR....RRR..', '..BBB....BBB..'],
      B: ['...RRR..RRR...', '...BBB..BBB...'],
      L: ['..BBB....RRR..', '.........BBB..'],
      R: ['..RRR....BBB..', '..BBB.........']
    }[ben];
    return r.concat(ben_rader);
  }
  var LUVA = [        // symmetrisk tomteluva: dusk i mitten, jämn trekant, bred vit kant
    '....WWW....',
    '....WWW....',
    '....RRR....',
    '...RRRRR...',
    '..RRRRRRR..',
    '.RRRRRRRRR.',
    'WWWWWWWWWWW'
  ];
  var GRAN = [
    '.....Y.....',
    '....GGG....',
    '...GGGGG...',
    '....GGG....',
    '...GGGGG...',
    '..GGGGGGG..',
    '...GGGGG...',
    '..GGGGGGG..',
    '.GGGGGGGGG.',
    '..GGGGGGG..',
    '.GGGGGGGGG.',
    'GGGGGGGGGGG',
    '.....BB....',
    '.....BB....',
    '..RRR.HHH..',
    '..RWR.HDH..',
    '..RRR.HHH..'
  ];
  // Prickar i granen (ljus och kulor): ritas ovanpå
  function granMedPrickar(skala) {
    var c = rita(GRAN, skala), x = c.getContext('2d');
    [[4,3,'#ffd378'],[6,5,'#f8aab6'],[3,7,'#8edded'],[7,9,'#ffd378'],[2,10,'#f8aab6'],[6,11,'#8edded']].forEach(function (p) {
      x.fillStyle = p[2]; x.fillRect(p[0] * skala, p[1] * skala, skala, skala);
    });
    return c;
  }

  function skapaKnapp() {
    var b = document.createElement('button'); b.id = 'jul-knapp'; b.type = 'button';
    function text() { b.textContent = document.body.classList.contains('jul-pa') ? 'Julspecial: på' : 'Julspecial: av'; }
    b.addEventListener('click', function () {
      var pa = !document.body.classList.contains('jul-pa');
      document.body.classList.toggle('jul-pa', pa); spara(pa ? '0' : '1'); text();
    });
    document.body.appendChild(b); text();
  }

  function snow() {
    if (!INST.snoflingor || minskaRorelse) return;
    var c = document.createElement('canvas'); c.id = 'jul-sno'; document.body.appendChild(c);
    var x = c.getContext('2d'), flingor = [], farger = ['#ffffff', '#b6f0fd', '#8edded', '#ffffff'];
    function storlek() { c.width = window.innerWidth; c.height = window.innerHeight; }
    storlek(); window.addEventListener('resize', storlek);
    for (var i = 0; i < INST.snoflingor; i++) flingor.push({
      x: Math.random() * c.width, y: Math.random() * c.height, s: 2 + Math.floor(Math.random() * 3) * 1.5,
      v: 0.4 + Math.random() * 0.9, f: Math.random() * 6.28, k: farger[i % farger.length] });
    function steg() {
      if (document.body.classList.contains('jul-pa') && !document.hidden) {
        x.clearRect(0, 0, c.width, c.height);
        flingor.forEach(function (f) {
          f.y += f.v; f.f += 0.02; f.x += Math.sin(f.f) * 0.4;
          if (f.y > c.height) { f.y = -4; f.x = Math.random() * c.width; }
          x.fillStyle = f.k; x.globalAlpha = 0.85;
          x.fillRect(Math.round(f.x), Math.round(f.y), f.s, f.s);    // fyrkantiga flingor, i pixelstil
          x.strokeStyle = 'rgba(21,141,175,.35)'; x.lineWidth = 1; x.strokeRect(Math.round(f.x) + .5, Math.round(f.y) + .5, f.s, f.s);
        });
        x.globalAlpha = 1;
      }
      requestAnimationFrame(steg);
    }
    requestAnimationFrame(steg);
  }

  // Tomten tar ibland en vanlig promenad, ibland shufflar han (sidledes, med studs och fotvickningar) och
  // ibland moonwalkar han (lutad, glider bakåt åt vänster med fötterna på tå). Solglasögon har han ibland, oavsett läge.
  function tomteVandrar() {
    if (!INST.tomte) return;
    var skala = 5;
    // bildrutor: 0 A, 1 B, 2 L, 3 R utan solglasögon; 4-7 samma med solglasögon
    var lagen = [['A', false], ['B', false], ['L', false], ['R', false], ['A', true], ['B', true], ['L', true], ['R', true]];
    var rutor = lagen.map(function (l) { return rita(tomteRuta(l[0], l[1]), skala, true); });
    var br = rutor[0].width, hj = rutor[0].height;
    var blad = document.createElement('canvas'); blad.width = br * rutor.length; blad.height = hj;
    var x = blad.getContext('2d'); rutor.forEach(function (c, i) { x.drawImage(c, i * br, 0); });
    var yttre = document.createElement('div'); yttre.className = 'jul-tomte';
    var inre = document.createElement('div'); inre.className = 'jul-tomte-i';
    inre.style.width = br + 'px'; inre.style.height = hj + 'px';
    inre.style.backgroundImage = 'url(' + blad.toDataURL() + ')';
    inre.style.backgroundSize = (br * rutor.length) + 'px ' + hj + 'px';
    yttre.appendChild(inre); document.body.appendChild(yttre);
    function ruta(i) { inre.style.backgroundPosition = (-i * br) + 'px 0'; }
    ruta(0);

    if (minskaRorelse || !yttre.animate) return;   // stillastående tomte om rörelse är avstängd

    var timer = null;
    function passage() {
      var slump = Math.random(), lage, ruteforljd, tempo, tid, fran, till, glas;
      var bredd = window.innerWidth;
      inre.className = 'jul-tomte-i';
      if (slump < 0.5) {                                   // promenad åt höger
        lage = 'gar'; ruteforljd = [0, 1]; tempo = 250; tid = 38000; fran = -90; till = bredd + 10;
      } else if (slump < 0.78) {                           // shuffle åt höger: studsar och vickar med fötterna
        lage = 'shuffle'; ruteforljd = [0, 2, 0, 3]; tempo = 150; tid = 30000; fran = -90; till = bredd + 10;
        inre.className += ' jul-hopp';
      } else {                                             // moonwalk åt vänster, solglasögon och lutad
        lage = 'moonwalk'; ruteforljd = [2, 3]; tempo = 450; tid = 36000; fran = bredd + 10; till = -90;
        inre.className += ' jul-lutar';
      }
      // Solglasögon ibland: oftast vid moonwalk (60 %), annars då och då (25 %)
      glas = Math.random() < (lage === 'moonwalk' ? 0.6 : 0.25);
      if (glas) ruteforljd = ruteforljd.map(function (n) { return n + 4; });
      var i = 0; clearInterval(timer);
      timer = setInterval(function () { i = (i + 1) % ruteforljd.length; ruta(ruteforljd[i]); }, tempo);
      ruta(ruteforljd[0]);
      var anim = yttre.animate([{ transform: 'translateX(' + fran + 'px)' }, { transform: 'translateX(' + till + 'px)' }],
                               { duration: tid, easing: 'linear', fill: 'forwards' });
      anim.onfinish = function () {
        clearInterval(timer); ruta(0); inre.className = 'jul-tomte-i';
        yttre.style.visibility = 'hidden';
        // Paus 15-40 s innan tomten kommer tillbaka
        setTimeout(function () { yttre.style.visibility = ''; passage(); }, 15000 + Math.random() * 25000);
      };
    }
    yttre.style.visibility = 'hidden';
    setTimeout(function () { yttre.style.visibility = ''; passage(); }, 2000);   // första gången efter 2 s
    // För test: window.julTomte('moonwalk') etc. går att lägga till vid behov
  }

  function ljusslinga() {
    var hu = document.querySelector('.rd-header'); if (!hu) return;
    if (getComputedStyle(hu).position === 'static') hu.style.position = 'relative';
    var g = document.createElement('div'); g.className = 'jul-ljus'; hu.appendChild(g);
    var farger = ['#c8102e', '#ffd378', '#8edded', '#54b798', '#f8aab6'];
    function fyll() {
      g.innerHTML = ''; var n = Math.max(8, Math.floor(hu.clientWidth / 34));
      for (var i = 0; i < n; i++) {
        var l = document.createElement('i'); l.style.left = ((i + 0.5) * 100 / n) + '%';
        l.style.top = (i % 2 ? 1 : 3) + 'px'; l.style.background = farger[i % farger.length];
        l.style.animationDelay = ((i % 5) * 0.3) + 's'; g.appendChild(l);
      }
    }
    fyll(); window.addEventListener('resize', fyll);
  }

  function luva() {
    var t = document.querySelector('.rd-header__title'); if (!t) return;
    var s = 5, c = rita(LUVA, s), e = document.createElement('span'); e.className = 'jul-luva';
    e.style.width = c.width + 'px'; e.style.height = c.height + 'px'; e.style.backgroundImage = 'url(' + c.toDataURL() + ')';
    t.insertBefore(e, t.firstChild);
  }

  function granIFoten() {
    if (!INST.gran) return;
    var f = document.querySelector('.rd-footer'); if (!f) return;
    if (getComputedStyle(f).position === 'static') f.style.position = 'relative';
    var s = 5, c = granMedPrickar(s), e = document.createElement('div'); e.className = 'jul-gran';
    e.style.width = c.width + 'px'; e.style.height = c.height + 'px'; e.style.backgroundImage = 'url(' + c.toDataURL() + ')';
    f.appendChild(e);
  }

  // Liten julnotis nere till vänster efter 15 s: att man kan slå av julkänslan med knappen. Visas en gång per
  // webbläsarsession och försvinner av sig själv efter 14 s (eller när man stänger den eller slår av julen).
  function notis() {
    var SETT = 'julspecial_notis_sett';
    try { if (sessionStorage.getItem(SETT) === '1') return; } catch (e) {}
    setTimeout(function () {
      if (!document.body.classList.contains('jul-pa')) return;
      var n = document.createElement('div'); n.id = 'jul-notis'; n.setAttribute('role', 'status'); n.setAttribute('aria-live', 'polite');
      n.innerHTML = '<b>God jul!</b> Vill du lämna julstämningen? Slå av den med knappen <b>Julspecial</b> här nere.';
      var x = document.createElement('button'); x.type = 'button'; x.setAttribute('aria-label', 'Stäng notisen'); x.textContent = '×';
      function stang() { n.classList.remove('jul-syns'); setTimeout(function () { if (n.parentNode) n.parentNode.removeChild(n); }, 600); }
      x.addEventListener('click', stang); n.appendChild(x); document.body.appendChild(n);
      document.getElementById('jul-knapp').addEventListener('click', stang);
      requestAnimationFrame(function () { n.classList.add('jul-syns'); });
      setTimeout(stang, 14000);
      try { sessionStorage.setItem(SETT, '1'); } catch (e) {}
    }, 15000);
  }

  function start() {
    // 'jul_special_av' = '1' betyder att besökaren har stängt av julen
    document.body.classList.toggle('jul-pa', lasa() !== '1');
    skapaKnapp(); snow(); tomteVandrar();
    if (INST.ljusslinga) ljusslinga();
    luva(); granIFoten(); notis();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
})();
")

  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(css)),
    htmltools::tags$script(htmltools::HTML(js))
  )
}
