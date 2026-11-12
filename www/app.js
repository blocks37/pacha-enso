// Interacción del cliente: colorear mapas sin reenviarlos y filas clicables.

window.ensoMapas = window.ensoMapas || {};
window.ensoPendientes = window.ensoPendientes || {};
window.ensoEncuadre = window.ensoEncuadre || {};       // último encuadre pedido por mapa
var ENSO_PERU = [-81.4, -18.4, -68.6, -0.03];           // oeste, sur, este, norte

function ensoEncuadrar(id) {
  var mapa = window.ensoMapas[id];
  if (!mapa) return;
  var b = window.ensoEncuadre[id] || ENSO_PERU;
  try {
    mapa.invalidateSize();
    // Sin tamaño (pestaña oculta) no se puede encuadrar: se hará al mostrarla
    if (mapa.getSize().x < 50) return;
    mapa.fitBounds([[b[1], b[0]], [b[3], b[2]]], { padding: [8, 8], animate: false });
  } catch (e) {}
}

function ensoLista(x) { return x === null || x === undefined ? [] : [].concat(x); }

function ensoColorear(mapa, msg) {
  var ids = ensoLista(msg.ids), colores = ensoLista(msg.colores),
      etiquetas = ensoLista(msg.etiquetas), opacidad = ensoLista(msg.opacidad);
  for (var i = 0; i < ids.length; i++) {
    var capa = mapa.layerManager.getLayer('shape', ids[i]);
    if (!capa) continue;
    capa.setStyle({ fillColor: colores[i], fillOpacity: opacidad[i] });
    if (etiquetas[i] && capa.getTooltip()) capa.setTooltipContent(etiquetas[i]);
  }
}

// Se llama desde htmlwidgets::onRender cuando el mapa termina de dibujarse.
function ensoRegistrarMapa(id, mapa) {
  window.ensoMapas[id] = mapa;
  setTimeout(function () { ensoEncuadrar(id); }, 50);
  var pendiente = window.ensoPendientes[id];
  if (pendiente) {
    pendiente.forEach(function (m) { ensoColorear(mapa, m); });
    delete window.ensoPendientes[id];
  }
}

$(document).on('shiny:connected', function () {
  Shiny.addCustomMessageHandler('colorear-mapa', function (msg) {
    var mapa = window.ensoMapas[msg.id];
    if (mapa) { ensoColorear(mapa, msg); return; }
    // El mapa aún no existe (pestaña no abierta): se aplica al dibujarse, en orden.
    window.ensoPendientes[msg.id] = (window.ensoPendientes[msg.id] || []).concat([msg]);
  });
  Shiny.addCustomMessageHandler('encuadrar-mapa', function (msg) {
    window.ensoEncuadre[msg.id] = msg.b;
    ensoEncuadrar(msg.id);
  });
});

// Leaflet necesita recalcular su tamaño cuando su pestaña se muestra; de paso se
// vuelve a centrar en el Perú (o en la región elegida).
$(document).on('shown.bs.tab', function () {
  setTimeout(function () { Object.keys(window.ensoMapas).forEach(ensoEncuadrar); }, 60);
});
var ensoTemporizador = null;
$(window).on('resize', function () {
  clearTimeout(ensoTemporizador);
  ensoTemporizador = setTimeout(function () { Object.keys(window.ensoMapas).forEach(ensoEncuadrar); }, 250);
});

// Vuelve a reproducir la animación de los gráficos de ECharts indicados.
function ensoReproducir(ids) {
  ids.forEach(function (id) {
    var el = document.getElementById(id);
    if (!el || !window.echarts) return;
    var g = echarts.getInstanceByDom(el.querySelector('div') || el) || echarts.getInstanceByDom(el);
    if (!g) return;
    var opcion = g.getOption();
    g.clear();
    g.setOption(opcion, true);
  });
}

// Focos de alerta: puntos que laten sobre los distritos en nivel alto o crítico ------------
window.ensoFocos = window.ensoFocos || {};
function ensoDibujarFocos(mapa, msg) {
  if (window.ensoFocos[msg.id]) { mapa.removeLayer(window.ensoFocos[msg.id]); }
  var grupo = L.layerGroup();
  var lat = ensoLista(msg.lat), lon = ensoLista(msg.lon), color = ensoLista(msg.color),
      etiqueta = ensoLista(msg.etiqueta), critico = ensoLista(msg.critico), ub = ensoLista(msg.ubigeo);
  for (var i = 0; i < lat.length; i++) {
    var icono = L.divIcon({
      className: 'foco' + (critico[i] ? ' foco-critico' : ''),
      html: '<span class="foco-onda" style="--c:' + color[i] + '"></span><span class="foco-punto" style="--c:' + color[i] + '"></span>',
      iconSize: [22, 22], iconAnchor: [11, 11]
    });
    var m = L.marker([lat[i], lon[i]], { icon: icono, keyboard: false, riseOnHover: true });
    if (etiqueta[i]) m.bindTooltip(etiqueta[i], { direction: 'top', className: 'etiqueta-mapa', offset: [0, -8] });
    (function (u) {
      m.on('click', function () { if (window.Shiny) Shiny.setInputValue(msg.id + '_shape_click', { id: u, nonce: Math.random() }); });
    })(ub[i]);
    grupo.addLayer(m);
  }
  grupo.addTo(mapa);
  window.ensoFocos[msg.id] = grupo;
}
$(document).on('shiny:connected', function () {
  Shiny.addCustomMessageHandler('focos-mapa', function (msg) {
    var mapa = window.ensoMapas[msg.id];
    if (mapa) { ensoDibujarFocos(mapa, msg); return; }
    window.ensoPendientesFocos = window.ensoPendientesFocos || {};
    window.ensoPendientesFocos[msg.id] = msg;
  });
});
var ensoRegistrarOriginal = ensoRegistrarMapa;
ensoRegistrarMapa = function (id, mapa) {
  ensoRegistrarOriginal(id, mapa);
  var p = (window.ensoPendientesFocos || {})[id];
  if (p) { ensoDibujarFocos(mapa, p); delete window.ensoPendientesFocos[id]; }
};

// El Niño en el mar: un cuadro por semana (NASA GIBS), en bucle ------------------------------
function ensoAnimarMar(mapa, datos) {
  var fechas = ensoLista(datos.fechas), capas = [], i = 0, corriendo = true, temporizador = null;
  fechas.forEach(function (f, k) {
    var capa = L.tileLayer(datos.plantilla.replace('{fecha}', f), {
      maxNativeZoom: 6, maxZoom: 8, opacity: k === fechas.length - 1 ? 0.85 : 0, attribution: k === 0 ? datos.atribucion : ''
    });
    capa.addTo(mapa);
    capas.push(capa);
  });
  var Control = L.Control.extend({
    onAdd: function () {
      var div = L.DomUtil.create('div', 'control-mar');
      div.innerHTML = '<button type="button" class="btn-mar" title="Pausar / reproducir">❚❚</button>' +
                      '<span class="fecha-mar"></span><span class="barra-mar"><span></span></span>';
      L.DomEvent.disableClickPropagation(div);
      return div;
    }
  });
  var control = new Control({ position: 'bottomleft' });
  control.addTo(mapa);
  var cont = control.getContainer(), etiqueta = cont.querySelector('.fecha-mar'),
      boton = cont.querySelector('.btn-mar'), avance = cont.querySelector('.barra-mar span');
  var meses = ['ene', 'feb', 'mar', 'abr', 'may', 'jun', 'jul', 'ago', 'set', 'oct', 'nov', 'dic'];
  function mostrar(k) {
    capas.forEach(function (c, j) { c.setOpacity(j === k ? 0.85 : 0); });
    var p = fechas[k].split('-');
    etiqueta.textContent = parseInt(p[2], 10) + ' ' + meses[parseInt(p[1], 10) - 1] + ' ' + p[0] +
                           (k === fechas.length - 1 ? ' · último' : '');
    avance.style.width = (100 * (k + 1) / fechas.length) + '%';
  }
  function paso() {
    mostrar(i);
    // Pausa más larga en el último cuadro antes de repetir
    temporizador = setTimeout(function () { i = (i + 1) % fechas.length; if (corriendo) paso(); },
                              i === fechas.length - 1 ? 2500 : (datos.intervalo || 900));
  }
  boton.addEventListener('click', function () {
    corriendo = !corriendo;
    boton.textContent = corriendo ? '❚❚' : '▶';
    clearTimeout(temporizador);
    if (corriendo) paso();
  });
  // Encuadre del Pacífico: si el mapa se dibujó cuando su caja aún no tenía tamaño, Leaflet
  // calcula un zoom enorme. Se vuelve a encuadrar cada vez que la caja cambia de tamaño,
  // hasta que la persona mueva o acerque el mapa por su cuenta.
  var limites = L.latLngBounds([[-25, -180], [15, -68]]), tocado = false, cont0 = mapa.getContainer();
  cont0.addEventListener('pointerdown', function () { tocado = true; });
  cont0.addEventListener('wheel', function () { tocado = true; }, { passive: true });
  function encuadrar() {
    if (!cont0.offsetWidth) return;
    mapa.invalidateSize();
    if (!tocado) mapa.fitBounds(limites, { animate: false });
  }
  if (window.ResizeObserver) new ResizeObserver(encuadrar).observe(cont0);
  setTimeout(function () { encuadrar(); paso(); }, 400);
}

// Los gráficos que se dibujaron en una pestaña oculta quedan «comidos»: se redimensionan
// al mostrarse la pestaña o cambiar el tamaño de la ventana.
function ensoRedimensionarGraficos() {
  if (!window.echarts) return;
  document.querySelectorAll('.echarts4r').forEach(function (el) {
    var g = echarts.getInstanceByDom(el);
    if (g && el.offsetWidth > 0) { try { g.resize(); } catch (e) {} }
  });
}
$(document).on('shown.bs.tab shown.bs.collapse', function () { setTimeout(ensoRedimensionarGraficos, 80); });
$(window).on('resize', function () { setTimeout(ensoRedimensionarGraficos, 150); });
$(document).on('shiny:value', function (ev) {
  if (ev.target && ev.target.classList && ev.target.classList.contains('echarts4r')) setTimeout(ensoRedimensionarGraficos, 120);
});
