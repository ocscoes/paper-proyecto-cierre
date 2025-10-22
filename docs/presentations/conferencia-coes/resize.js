// resize.js
(function () {
  const log = (...a) => { if (window.console) console.log("[plotly-resize]", ...a); };

  function isPlotly(el) { return el && el.classList && el.classList.contains("js-plotly-plot"); }
  function findPlots(root) {
    return Array.from((root || document).querySelectorAll(".js-plotly-plot"));
  }
  function resizeEl(el) {
    if (window.Plotly && isPlotly(el)) {
      try { window.Plotly.Plots.resize(el); } catch (e) {}
    }
  }
  function resizeAllIn(slide) {
    if (!slide) return;
    const plots = findPlots(slide);
    plots.forEach((el) => {
      resizeEl(el);
      requestAnimationFrame(() => resizeEl(el));
      setTimeout(() => resizeEl(el), 120);
      setTimeout(() => resizeEl(el), 300);
    });
  }
  function currentSlide() {
    return (window.Reveal && Reveal.getCurrentSlide && Reveal.getCurrentSlide()) || null;
  }

  function hookRevealEvents() {
    if (!window.Reveal) return false;
    Reveal.on("ready",        () => resizeAllIn(currentSlide()));
    Reveal.on("slidechanged", (e) => resizeAllIn(e.currentSlide));
    Reveal.on("resize",       () => resizeAllIn(currentSlide()));
    Reveal.on("fragmentshown",() => resizeAllIn(currentSlide()));
    Reveal.on("fragmenthidden",() => resizeAllIn(currentSlide()));
    return true;
  }

  function observeInsertions() {
    const target = document.querySelector(".reveal .slides") || document.body;
    if (!target) return;
    const mo = new MutationObserver((mutations) => {
      mutations.forEach((m) => {
        m.addedNodes && m.addedNodes.forEach((n) => {
          if (isPlotly(n)) resizeEl(n);
          // if a chunk wrapper gets added, look inside
          if (n.querySelectorAll) {
            n.querySelectorAll(".js-plotly-plot").forEach((el) => resizeEl(el));
          }
        });
      });
    });
    mo.observe(target, { childList: true, subtree: true });
  }

  function start() {
    log("init");
    // Try immediately, then after common widget init delays
    resizeAllIn(currentSlide());
    setTimeout(() => resizeAllIn(currentSlide()), 80);
    setTimeout(() => resizeAllIn(currentSlide()), 240);
    window.addEventListener("resize", () => resizeAllIn(currentSlide()));
    observeInsertions();
    const ok = hookRevealEvents();
    if (ok) log("Reveal hooks attached");
    if (window.Plotly) log("Plotly present");
  }

  if (document.readyState === "complete" || document.readyState === "interactive") {
    start();
  } else {
    document.addEventListener("DOMContentLoaded", start);
  }
})();
