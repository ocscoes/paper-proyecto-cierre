// fix_plotly_bug.js
export default function FixPlotlyBug() {
  return {
    id: "fix_plotly_bug",
    init: (deck) => {
      const resizePlotsIn = (slide) => {
        if (!slide) return;
        slide.querySelectorAll(".js-plotly-plot, .plotly").forEach((el) => {
          try { window.Plotly?.Plots?.resize(el); } catch(e) {}
        });
      };

      // Llamadas cuando cambia o se redibuja la slide
      deck.on("ready",        () => resizePlotsIn(deck.getCurrentSlide()));
      deck.on("slidechanged", (e) => resizePlotsIn(e.currentSlide));
      deck.on("resize",       () => resizePlotsIn(deck.getCurrentSlide()));
      deck.on("fragmentshown",() => resizePlotsIn(deck.getCurrentSlide()));
      deck.on("fragmenthidden",() => resizePlotsIn(deck.getCurrentSlide()));
    },
  };
}
