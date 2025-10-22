window.RevealTest = function () {
  return {
    id: "RevealTest",
    init: function (deck) {
      Reveal.addEventListener( 'slidechanged', function( event ) {
        let plotlyPlots = document.getElementsByClassName("js-plotly-plot");
        for (let i = 0; i < plotlyPlots.length; i++) {
          _Plotly.Plots.resize(plotlyPlots[i]);
        };
      });
    },
  };
};