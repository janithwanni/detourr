HTMLWidgets.widget({
  name: "show_scatter_2d",
  type: "output",

  factory: function (el, width, height) {
    let scatter = new show_scatter_2d.DisplayScatter2d(el, width, height);
    return {
      renderValue: function (x) {
        scatter.renderValue(x);
      },
      resize: function (width, height) {
        scatter.resize(width, height);
      },
      s: scatter,
    };
  },
});

if(HTMLWidgets.shinyMode) {
  // register shiny callbacks
  Shiny.addCustomMessageHandler("add-points", function(x) {
    var widget = HTMLWidgets.find(`#${x.id}`);
    try{
      var scatter = widget.s;
      scatter.addPoints(
        x.data,
        x.config.colour,
        x.config.size,
        x.config.alpha
      );
    } catch (error) {
      console.error(`Could not find detour widget ${x.id}`)
    }
  })

  Shiny.addCustomMessageHandler("add-edges", function(x) {
    var widget = HTMLWidgets.find(`#${x.id}`);
    try{
      var scatter = widget.s;
      scatter.addEdges(x.edges, x.config.color);
    } catch (error) {
      console.error(`Could not find detour widget ${x.id}`)
    }
  })
}
