
function make_node(id){
  return { data: { id: id, label: id } }
}

function make_edge(src, trgt){
  var id = `${src}_x_${trgt}`
  return { data: { id: id, source: src, target: trgt, label: id}}
}

function init(data, style, el){
  cy = cytoscape({
    container: el,
    boxSelectionEnabled: false,
    //autounselectify: true,
    layout: {
      name: 'dagre',
      rankDir: 'TB',
      nodeDimensionsIncludeLabels: true
    },
    style: style,
    elements: data
  });

  return cy;
}

var default_style = [
  {
    selector: 'node',
    css: {
      'label': 'data(text)',
      'text-valign': 'center',
      'text-halign': 'center',
      //'background-color': '#11479e'
    }
  },
  {
    selector: 'node[?highlight]',
    css: {
      'background-color': 'orange'
    }
  },
  {
    selector: 'edge',
    css: {
      'font-size': 10
    }
  },
  {
    selector: ':active',
    css: {
      'background-color': 'red'
    }
  }
]


HTMLWidgets.widget({

  name: 'cytoscape_dagre',

  type: 'output',

  factory: function(el, width, height) {

    var self = {
      renderValue,
      resize,
      cy: undefined
    };

    return self;


    function renderValue(x) {
      var elements = typeof x.elements == "string"? JSON.parse(x.elements) : x.elements;
      console.log(elements)
      var graph_style = x.style ? x.style : default_style;
      if (!this.cy) {
        var cy = this.cy = init(elements, graph_style, el);
        cy.on('click', 'node', function(evt) {
          var data = evt.target.data()
          console.log(data)
          var message = {id: data.id, data: data};
          if (window.Shiny)
            window.Shiny.onInputChange(el.id + "__click", message);
          console.log(evt)
        })

      } else {
        this.cy.elements().remove();
        this.cy.add(elements);
        this.cy
          .layout({
            name: 'dagre',
            rankDir: 'TB',
            nodeDimensionsIncludeLabels: true
          })
          .run()
      }

      //el.innerText = x.message;

    }

    function resize(width, height) {

      // TODO: code to re-render the widget with a new size

    }
  }
});
