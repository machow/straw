Shiny.addCustomMessageHandler("ace-mark", aceMark)

var crntMarkerId

function aceMark (evt) {
  console.log(evt)
  var Range = ace.require('ace/range').Range
  var importCssString = ace.require("ace/lib/dom").importCssString
  
  importCssString(`.highlight {background-color: orange; position: absolute;}`)
  
  if (evt.length) {
    if (crntMarkerId) editor__acecode.session.removeMarker(crntMarkerId)
    var r = new Range(evt[0], evt[1], evt[2], evt[3])
    crntMarkerId = editor__acecode.session.addMarker(r, "highlight", "text")
  }
}
/*
var importCssString = ace.require("ace/lib/dom").importCssString
importCssString(`.errorhighlight {background-color: aquamarine;}`)

var Range = ace.require('ace/range').Range
r = new Range(0, 1, 1, 3)
editor__acecode.session.addMarker(r, "errorhighlight", "text")
*/