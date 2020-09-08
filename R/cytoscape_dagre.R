#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
cytoscape_dagre <- function(elements, style = NULL, width = NULL, height = NULL, elementId = NULL) {

  # forward options using x
  x = list(
    elements = elements,
    style = style
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'cytoscape_dagre',
    x,
    width = width,
    height = height,
    package = 'straw',
    elementId = elementId
  )
}

#' Shiny bindings for cytoscape_dagre
#'
#' Output and render functions for using cytoscape_dagre within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a cytoscape_dagre
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name cytoscape_dagre-shiny
#'
#' @export
cytoscape_dagreOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'cytoscape_dagre', width, height, package = 'straw')
}

#' @rdname cytoscape_dagre-shiny
#' @export
renderCytoscape_dagre <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, cytoscape_dagreOutput, env, quoted = TRUE)
}
