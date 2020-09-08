#' @import htmlwidgets
#' @export
cyto_tree <- function(g, width = NULL, height = NULL) {

  # pass the data and settings using 'x'
  x <- list(
    data = g,
    settings = list()
  )

  # create the widget
  htmlwidgets::createWidget("cyto_tree", x, width = width, height = height)
}
