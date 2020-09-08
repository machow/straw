#' Produce an ast or parse graph of an expression
#'
#' @param x values to be matched
#' @param table values to be matched against
#' @return list with vector of matching table indices for each element in x
#' @examples
#' multimatch(1:3, c(1,2,3,1,2,3))
#'
#' @export
multimatch <- function(x, table) {
  backref <- match(table, x)

  out <- vector("list", length(x))
  for (ii in 1:length(backref)) {
    parent_pos <- backref[ii]
    parent <- out[[backref[ii]]]

    if (is.na(parent_pos)) next

    if (is.null(parent))
      out[[parent_pos]] <- ii
    else out[[parent_pos]] <- c(parent, ii)
  }

  out
}

#' @export
path_to_root <- function(g_ast, row_num) {
  dst <- g_ast[row_num, ]
  if (is.na(dst$parent)) return(dst$row_num)
  c(dst$row_num, path_to_root(g_ast, match(dst$parent, g_ast$id)))
}


#' @export
tbl_to_cyto <- function(d, to_json = FALSE) {
  matches <- match(d$parent, d$id)
  edge_pairs <- dplyr::tibble(
    source = d$parent,
    target = d$id,
    id = paste0('edge-', 1:nrow(d))
  )[-1,]

  #d$id <- as.character(d$id)

  d_nodes <- dplyr::distinct(d, id, .keep_all = TRUE)
  elements <- c(
    purrr::transpose(dplyr::select(d_nodes, -parent)),
    purrr::transpose(as.data.frame(edge_pairs))
  )

  out <- lapply(elements, function(row) list(data = row))

  if(to_json) jsonlite::toJSON(out, auto_unbox = TRUE) else out
}
