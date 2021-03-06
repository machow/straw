# Graph data.frame creating functions ----
# TODO: cleanup flow?
#       handle pairlists
# takes an call object, returns an enhanced tree representation (list) with
#   * node id
#   * parent_id
#   * node: call object for node
#   * text: simplified text for nodes that atomic or name types
create_ast_graph <- function(x, acc = NULL, parent_id = NA) {
  if (is.null(acc)) {
    acc <- new.env()
    acc$crnt_n <- 1
    acc$nodes <- list()
  }

  node_id <- acc$crnt_n
  acc$crnt_n <- acc$crnt_n + 1
  acc$nodes[[node_id]] <- list(
    node = x,
    id = node_id,
    parent = parent_id,
    text = if (is.atomic(x) | is.name(x)) as.character(x) else ""
    )

  if (is.atomic(x) | is.name(x)) {
    NULL
  } else if (is.call(x) | is.expression(x) | is.pairlist(x)) {
    # don't use last node of function def (it's a srcref)
    n_nodes <- if (x[[1]] == "function") length(x) - 1 else length(x)
    for (ii in 1:n_nodes) {
      create_ast_graph(x[[ii]], acc, node_id)
    }
  } else {
    stop("unexpected object type in the ast", x)
  }
  return(acc$nodes)
}


create_ast_df <- function(x) {
  if (is.character(x)) x <- parse(x)
  if (!is.expression(x)) x <- as.expression(substitute(x))
  nodes <- create_ast_graph(x)

  for (ii in 1:length(nodes))
    nodes[[ii]]$node = list(nodes[[ii]]$node)

  dplyr::bind_rows(nodes)
}


# Parse graph to AST graph linking functions -----

#' Produce an ast or parse graph of an expression
#'
#' @param expr An expression (e.g. created by parse(text = "1 + 1"))
#' @param type Generate a full parse graph or ast
#' @return An igraph graph object
#' @examples
#' create_parse_graph(parse(text = "if (x) 2 else a$b"))
#' create_parse_graph(expression(if (x) 2 else a$b), type = "ast")
#'
#' @importFrom igraph graph.tree vertices edges
#' @export
create_parse_graph <- function(expr, type = c("parse", "ast")[1]) {
  if (type == "parse") {
    y = getParseData(expr)
    # need to add a root node at top of df
    header = y[1,]
    header[1,] = NA
    header[1, 'id'] = 0
    header[1, 'text'] = ""
    y = rbind(header, y)
    y$row_num = 1:nrow(y)
  }
  else if (type == "ast") {
    # TODO: replace create_ast_graph w/ something that returns a dataframe
    y = create_ast_df(expr)
    y$token = NA
    y$row_num = 1:nrow(y)
  }
  else {
    stop("type argument should be either 'parse' or 'ast'")
  }

  # find edges
  y$children <- multimatch(y$id, y$parent)
  y$children[sapply(y$children, is.null)] <- list(integer())

  y
}



#' Produce an ast or parse graph of an expression
#'
#' @param g_parse Parse graph generated by create_parse_graph
#' @param g_ast AST graph generated by create_parse_graph
#' @param parse_node node from g_parse to search from
#' @param ast_node node from g_ast to search from
#' @return A data.frame linking the graphs with columns parse_id, ast_id
#' @examples
#' exp <- parse(text = "if (x) 2 else a$'b'")
#' g <- create_parse_graph(parse(text = "if (x) 2 else a$'b'"))
#' plot(g, layout = layout.reingold.tilford)
#'
#' g_ast <- create_parse_graph(exp, type = "ast")
#' plot(g_ast, layout = layout.reingold.tilford)
#'
#' match_expr(g, g_ast, g[2,], g_ast[2,])
#'
#' @export
match_expr <- function(g_parse, g_ast, parse_node, ast_node) {

  # note: could also use %||% from rlang
  ast_children <- g_ast[ast_node$children[[1]],]
  n_ast <- nrow(ast_children)

  if (n_ast == 0) {
    NULL
  } else if (is.pairlist(ast_node$node[[1]])) {
    # pairlist occurs inside function definition, but its children are
    # matched when visiting the function (see block below)
    return(data.frame(parse_row = NULL, ast_row = NULL))
  } else if (ast_children$node[[1]] == "function") {
    # in the ast, function parameters are children of a pairlist,
    # parse node: function ( a = <expr1>, b = <expr2> ... ) <expr>
    #
    parse_children <- g_parse[parse_node$children[[1]],]

    # keep `function`, "(", and body expr
    # "(" will correspond to pairlist in ast
    non_args <- parse_children[c(1,2,nrow(parse_children)),]

    # get ast args from pairlist, to match with parse args
    # Match an expression for params with defaults. Otherwise, the parameter symbol.
    ast_args <- g_ast[ast_children$children[[2]],]

    empty_args <- sapply(
      ast_args$node,
      function(x) is.symbol(x) & x == ""
      )

    # Add two if not empty to get default arg value
    par_symbol_indx <- which(parse_children$token == "SYMBOL_FORMALS")
    args_indx <- ifelse(empty_args, par_symbol_indx, par_symbol_indx + 2)
    args <- parse_children[args_indx,]

    # TODO: remove once unit tested / sure we've covered all cases
    stopifnot(nrow(args) == nrow(ast_args))
    stopifnot(nrow(non_args) == nrow(ast_children))

    expr <- bind_rows(non_args, args)
    ast_children <- bind_rows(ast_children, ast_args)

  } else {
    parse_children <- g_parse[parse_node$children[[1]],]
    # note: most operators are expr, but a literal = is its own kind of token
    expr <- filter(parse_children, token == "expr" | token == "equal_assign")

    n_expr <- nrow(expr) %||% 0

    if (n_expr != n_ast) {
      # lispy calls are the same length (e.g. `+`(1,2))
      # but inline operators, like 1 + 1, will have 1 less expr
      op <- filter(parse_children, text == ast_children$text[1])
      expr <- bind_rows(op, expr)

      # check for use of $ operator (e.g. a$'b'),
      # since 'b' is not treated like an expression there
      if (op$text == "$") {
        # quotes <- c('"', "'")
        # index_val <- paste0(quotes, ast_children$text[n_ast], quotes)
        # indx_op <- filter(parse_children, text %in% index_val)
        indx_op <- parse_children[3,]

        expr <- bind_rows(expr, indx_op)
      } else if (op$text %in% c("[", "[[")) {
        # indexing with [ or [[ can have empty arguments, e.g. x[1,,]
        # empty arguments are included in ast call, but not in parse tree.
        # To link the two trees, we map empty arguments in the AST to their
        # following "," or "]" in the parse tree.
        ast_args <- ast_children[-(1:2),]
        empty_ast <- sapply(
          ast_args$node,
          function(x) is.symbol(x) & x == ""
          )

        # For empty AST nodes, point to delimiter. Otherwise point to preceeding expression.
        delim_node_indx <- which(parse_children$text %in% c(",", "]"))
        args_indx <- ifelse(empty_ast, delim_node_indx, delim_node_indx - 1)

        expr <- bind_rows(
          expr[1:2,],                       # non args
          parse_children[args_indx,]        # args
          )
      }

      stopifnot(nrow(expr) == n_ast)
    }
  }
  data.frame(parse_row = expr$row_num, ast_row = ast_children$row_num)
}


#' Produce an ast or parse graph of an expression
#'
#' @param g_parse Parse graph generated by create_parse_graph
#' @param g_ast AST graph generated by create_parse_graph
#' @param id id of ast node to find
#' @return A path of vectors in g_parse leading to corresponding ast node
#' @examples
#' exp <- parse(text = "if (x) 2 else a$'b'")
#' g <- create_parse_graph(parse(text = "if (x) 2 else a$'b'"))
#' plot(g, layout = layout.reingold.tilford)
#'
#' g_ast <- create_parse_graph(exp, type = "ast")
#' plot(g_ast, layout = layout.reingold.tilford)
#'
#' # find code corresponding to `$` call
#' match_path(g, g_ast, V(g_ast)[label == "$"]$id)
#' match_path(g, g_ast, 8)
#' @importFrom igraph V shortest_paths
#' @export
match_path <- function(g_parse, g_ast, row_num) {
  ast_path <- rev(path_to_root(g_ast, row_num))
  nodes <- g_ast[ast_path,]
  parse_indx <- c(1, rep(NA, nrow(nodes) - 1))
  for (ii in 2:nrow(nodes)) {
    crnt_parse_node <- g_parse[parse_indx[ii-1],]
    node <- nodes[ii-1,]

    matches <- match_expr(g_parse, g_ast, crnt_parse_node, node)

    # set index for next node
    row_match <- match(nodes[ii,]$row_num, matches$ast_row)
    parse_indx[ii] <- matches$parse_row[row_match]
  }

  g_parse[parse_indx,]
}

#' @export
enhance_ast <- function(exp = NULL, g = NULL, g_ast = NULL) {
  if (!is.null(exp)) {
    if (!is.expression(exp)) exp <- parse(text = exp, keep.source = TRUE)

    g <- create_parse_graph(exp)
    g_ast <- create_parse_graph(exp, type = 'ast')
  } else if (xor(is.null(g), is.null(g_ast))) {
    stop("Both g and g_ast must either be NULL, or specified")
  }

  n_ast_nodes <- nrow(g_ast)
  all_matches <- data.frame(
    parse_row = rep(NA, n_ast_nodes),
    ast_row =   rep(NA, n_ast_nodes)
    )

  all_matches[1,] <- 1
  max_match <- 1
  ii <- 1
  while (max_match < n_ast_nodes) {
    crnt_ast <- g_ast[all_matches[ii, 'ast_row'],]
    crnt_parse <- g[all_matches[ii, 'parse_row'],]

    if (length(crnt_ast$children[[1]]) == 0) {
      ii <- ii + 1
      next
    }

    matches <- match_expr(g, g_ast, crnt_parse, crnt_ast)
    n_matches <- nrow(matches)
    if (n_matches > 0)
      all_matches[(max_match + 1):(max_match+n_matches),] <- matches

    max_match <- max_match + n_matches
    ii <- ii + 1
  }

  parse_cols <- c('line1', 'col1', 'line2', 'col2')
  ordered_g <- g[all_matches$parse_row,]
  for (k in parse_cols) {
    g_ast[all_matches$ast_row, k] <- ordered_g[,k]
  }
  g_ast$parse_row_num <- ordered_g$row_num

  g_ast
}

#' Produce an ast or parse graph of an expression
#'
#' @param g_parse Parse graph generated by create_parse_graph
#' @param g_ast AST graph generated by create_parse_graph
#' @param id id of ast node to find
#' @return A path of vectors in g_parse leading to corresponding ast node
#' @examples
#' exp <- parse(text = "if (x) 2 else a$'b'")
#' g <- create_parse_graph(parse(text = "if (x) 2 else a$'b'"))
#' plot(g, layout = layout.reingold.tilford)
#'
#' g_ast <- create_parse_graph(exp, type = "ast")
#' plot(g_ast, layout = layout.reingold.tilford)
#'
#' # find code corresponding to `$` call
#' plot_match(g, g_ast, 8)
#'
#' @export
plot_match <- function(g_parse, g_ast, id) {

  parse_path <- match_path(g_parse, g_ast, id)$row_num

  edge_pairs <- with(g_parse, cbind(match(parent, id), row_num)[-1,])
  h <- igraph::graph.tree(0) +
    igraph::vertices(id = g_parse$row_num, label = g_parse$text) +
    igraph::edges(c(t(edge_pairs)))

  igraph::V(h)[parse_path]$color <- 'lightblue'
  plot(h, layout = igraph::layout.reingold.tilford)
}
