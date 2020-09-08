library(purrr)
library(magrittr)
library(ggraph)
library(dplyr)
library(tidygraph)
library(tidyr)
library(tibble)

# takes an call object, returns an enhanced tree representation (list) with
#   * node id
#   * parent_id
#   * node: call object for node
#   * text: simplified text for nodes that atomic or name types
create_ast_graph <- function(x, acc = NULL, parent_id = 0) {
  if (is.null(acc)) {
    acc <- new.env()
    acc$crnt_n <- 0
    acc$nodes <- list()
  }

  node_id <- acc$crnt_n <- acc$crnt_n + 1
  acc$nodes[[acc$crnt_n]] <- list(
    node = x,
    id = node_id,
    parent = parent_id,
    text = if (is.atomic(x) | is.name(x)) as.character(x) else ""
    )

  if (is.atomic(x) | is.name(x)) {
    NULL
  } else if (is.call(x) | is.expression(x)) {
    for (ii in 1:length(x)) {
      create_ast_graph(x[[ii]], acc, node_id)
    }
  } else {
    stop("whoops", x)
  }
  return(acc$nodes)
}


# convenience function to return data.frames for parse data and ast graph
create_graph_dfs <- function(x) {
  p <- parse(text = x)
  list(
    parsed = getParseData(p) %>% mutate(ast_id = ifelse(parent == 0, 1, NA)),
    # for some reason, parse adds extra call at root of call tree
    ast = create_ast_df(as.call(p)[[1]])
    )
}

# Converts
to_tbl_graph <- . %>%
  mutate(
    new_id = 1:n(),
    from = new_id[match(parent, id)],
    to = new_id
    ) %>%
  { tbl_graph(., slice(., -1))}

get_root_dist <- function(df) {
  df %>%
    to_tbl_graph() %>%
    mutate(dist_to_root = node_distance_from(node_is_root())) %>%
    as_tibble() %>%
    select(id, dist_to_root) %>%
    right_join(df, "id")
}

plot_tree <- . %>%
  map(~ to_tbl_graph(.)) %>%
  imap(~ mutate(.x, graph = .y)) %>%
  setNames(c()) %>%
  {do.call(bind_graphs, .)} %>%
  {
  ggraph(., 'tree') +
    geom_node_point() +
    geom_edge_link(aes(), arrow = arrow()) +
    geom_node_label(aes(label = text))
  }


# Graph matching ========

link_row <- function(l, row_num) {
  par_row <- l$parsed %>% filter(dist_to_root == row_num)
  ast_row <- l$ast %>% filter(dist_to_root == row_num)

  par_row %>%
    left_join(
      select(l$parsed, id, parent_ast_id = ast_id),
      c("parent" = "id")
      ) %>%
    split(.$parent_ast_id) %>%
    lapply(function(x) link_nodes(x, l$ast %>% filter(parent == x$parent_ast_id[1]))) %>%
    bind_rows()
}


link_nodes <- function(par_row, ast_row) {
  n_args <- nrow(ast_row)
  call_symbol <- ast_row$text[1]

  par_row_expr <- par_row %>% filter(token == "expr")
  if (nrow(par_row_expr) == n_args) {
    par_ids <- par_row_expr$id
  } else if (n_args - nrow(par_row_expr) == 1) {
    par_ids <- c(with(par_row, id[match(call_symbol, text)]), par_row_expr$id)

  } else if (call_symbol == "$") {
    par_ids <- c(
      with(par_row, id[match(call_symbol, text)]),
      par_row_expr$id,
      with(par_row, id[length(id)])
      )
  } else {
    stopifnot(FALSE, "mismatch")
  }
  data_frame(parsed_id = par_ids, ast_id = ast_row$id)
}

add_links <- function(l, row_num) {
  indx <- link_row(l, row_num)
  l$parsed[match(indx$parsed_id, l$parsed$id), 'ast_id'] <- indx$ast_id
  l
}

create_graph_dfs("f(a =1, 2, ...) + g('a' = 'b')") %>%
#create_graph_dfs('d$"x"') %>%
  map(~ get_root_dist(.)) %>%
  add_links(0) %>%
  add_links(1) %>%
  add_links(2) %>%
  add_links(3) %>%
  {
    link <- .$parsed %>% filter(!is.na(ast_id)) %>% rownames_to_column()
    .$parsed <- .$parsed %>% mutate(link_color = match(id, link$id))
    .$ast <- .$ast %>% mutate(link_color = match(id, link$ast_id))
    .
  } %>%
  map(~ to_tbl_graph(.)) %>%
  imap(~ mutate(.x, graph = .y)) %>%
  setNames(c()) %>%
  {do.call(bind_graphs, .)} %>%
  {
  ggraph(., 'tree') +
    geom_node_point(aes()) +
    geom_edge_link(aes(), arrow = arrow()) +
    geom_node_label(aes(label = text, fill = factor(link_color)))
  }

create_graph_dfs("{1 + 1; 2 + 2; { 3 + 3; };}") %>% plot_tree()
create_graph_dfs("1 + 1") %>% plot_tree()




# Alternative approach ================================
create_graph_dfs("1 + {2 + 3}")
ast <- create_ast(quote(1 + {2 + 3}))
p <- getParseData(parse(text = "1 + {2 + 3}"))
ast[[1]]
ast[[1]]
p[[1]][[2]]
# Call from parse object idiosyncracies

as.call(quote(1 + 1))                # 1 + 1
as.call(parse(text = "1 + 1"))       # (1 + 1)()
as.call(quote(f(1)))                 # f(1)
as.call(parse(text = "f(1);"))       # f(1)()        # note: outer call
as.call(parse(text = "f(1); g(2);")) # f(1)(g(2))    # note: two root calls
as.call(parse(text = "{f(1); g(2);}")) # f(1)(g(2))    # note: two root calls




# Right associativity (seems fine) =========
out <- create_graph_dfs("a^5^6")
plot.parser(out$parsed)
plot.parser(out$ast)


pd <- getParseData(parse(text = "if (1) {2;3} else {4}"))
pd <- getParseData(parse(text = "`+`({1;2}, 1)"))
plot.parser(pd)


# Notes
#   * is.call(node) to see if should go deeper
#   * is.atomic(node) to see if basic data structure (e.g. number)
#   * pryr checks for pairlists. Why?
# Types of calls
#   * standard,  e.g. f(1, 2) -> _ ( _ , _ )
#   * unary op,  e.g. -1 -> - _
#   * binary op, e.g. 1 > 2 -> _ > _
#   * keyword,   e.g. if (1) 2 else 3 -> if ( _ ) _ else _
#   * brackets,  e.g. { 1; 2 } -> { _ ; _ }
#   * parentheses, e.g. ( 1 ) -> ( _ )
#   * index, e.g. a[ ] -> _ [ _ ]
# Steps to map to AST from parse data
#   1. start at root AST node (call ast), and PD node (call pd)
#   2a. if is.atomic(ast), build atomic node by returning ast[1]. Otherwise...
#   2b. if is.call(ast) and is.name(ast[[1]]), then ast[[1]] is child token of pd
#     i. standard call will make it the first child token
#    ii. binary will be in the middle (so easier to search for ast[[1]])
#   2c. if is.call(ast) and is.call(ast[[1]]), then it is first child of pd
#   3. Get all child exprs of pd (in order), move one matching ast[[1]] to front
#   4. You have matched up ast to pd! Recurse
