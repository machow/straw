library(straw)

exp <- parse(text = "if (x) 2 else a$'b'")
g <- create_parse_graph(parse(text = "if (x) 2 else a$'b'"))
plot(g, layout = layout.reingold.tilford)

g_ast <- create_parse_graph(exp, type = "ast")
plot(g_ast, layout = layout.reingold.tilford)

# find code corresponding to `$` call
match_path(g, g_ast, V(g_ast)[label == "$"]$id)

library(visNetwork)

V(g)$shape <- 'circle'
V(g)$x <- rev(1:length(V(g)))


visIgraph(g, idToLabel = FALSE) %>%
  visHierarchicalLayout(blockShifting = FALSE, edgeMinimization = FALSE)
