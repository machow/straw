context("match_expr - simple expressions")

match_simple <- function(text) {
  parsed <- parse(text = text, keep.source = TRUE)
  # remove root expression, since its there for when the code
  # has multiple top level expressions (e.g. "1 + 1; 2 + 2")
  g <- create_parse_graph(parsed)
  g_ast <- create_parse_graph(parsed, 'ast')

  straw::match_expr(g, g_ast, g[2,], g_ast[2,])
}

test_that("binary operator", {
  match_simple("1 + 1")

})

test_that("unary operator", {
  match_simple("!1")
})

test_that("$ index with literal", {
  match_simple("a$b")
})

test_that("$ index with string", {
  match_simple("a$'b'")
})

test_that("[ index with empty args", {
  match_simple("x[]")
  match_simple("x[,]")
  match_simple("x[1,,]")
})

test_that("-> right assignment", {
  skip("TODO: ")
  match_simple("1 -> a")
})

test_that("function def with ellipsis ...", {
  match_simple("function(a, ...) 1")
})

test_that("function call with empty args", {
  skip("TODO: ")
  match_simple("f(,)")
})

test_that("= simple assignment", {
  match_simple("a = 1 + 1")
})
