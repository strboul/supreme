
context("test-get")

test_that("server exprs elems", {

  f <- file.path("data", "server-exprs-elems.Rtest")
  p <- read_srcfile(f)
  s <- get_server_block(p)
  sm <- get_block_modules(s)

  expect_equal(
    sm,
    list("SomeTabServer", "BarPlotPanelServer", "CustomerListPanelServer",
         "ObservedPanelServer", "ConditionalItemsServer", "ConditionalConditionalItems1Server",
         "ConditionalConditionalItems2Server", "DetailsButtonServer")
  )

})

test_that("multiple server definition in a file", {

  f <- file.path("data", "multiple-server-definition.Rtest")
  p <- read_srcfile(f)

  ## Here, this function is permissive that will return the multiple definitions of
  ## server.
  server <- get_server_block(p)
  expect_length(server, 2)
})

