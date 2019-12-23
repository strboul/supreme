
context("test-example: the examples are the equivalents")


supreme_file <- supreme(src_file(example_app_path()))
supreme_yaml <- supreme(src_yaml(example_yaml()))


test_that("example supreme$data outputs are equivalent", {
  expect_equivalent(supreme_file$data, supreme_yaml$data)
})


test_that("example as.data.frame outputs are equal", {
  expect_equal(as.data.frame(supreme_file), as.data.frame(supreme_yaml))
})


test_that("example supreme graphs are equal (test nomnoml code)", {
  ## set seed due to the random classifier:
  {set.seed(2019); graph_supreme_file <- graph(supreme_file)}
  {set.seed(2019); graph_supreme_yaml <- graph(supreme_yaml)}
  expect_equal(
    graph_supreme_yaml[["x"]][["code"]],
    graph_supreme_file[["x"]][["code"]]
  )
})

