
context("test-example")

skip("till fixed")

file <- src_file(example_app_path())
yaml <- src_yaml(example_yaml())

# Remove 'src' field for these tests because they aren't relevant.
remove_src_field <- function(x) lapply(x, function(e) { e[["src"]] <- NULL; e })

file_ <- remove_src_field(file)
yaml_ <- remove_src_field(yaml)

supreme_file <- supreme(file)
supreme_yaml <- supreme(yaml)

#' Custom expectation for the examples
#'
#' Since the order of the list elements can be different, this custom expectation
#' checks the list elements where their names are the same.
#'
#' @references
#' Source: \url{https://testthat.r-lib.org/articles/custom-expectation.html}
#' @noRd
expect_example_equivalent <- function(object, expected) {
  expect_identical(length(object), length(expected))
  for (i in seq_along(object)) {
    obj <- object[[i]]
    obj_name <- obj[["name"]]
    exp <- unlist(lapply(expected, function(e) {
      if (e[["name"]] == obj_name) {
        e
      }
    }), recursive = FALSE)
    label_msg <- function(input, n_i) paste0(input, "[[", n_i, "]]")
    ## expect_equivalent because we don't check attributes (e.g. class)
    expect_equivalent(
      obj, exp,
      label = label_msg(deparse(substitute(object)), i),
      expected.label = label_msg(deparse(substitute(expected)), i)
    )
  }
}

test_that("example src_* outputs are equivalent", {
  expect_example_equivalent(file_, yaml_)
})

test_that("example as.data.frame.supreme outputs are equal", {

  ## remove the 'src' column, we don't need to check..
  supreme_file_df_ <- subset(as.data.frame(supreme_file), select = -c(src))
  supreme_yaml_df_ <- subset(as.data.frame(supreme_yaml), select = -c(src))

  expect_equal(supreme_file_df_, supreme_yaml_df_)

})
