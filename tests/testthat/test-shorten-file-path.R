
test_that("can uniquely shorten relative file paths", {

  all_unique <- c(
    file.path("a", "b", "c"),
    file.path("x", "y", "z")
  )
  expect_equal(
    shorten_src_file_path(all_unique),
    c("c", "z")
  )

  ## two duplicated:
  dup2 <- c(
    file.path("a", "b", "c", "d"),
    file.path("x1", "x2", "c", "d"),
    file.path("k")
  )
  expect_equal(
    shorten_src_file_path(dup2),
    c("b/c/d", "x2/c/d", "k")
  )

  dup_long_short_paths <- c(
    file.path("x", "y", "z", "a", "b"),
    file.path("z", "a", "b"),
    file.path("a", "b")
  )
  expect_equal(
    shorten_src_file_path(dup_long_short_paths),
    c("y/z/a/b", "z/a/b", "a/b")
  )

  dup_long_short_paths2 <- c(
    file.path("h1", "h2", "z", "a", "b"),
    file.path("p", "a", "b"),
    file.path("t"),
    file.path("1", "2", "3")
  )
  expect_equal(
    shorten_src_file_path(dup_long_short_paths2),
    c("z/a/b", "p/a/b", "t", "3")
  )

  multiple_dups <- c(
    file.path("h", "x", "y", "z"),
    file.path("p", "y", "z"),
    file.path("z"),
    file.path("C", "B", "A"),
    file.path("D", "C", "B", "A"),
    file.path("1", "2", "3")
  )
  expect_equal(
    shorten_src_file_path(multiple_dups),
    c("x/y/z", "p/y/z", "z", "D/C/B/A", "C/B/A", "3")
  )

  ## no unique:
  none_uniq <- c(
    file.path("x", "y", "z"),
    file.path("x", "y", "z")
  )
  expect_error(
    shorten_src_file_path(none_uniq),
    regexp = "[supreme] the following src path(s) not unique: 'x/y/z'",
    fixed = TRUE
  )

  none_uniq2 <- c(
    file.path("x", "y", "z"),
    file.path("x", "y", "z"),
    file.path("a", "b", "c"),
    file.path("a", "b", "c"),
    file.path("1001")
  )
  expect_error(
    shorten_src_file_path(none_uniq2),
    regexp = "[supreme] the following src path(s) not unique: 'x/y/z', 'a/b/c'",
    fixed = TRUE
  )

})

