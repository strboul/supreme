
test_that("graph test", {

  expect_error(
    graph(supreme(src_yaml(example_yaml())), fields = c("input", "output", "none", "mone")),
    regexp = "[supreme] unknown `fields` supplied: \"none\", \"mone\"",
    fixed = TRUE
  )

})

test_that("graph_create_general_directives", {
  expect_equal(
    graph_create_general_directives(list(
      direction = "down",
      font = "Arial",
      fontSize = 11,
      padding = 8
    )),
    "#direction: down\n#font: Arial\n#fontSize: 11\n#padding: 8"
  )
  expect_error(graph_create_general_directives(list(1,2,3)))
  expect_error(graph_create_general_directives(list(a = "namely", "nameless")))
  expect_error(graph_create_general_directives(NULL))
})


test_that("graph_generate_custom_classifier", {
  {
    set.seed(1234)
    cls1 <- graph_generate_custom_classifier("my_great_MoDule123_21")
    cls2 <- graph_generate_custom_classifier("server",
                                             list("fill" = "#8f8", "italic", "dashed"))
  }
  expect_equal(
    cls1,
    list(
      original = "my_great_MoDule123_21",
      classifier = "mygreatmodulepveloixfzdbgsjn",
      classifier.str = "#.mygreatmodulepveloixfzdbgsjn: fill=#fff"
    )
  )
  expect_equal(
    cls2,
    list(
      original = "server",
      classifier = "serverontydvhcuebwxkr",
      classifier.str = "#.serverontydvhcuebwxkr: fill=#8f8 italic dashed"
    )
  )
})


test_that("graph_create_node", {
  x <- list(
    list(
      name = "childModuleA",
      input = c("input.data", "reactive"),
      output = c("output1", "output2"),
      return = "ret",
      calling_modules = list(
        list("grandChildModule1Server" = "grandChildModule1UI"),
        list("grandChildModule2Server" = "grandChildModule2UI")
      )
    )
  )
  ## create a node with a classifier:
  set.seed(2019)
  cls <- graph_generate_custom_classifier(x[[1]][["name"]])[["classifier"]]
  ## disable 'centre' because it breaks the text output:
  node <- graph_create_node(x[[1]], classifier = cls, centre = FALSE)
  expect_equal(
    unlist(strsplit(node, "\\|")),
    c("[<childmoduleayjemqlsiwnahcgo> childModuleA ", " ▹ input.data;▹ reactive ",
      " ○ output1;○ output2 ", " ◻ ret ", " grandChildModule1Server;<grandChildModule1UI>;grandChildModule2Server;<grandChildModule2UI>]"
    )
  )

  ## with some missing fields:
  y <- list(list(name = "childModuleB", input = "data"))
  node_incomplete <- graph_create_node(y[[1]])
  expect_equal(
    unlist(strsplit(node_incomplete, "\\|")),
    c("[ childModuleB ", " ▹ data]")
  )

})


test_that("graph_create_edge", {
  x <- list(
    list(
      name = "childModuleA",
      input = c("input.data", "reactive"),
      output = c("tbl1", "tbl2"),
      return = "ret",
      calling_modules = "grandChildModule1"
    ),
    list(
      name = "childModuleB",
      input = NULL,
      calling_modules = NULL
    )
  )
  expect_equal(graph_create_edge(x[[1]]), "[childModuleA]->[NULL]")
  expect_null(graph_create_edge(x[[2]]))
})


test_that("test graph styles - errors", {

  sp <- supreme(src_yaml(example_yaml()))

  expect_error(
    graph(sp, styles = list("xx")),
    regexp = "[supreme] `styles` must be a \"named list\" object",
    fixed = TRUE
  )

  expect_error(
    graph(sp, styles = list(server = "xx")),
    regexp = "[supreme] objects inside the `styles` argument must be a list, see the element: 1",
    fixed = TRUE
  )

  expect_error(
    graph(sp, styles = list(a_non_existing_module = list("dashed"))),
    regexp = "[supreme] module names specified in `styles` cannot be found: \"a_non_existing_module\"",
    fixed = TRUE
  )

})


test_that("test graph options - errors", {

  sp <- supreme(src_yaml(example_yaml()))

  expect_error(
    graph(sp, options = list(1)),
    regexp = "[supreme] `options` must be a \"named list\" object",
    fixed = TRUE
  )

})


test_that("test graph styles (test nomnoml text with hashing)", {
  {
    set.seed(2019)
    graph_supreme_yaml <- graph(supreme(src_yaml(example_yaml())), styles = list(
      "server" = list(fill = "#ff0", "underline", "bold"),
      "module_modal_dialog" = list(fill = "lightblue", "dashed", visual = "note")
    ))
  }
  expect_identical(
    digest::digest(graph_supreme_yaml[["x"]][["code"]]),
    "6bca5905defae1eafb12cbe00be94535"
  )
})


test_that("test graph options (test nomnoml text with hashing)", {
  {
    set.seed(2019)
    graph_supreme_yaml1 <- graph(
      supreme(src_yaml(example_yaml())),
      options = list(
        direction = "right",
        fontSize = 10,
        title = "Model application"
      ))
  }
  expect_identical(
    digest::digest(graph_supreme_yaml1[["x"]][["code"]]),
    "c1dfc6d6a1850cdcc1255b40d9abfc00"
  )
  ## non default overriding options:
  {
    set.seed(2019)
    graph_supreme_yaml2 <- graph(supreme(src_yaml(example_yaml())),
                                 options = list(bendSize = 5))
  }
  expect_identical(
    digest::digest(graph_supreme_yaml2[["x"]][["code"]]),
    "c37cf2642a9482a603013937890ff516"
  )
})

