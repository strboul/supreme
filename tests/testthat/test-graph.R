
context("test-graph")

test_that("graph test", {
  expect_s3_class(
    graph(supreme(src_file(example_app_path()))),
    c("nomnoml", "htmlwidget")
  )
  expect_s3_class(
    graph(supreme(src_expr(example_expression()))),
    c("nomnoml", "htmlwidget")
  )
  expect_s3_class(
    graph(supreme(src_env(example_environment()))),
    c("nomnoml", "htmlwidget")
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
      " ○ output1;○ output2 ", " ◻ \"ret\" ", " grandChildModule1Server;<grandChildModule1UI>;grandChildModule2Server;<grandChildModule2UI>]"
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

