box::use(
  shiny[testServer, reactiveVal],
  testthat[...],
  config = config[get]
)
box::use(
  app/view/filters/filter_picker[...]
)
box::use(
  app/logic/data_utils[prepare_count_data]
)

test_that(
  desc = "picker filter server works",
  code = {
    cfg <- config$get(file = "./data/test_config.yml")
    count_data <- prepare_count_data(cfg)

    Input_data <- reactiveVal(count_data)

    testServer(
      app = server,
      args = list(colname = "fraction", Input_data = Input_data),
      expr = {
        session$setInputs(picker = c("A", "B", "C", "D", "E", "F", "G", "H"))
        expect_equal(Filtered_data(), count_data)

        session$setInputs(picker = c("A", "D"))
        expect_equal(nrow(Filtered_data()$df), 16)
        expect_equal(as.character(unique(Filtered_data()$df$fraction)), c("A", "D"))

        session$setInputs(picker = c())
        expect_equal(nrow(Filtered_data()$df), 0)
      }
    )
  }
)
