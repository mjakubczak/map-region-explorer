box::use(
  shiny[testServer, reactiveVal],
  testthat[...],
  config = config[get]
)
box::use(
  app/view/filters/filter_slider[...]
)
box::use(
  app/logic/data_utils[prepare_count_data]
)

test_that(
  desc = "slider filter server works", 
  code = {
    cfg <- config$get(file = "./data/test_config.yml")
    count_data <- prepare_count_data(cfg)
    
    Input_data <- reactiveVal(count_data)
    
    testServer(
      app = server, 
      args = list(colname = "count", Input_data = Input_data), 
      expr = {
        session$setInputs(slider = c(25, 994))
        expect_equal(Filtered_data(), count_data)
        
        session$setInputs(slider = c(254, 312))
        expect_equal(nrow(Filtered_data()$df), 4)
        expect_true(all(Filtered_data()$df$count >= 254))
        expect_true(all(Filtered_data()$df$count <= 312))
        
        session$setInputs(slider = NULL)
        expect_is(
          object = attr(
            x = try(
              expr = Filtered_data(),
              silent = TRUE
            ), 
            which = "condition"
          ), 
          class = "shiny.silent.error"
        )
      }
    )
  }
)
