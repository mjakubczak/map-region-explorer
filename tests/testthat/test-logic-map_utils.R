box::use(
  testthat[...],
  config = config[get]
)
box::use(
  app/logic/map_utils[...],
  app/logic/data_utils[...]
)

test_that(
  desc = "map utils work fine",
  code = {
    cfg <- config$get(file = "./data/test_config.yml")
    
    count_data <- prepare_count_data(cfg)
    location_data <- prepare_location_data(cfg)
    
    merged_data <- join_summarized_counts_and_locations(
      count_data = count_data,
      location_data = location_data
    )
    
    p <- generate_map_plot(
      polygon_data = merged_data$df,
      x_var = merged_data$x_col,
      y_var = merged_data$y_col,
      fill_var = merged_data$fill_col,
      group_var = merged_data$group_col
    )
    
    expect_is(p, "ggplot")
    expect_equal(length(p$layers), 1)
    expect_is(p$layers[[1]]$geom, "GeomPolygon")
    expect_null(p$labels$title)
    expect_equal(p$labels$group, "label")
    expect_equal(p$labels$fill, ".total")
    
    expect_error(generate_map_plot(
      polygon_data = merged_data$df,
      x_var = NULL,
      y_var = merged_data$y_col,
      fill_var = merged_data$fill_col,
      group_var = merged_data$group_col
    ))
    
    expect_error(generate_map_plot(
      polygon_data = merged_data$df,
      x_var = "dummy",
      y_var = merged_data$y_col,
      fill_var = merged_data$fill_col,
      group_var = merged_data$group_col
    ))
    
    expect_error(generate_map_plot(
      polygon_data = merged_data$df,
      x_var = merged_data$x_col,
      y_var = merged_data$y_col,
      fill_var = merged_data$fill_col,
      group_var = merged_data$group_col,
      linewidth = 0
    ))
  }
)
