box::use(
  testthat[...],
  config = config[get],
  crosstalk = crosstalk[SharedData]
)
box::use(
  app/logic/data_utils[...]
)

test_labelled_df <- function(df, test_cases) {
  expect_is(df, "data.frame")
  expect_named(df, names(test_cases))

  for (col_name in names(test_cases)) {
    test_case <- test_cases[[col_name]]

    expect_is(df[[col_name]], test_case[["class"]])
    expect_equal(
      object = get_column_label(
        column = df[[col_name]],
        key = col_name
      ),
      expected = test_case[["label"]]
    )
  }
}

test_that(
  desc = "count data load works fine",
  code = {
    cfg <- config$get(file = "./data/test_config.yml")

    count_data <- prepare_count_data(cfg)
    expect_is(count_data, "list")
    expect_named(count_data, c("df", "count_col", "location_id_col"))
    expect_equal(count_data[["count_col"]], "count")
    expect_equal(count_data[["location_id_col"]], "region_id")

    count_test_cases <- list(
      region_id = list(
        class = "factor",
        label = "Region ID"
      ),
      fraction = list(
        class = "factor",
        label = "Fraction"
      ),
      foo = list(
        class = "integer",
        label = "Foo bar"
      ),
      count = list(
        class = "integer",
        label = "Count"
      )
    )

    test_labelled_df(
      df = count_data[["df"]],
      test_cases = count_test_cases
    )

    cfg_no_fraction <- cfg
    cfg_no_fraction$count_data$file_details$col_defs$fraction <- NULL

    count_data_no_fraction <- prepare_count_data(cfg_no_fraction)
    expect_named(count_data_no_fraction[["df"]], c("region_id", "foo", "count"))

    cfg_missing_count_data <- cfg
    cfg_missing_count_data$count_data <- NULL
    expect_error(
      object = prepare_count_data(cfg_missing_count_data),
      regexp = "Assertion on \\'count_data\\' failed"
    )

    cfg_missing_file_details <- cfg
    cfg_missing_file_details$count_data$file_details <- NULL
    expect_error(
      object = prepare_count_data(cfg_missing_file_details),
      regexp = "Assertion on \\'file_details\\' failed"
    )

    test_count_config_fields <- function(field_name) {
      incorrect_cfg <- cfg

      incorrect_cfg$count_data[[field_name]] <- NULL
      expect_error(
        object = prepare_count_data(incorrect_cfg),
        regexp = paste0("Assertion on '", field_name, "' failed")
      )

      incorrect_cfg$count_data[[field_name]] <- ""
      expect_error(
        object = prepare_count_data(incorrect_cfg),
        regexp = paste0("Assertion on '", field_name, "' failed")
      )

      incorrect_cfg$count_data[[field_name]] <- "dummy"
      expect_error(
        object = prepare_count_data(incorrect_cfg),
        regexp = paste0(
          "Assertion on '", field_name,
          ": dummy must be present in the count data' failed"
        )
      )

      incorrect_cfg$count_data[[field_name]] <- 42
      expect_error(
        object = prepare_count_data(incorrect_cfg),
        regexp = paste0("Assertion on '", field_name, "' failed")
      )
    }

    test_count_config_fields("count_col")
    test_count_config_fields("location_id_col")
  }
)

test_that(
  desc = "location data load works fine",
  code = {
    cfg <- config$get(file = "./data/test_config.yml")

    location_data <- prepare_location_data(cfg)
    expect_is(location_data, "list")
    expect_named(location_data, c("df", "id_col", "label_col", "x_col", "y_col"))
    expect_equal(location_data[["id_col"]], "id")
    expect_equal(location_data[["label_col"]], "label")
    expect_equal(location_data[["x_col"]], "x")
    expect_equal(location_data[["y_col"]], "y")

    location_test_cases <- list(
      id = list(
        class = "factor",
        label = "Region ID"
      ),
      label = list(
        class = "factor",
        label = "Region name"
      ),
      x = list(
        class = "numeric",
        label = "X"
      ),
      y = list(
        class = "numeric",
        label = "Y"
      )
    )

    test_labelled_df(
      df = location_data[["df"]],
      test_cases = location_test_cases
    )

    cfg_missing_location_data <- cfg
    cfg_missing_location_data$location_data <- NULL
    expect_error(
      object = prepare_location_data(cfg_missing_location_data),
      regexp = "Assertion on 'location_data' failed"
    )

    cfg_missing_file_details <- cfg
    cfg_missing_file_details$location_data$file_details <- NULL
    expect_error(
      object = prepare_location_data(cfg_missing_file_details),
      regexp = "Assertion on \\'file_details\\' failed"
    )

    test_location_config_fields <- function(field_name) {
      incorrect_cfg <- cfg

      incorrect_cfg$location_data[[field_name]] <- NULL
      expect_error(
        object = prepare_location_data(incorrect_cfg),
        regexp = paste0("Assertion on '", field_name, "' failed")
      )

      incorrect_cfg$location_data[[field_name]] <- ""
      expect_error(
        object = prepare_location_data(incorrect_cfg),
        regexp = paste0("Assertion on '", field_name, "' failed")
      )

      incorrect_cfg$location_data[[field_name]] <- "dummy"
      expect_error(
        object = prepare_location_data(incorrect_cfg),
        regexp = paste0(
          "Assertion on '", field_name,
          ": dummy must be present in the location data' failed"
        )
      )

      incorrect_cfg$location_data[[field_name]] <- 42
      expect_error(
        object = prepare_location_data(incorrect_cfg),
        regexp = paste0("Assertion on '", field_name, "' failed")
      )
    }

    test_location_config_fields("id_col")
    test_location_config_fields("label_col")
    test_location_config_fields("x_col")
    test_location_config_fields("y_col")
  }
)

test_that(
  desc = "data helpers work fine",
  code = {
    cfg <- config$get(file = "./data/test_config.yml")

    count_data <- prepare_count_data(cfg)
    location_data <- prepare_location_data(cfg)

    labels <- get_column_labels(count_data$df)
    expect_is(labels, "list")
    expect_equal(labels$region_id, "Region ID")
    expect_equal(labels$fraction, "Fraction")
    expect_equal(labels$foo, "Foo bar")
    expect_equal(labels$count, "Count")

    expect_equal(get_column_label(count_data$df$region_id, "region_id"), "Region ID")
    expect_equal(get_column_label(1:5, "dummy"), "dummy")

    sd_obj <- crosstalk$SharedData$new(
      data = count_data$df,
      key = ~region_id
    )

    sd_labels <- get_column_labels(sd_obj)
    expect_equal(sd_labels, labels)

    available_colnames <- get_available_colnames(count_data$df)
    expect_equal(available_colnames, c("region_id", "fraction", "foo", "count"))

    sd_colnames <- get_available_colnames(sd_obj)
    expect_equal(sd_colnames, available_colnames)

    pretty_df <- prettify_data(count_data$df)
    expect_equal(colnames(pretty_df), c("Region ID", "Fraction", "Foo bar", "Count"))

    merged_data <- join_summarized_counts_and_locations(
      count_data = count_data,
      location_data = location_data,
      total_label = "Test label"
    )
    expect_is(merged_data, "list")
    expect_named(merged_data, c("df", "x_col", "y_col", "fill_col", "group_col"))
    expect_is(merged_data$df, "data.frame")
    expect_equal(colnames(merged_data$df), c("region_id", ".total", "label", "x", "y"))
    expect_equal(nrow(merged_data$df), 14)
    expect_equal(get_column_label(merged_data$df$.total, ".total"), "Test label")

    result_df <- filter_locations(
      count_data = count_data,
      location_data = location_data,
      selected_locations = "region 1"
    )
    expect_is(result_df, "data.frame")
    expect_equal(nrow(result_df), 16)
    expect_equal(levels(result_df$label), "region 1")

    pretty_result_df <- prettify_data(result_df)
    expect_equal(colnames(pretty_result_df), c("Fraction", "Foo bar", "Count", "Region name"))

    result_df_2 <- filter_locations(
      count_data = count_data,
      location_data = location_data,
      selected_locations = c("region 1", "region 3")
    )
    expect_equal(nrow(result_df_2), 32)
    expect_equal(levels(result_df_2$label), c("region 1", "region 3"))
  }
)
