box::use(
  readr = readr[
    read_csv, cols_only, col_logical, col_integer, col_double,
    col_character, col_factor, col_guess
  ],
  checkmate = checkmate[...],
  dplyr = dplyr[select, left_join, filter, distinct, group_by, summarize, mutate, across],
  tidyselect = tidyselect[where],
  purrr = purrr[transpose],
  rlang = rlang[sym],
  gtools = gtools[mixedsort],
  glue = glue[glue]
)

assert_nz_string <- function(x, min.chars = 1, .var.name = checkmate$vname(x), ...) {
  checkmate$assert_string(
    x = x,
    min.chars = min.chars,
    .var.name = .var.name,
    ...
  )
}

# Data load -------------------------------------------------------------------
#' Prepare count data
#'
#' @param settings list, general data configuration
#' @return list with fields: df, count_col, location_id_col
#' @export
prepare_count_data <- function(settings) {
  count_data <- settings[["count_data"]]
  checkmate$assert_list(count_data)

  parse_counts(
    count_data_def = count_data,
    data_dir = settings[["data_dir"]]
  )
}

#' Prepare location data
#'
#' @param settings list, general data configuration
#' @return list with fields: df, id_col, label_col, x_col, y_col
#' @export
prepare_location_data <- function(settings) {
  location_data <- settings[["location_data"]]
  checkmate$assert_list(location_data)

  parse_locations(
    location_data_def = location_data,
    data_dir = settings[["data_dir"]]
  )
}

#' Parse count data
#'
#' @param count_data_def list, count data config
#' @param data_dir character string, data directory
#' @return list with fields: df, count_col, location_id_col
parse_counts <- function(count_data_def, data_dir) {
  file_details <- count_data_def[["file_details"]]
  checkmate$assert_list(file_details)

  count_df <- read_file(
    file_details = file_details,
    data_dir = data_dir
  )

  count_col <- count_data_def[["count_col"]]
  location_id_col <- count_data_def[["location_id_col"]]

  validate_column <- function(x, .var.name = checkmate$vname(x)) {
    assert_nz_string(
      x = x,
      .var.name = .var.name
    )
    checkmate$assert_true(
      x = x %in% colnames(count_df),
      .var.name = glue$glue("{.var.name}: {x} must be present in the count data")
    )
  }

  validate_column(count_col)
  validate_column(location_id_col)

  dictionaries <- lapply(
    X = count_data_def[["dictionaries"]],
    FUN = parse_dictionary,
    data_dir = data_dir
  )
  names(dictionaries) <- purrr$map_chr(dictionaries, ~ .x$explained_col)

  for (key in names(dictionaries)) {
    dict <- dictionaries[[key]]
    checkmate$assert_true(dict$explained_col %in% colnames(count_df))

    count_df[[key]] <- map_dictionary(
      ids = count_df[[key]],
      dict = dict,
      keep_factors = TRUE
    )
  }

  list(
    df = count_df,
    count_col = count_col,
    location_id_col = location_id_col
  )
}

#' Parse location data
#'
#' @param location_data_def list, location data config
#' @param data_dir character string, data directory
#' @return list with fields: df, id_col, label_col, x_col, y_col
parse_locations <- function(location_data_def, data_dir) {
  file_details <- location_data_def[["file_details"]]
  checkmate$assert_list(file_details)

  location_df <- read_file(
    file_details = file_details,
    data_dir = data_dir
  )

  id_col <- location_data_def[["id_col"]]
  label_col <- location_data_def[["label_col"]]
  x_col <- location_data_def[["x_col"]]
  y_col <- location_data_def[["y_col"]]

  validate_column <- function(x, .var.name = checkmate$vname(x)) {
    assert_nz_string(
      x = x,
      .var.name = .var.name
    )
    checkmate$assert_true(
      x = x %in% colnames(location_df),
      .var.name = glue$glue("{.var.name}: {x} must be present in the location data")
    )
  }

  validate_column(id_col)
  validate_column(label_col)
  validate_column(x_col)
  validate_column(y_col)

  list(
    df = location_df,
    id_col = id_col,
    label_col = label_col,
    x_col = x_col,
    y_col = y_col
  )
}

#' Parse dictionary data
#'
#' @param dict_data_def list, dict data config
#' @param data_dir character string, data directory
#' @return list with fields: df, explained_col, id_col, label_col, show_id
parse_dictionary <- function(dict_data_def, data_dir) {
  dict_df <- read_file(
    file_details = dict_data_def[["file_details"]],
    data_dir = data_dir,
    sort_factor_levels = FALSE
  )

  explained_col <- dict_data_def[["explained_col"]]
  id_col <- dict_data_def[["id_col"]]
  label_col <- dict_data_def[["label_col"]]
  show_id <- dict_data_def[["show_id"]]

  assert_nz_string(explained_col)
  assert_nz_string(id_col)
  checkmate$assert_true(id_col %in% colnames(dict_df))
  assert_nz_string(label_col)
  checkmate$assert_true(label_col %in% colnames(dict_df))
  checkmate$assert(
    checkmate$check_logical(
      x = show_id,
      len = 1
    ),
    checkmate$check_null(show_id)
  )

  if (is.null(show_id)) {
    show_id <- FALSE
  }

  list(
    df = dict_df,
    explained_col = explained_col,
    id_col = id_col,
    label_col = label_col,
    show_id = show_id
  )
}

#' Read file
#'
#' @param file_details list, file config
#' @param data_dir character string, data directory
#' @param sort_factor_levels logical, should sort factor levels?
read_file <- function(file_details, data_dir, sort_factor_levels = TRUE) {
  assert_nz_string(data_dir)

  file_name <- file_details[["file_name"]]
  assert_nz_string(file_name)
  file_dir <- file.path(data_dir, file_name)
  checkmate$assert_file_exists(file_dir)

  col_defs <- file_details[["col_defs"]]

  labels <- NULL
  args <- list(
    file = file_dir
  )

  if (!is.null(col_defs)) {
    col_defs <- parse_col_defs(col_defs)

    labels <- col_defs$labels
    args[["col_types"]] <- col_defs$types
  }

  df <- do.call(
    what = readr$read_csv,
    args = args
  )

  checkmate$assert_true(nrow(df) > 0)

  if (sort_factor_levels) {
    df <- df |>
      dplyr$mutate(dplyr$across(
        .cols = tidyselect$where(is.factor),
        .fns = mixsort_factor
      ))
  }

  for (key in names(labels)) {
    if (key %in% colnames(df)) {
      set_label_attr(df[[key]]) <- labels[[key]]
    }
  }

  df
}

#' Parse column definitions
#'
#' @param col_defs named list, column definitions
#' @return list with fields: types, labels
parse_col_defs <- function(col_defs) {
  checkmate$assert_list(
    x = col_defs,
    types = "list",
    names = "named"
  )

  col_info <- lapply(
    X = names(col_defs),
    FUN = function(col_name) {
      col_def <- col_defs[[col_name]]
      type <- col_def[["type"]]
      assert_nz_string(
        x = type,
        .var.name = paste0("col_defs['", col_name, "'].type")
      )
      label <- col_def[["label"]]
      assert_nz_string(
        x = label,
        .var.name = paste0("col_defs['", col_name, "'].label")
      )

      collector <- switch(
        EXPR = type,
        "logical" = readr$col_logical(),
        "integer" = readr$col_integer(),
        "double" = readr$col_double(),
        "character" = readr$col_character(),
        "factor" = readr$col_factor(),
        readr$col_guess()
      )

      list(
        collector = collector,
        label = label
      )
    }
  )

  names(col_info) <- names(col_defs)
  col_info <- purrr$transpose(col_info)

  types <- do.call(
    what = readr$cols_only,
    args = col_info$collector
  )

  list(
    types = types,
    labels = col_info$label
  )
}

#' Map dictionary
#'
#' @param ids character or factor, values to be mapped (usually IDs)
#' @param dict list, parse_dictionary function output
#' @param keep_factors logical, should keep factor class?
map_dictionary <- function(ids, dict, keep_factors = FALSE) {
  checkmate$assert(
    checkmate$check_character(ids),
    checkmate$check_factor(ids)
  )

  idx <- match(
    x = ids,
    table = dict$df[[dict$id_col]]
  )

  if (anyNA(idx)) {
    warning("some IDs don't have label for column ", dict$explained_col)
    return(ids)
  }

  is_factor <- is.factor(ids)
  label_attr <- get_label_attr(ids)

  labels_df <- dict$df[idx, ]
  dict_labels <- labels_df[[dict$label_col]]
  res <- if (isTRUE(dict$show_id)) {
    dict_ids <- labels_df[[dict$id_col]]
    paste(dict_ids, dict_labels, sep = " - ")
  } else {
    dict_labels
  }

  if (keep_factors && is_factor) {
    res <- mixsort_factor(res)
  }

  if (!is.null(label_attr)) {
    set_label_attr(res) <- label_attr
  }

  res
}

# Labels ----------------------------------------------------------------------
#' Get label attribute
#'
#' @param x vector
#' @return character string (if label exists) or NULL
get_label_attr <- function(x) {
  attr(x, "label")
}

#' Set label attribute
#'
#' @param x any object
#' @param value label to be assigned
#' @return updated x
`set_label_attr<-` <- function(x, value) {
  attr(x, "label") <- value
  x
}

#' Get column labels
#'
#' Returns labels assigned to every column in data.frame-like object
#'
#' @param df data.frame-like object
#' @return list
#' @export
get_column_labels <- function(df) {
  UseMethod("get_column_labels")
}

get_column_labels.default <- function(df) {
  res <- lapply(
    X = colnames(df),
    FUN = function(x) {
      get_column_label(
        column = df[[x]],
        key = x
      )
    }
  )

  names(res) <- colnames(df)
  res
}

get_column_labels.SharedData <- function(df) {
  get_column_labels(df$origData())
}

#' Get column label
#'
#' @param column any vector
#' @param key character string, fallback value if label does not exist
#' @return character string
#' @export
get_column_label <- function(column, key) {
  label <- get_label_attr(column)
  if (is.null(label)) {
    key
  } else {
    label
  }
}

# App helpers -----------------------------------------------------------------
#' Join summarized counts and locations
#'
#' This function aggregates counts by location and joins it with the location data
#'
#' @param count_data list, result of parse_counts function
#' @param location_data list, result of parse_locations functions
#' @param total_label character string, label for the aggregated column
#' @return list with fields: df, x_col, y_col, fill_col, group_col
#' @export
join_summarized_counts_and_locations <- function(count_data, location_data,
                                                 total_label = "Total count") {
  count_col_sym <- rlang$sym(count_data$count_col)
  location_col_sym <- rlang$sym(count_data$location_id_col)

  count_data$df <- count_data$df |>
    dplyr$group_by(!!location_col_sym) |>
    dplyr$summarise(.total = sum(!!count_col_sym))

  merged_df <- merge_counts_and_locations(
    count_data = count_data,
    location_data = location_data
  )

  set_label_attr(merged_df[[".total"]]) <- total_label

  list(
    df = merged_df,
    x_col = location_data$x_col,
    y_col = location_data$y_col,
    fill_col = ".total",
    group_col = location_data$label_col
  )
}

#' Filter locations
#'
#' @param count_data list, result of parse_counts function
#' @param location_data list, result of parse_locations functions
#' @param selected_locations vector of selected location labels
#' @return data.frame
#' @export
filter_locations <- function(count_data, location_data, selected_locations) {
  id_col_sym <- rlang$sym(location_data$id_col)
  label_col_sym <- rlang$sym(location_data$label_col)
  count_location_id_col <- rlang$sym(count_data$location_id_col)

  location_data$df <- location_data$df |>
    dplyr$select(!!id_col_sym, !!label_col_sym) |>
    dplyr$distinct()

  res <- merge_counts_and_locations(
    count_data = count_data,
    location_data = location_data
  ) |>
    dplyr$filter(!!label_col_sym %in% selected_locations) |>
    dplyr$select(-!!count_location_id_col)

  labels <- lapply(
    X = res,
    FUN = get_label_attr
  )

  # droplevels also drops label attribute, so it's essential to restore them afterwards
  res <- droplevels(res)

  for (key in names(res)) {
    set_label_attr(res[[key]]) <- labels[[key]]
  }

  res
}

#' Merge counts and locations
#'
#' Helper function that merges counts and locations by proper columns
#'
#' @param count_data list, result of parse_counts function
#' @param location_data list, result of parse_locations functions
#' @return data.frame
merge_counts_and_locations <- function(count_data, location_data) {
  count_col_sym <- rlang$sym(count_data$count_col)
  location_col_sym <- rlang$sym(count_data$location_id_col)

  merge_def <- structure(
    location_data$id_col,
    names = count_data$location_id_col
  )

  count_data$df |>
    dplyr$left_join(location_data$df, by = merge_def)
}

#' Prettify data
#'
#' @param df data.frame
#' @return pretty df
#' @export
prettify_data <- function(df) {
  labels <- get_column_labels(df)
  names(df) <- labels
  df
}

# General helpers -------------------------------------------------------------
#' Sort factor levels
#'
#' Sort factor levels using mixedsort approach
#'
#' @param x factor-like object
#' @return factor
mixsort_factor <- function(x) {
  factor(
    x = as.character(x),
    levels = gtools$mixedsort(as.character(unique(x)))
  )
}

#' Get available colnames
#'
#' @param df data.frame-like object
#' @return character vector
#' @export
get_available_colnames <- function(df) {
  UseMethod("get_available_colnames")
}

get_available_colnames.default <- function(df) {
  colnames(df)
}

get_available_colnames.SharedData <- function(df) {
  get_available_colnames(df$origData())
}
