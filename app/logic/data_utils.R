box::use(
  readr = readr[read_csv, cols_only, col_logical, col_integer, col_double, 
                col_character, col_factor, col_guess],
  checkmate = checkmate[assert_list, assert_string, assert_file_exists],
  dplyr = dplyr[select, left_join, filter, distinct, group_by, summarize, mutate, across],
  tidyselect = tidyselect[where],
  purrr = purrr[transpose],
  rlang = rlang[sym],
  gtools = gtools[mixedsort]
)

assert_nz_string <- function(x, ...){
  checkmate$assert_string(
    x = x,
    min.chars = 1,
    ...
  )
}

#' @export
prepare_count_data <- function(settings){
  parse_counts(
    count_data_def = settings[["count_data"]],
    data_dir = settings[["data_dir"]]
  )
}

#' @export
prepare_location_data <- function(settings){
  parse_locations(
    location_data_def = settings[["location_data"]],
    data_dir = settings[["data_dir"]]
  )
}

read_file <- function(file_details, data_dir, sort_factor_levels = TRUE){
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
  
  if (!is.null(col_defs)){
    col_defs <- parse_col_defs(col_defs)
    
    labels <- col_defs$labels
    args[["col_types"]] <- col_defs$types
  }
  
  df <- do.call(
    what = readr$read_csv,
    args = args
  )
  
  if (sort_factor_levels){
    df <- df |>
      dplyr$mutate(dplyr$across(
        .cols = tidyselect$where(is.factor), 
        .fns = mixsort_factor
      ))
  }
  
  for (key in names(labels)){
    if (key %in% colnames(df)){
      df[[key]] <- set_label_attr(
        x = df[[key]],
        label = labels[[key]]
      )
    }
  }
  
  df
}

mixsort_factor <- function(x){
  factor(
    x = as.character(x),
    levels = gtools$mixedsort(as.character(unique(x)))
  )
}

get_label_attr <- function(x){
  attr(x, "label")
}

set_label_attr <- function(x, label){
  attr(x, "label") <- label
  x
}

parse_col_defs <- function(col_defs){
  checkmate$assert_list(
    x = col_defs,
    types = "list",
    names = "named"
  )
  
  col_defs <- lapply(
    X = col_defs,
    FUN = function(x){
      type <- x[["type"]]
      assert_nz_string(type)
      label <- x[["label"]]
      assert_nz_string(label)
      
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
  
  col_defs <- purrr$transpose(col_defs)
  
  types <- do.call(
    what = readr$cols_only,
    args = col_defs$collector
  )
  
  list(
    types = types,
    labels = col_defs$label
  )
}

parse_counts <- function(count_data_def, data_dir){
  count_df <- read_file(
    file_details = count_data_def[["file_details"]],
    data_dir = data_dir
  )
  
  dictionaries <- lapply(
    X =  count_data_def[["dictionaries"]],
    FUN = parse_dictionary,
    data_dir = data_dir
  )
  names(dictionaries) <- purrr$map_chr(dictionaries, ~ .x$explained_col)
  
  list(
    df = count_df,
    count_col = count_data_def[["count_col"]],
    location_id_col = count_data_def[["location_id_col"]],
    dictionaries = dictionaries
  )
}

parse_locations <- function(location_data_def, data_dir){
  location_df <- read_file(
    file_details = location_data_def[["file_details"]],
    data_dir = data_dir
  )
  
  list(
    df = location_df,
    id_col = location_data_def[["id_col"]],
    label_col = location_data_def[["label_col"]],
    x_var = location_data_def[["x_var"]],
    y_var = location_data_def[["y_var"]]
  )
}

parse_dictionary <- function(dict_data_def, data_dir){
  dict_df <- read_file(
    file_details = dict_data_def[["file_details"]],
    data_dir = data_dir,
    sort_factor_levels = FALSE
  )
  
  list(
    df = dict_df,
    explained_col = dict_data_def[["explained_col"]],
    id_col = dict_data_def[["id_col"]],
    label_col = dict_data_def[["label_col"]],
    show_id = dict_data_def[["show_id"]]
  )
}

#' @export
get_available_colnames <- function(df){
  UseMethod("get_available_colnames")
}

get_available_colnames.data.frame <- function(df){
  colnames(df)
}

get_available_colnames.SharedData <- function(df){
  get_available_colnames(df$origData())
}

#' @export
get_column_labels <- function(df){
  UseMethod("get_column_labels")
}

get_column_labels.data.frame <- function(df){
  res <- lapply(
    X = colnames(df),
    FUN = function(x){
      get_column_label(
        column = df[[x]],
        key = x
      )
    }
  )
  
  names(res) <- colnames(df)
  res
}

get_column_labels.SharedData <- function(df){
  get_column_labels(df$origData())
}

#' @export
get_column_label <- function(column, key){
  label <- get_label_attr(column)
  if (is.null(label)){
    key
  } else {
    label
  }
}

#' @export
join_summarized_counts_and_locations <- function(count_data, location_data, total_label = "Total count"){
  count_col_sym <- rlang$sym(count_data$count_col)
  location_col_sym <- rlang$sym(count_data$location_id_col)
  
  count_data$df <- count_data$df |>
    dplyr$group_by(!!location_col_sym) |>
    dplyr$summarise(.total = sum(!!count_col_sym))
  
  merged_df <- merge_counts_and_locations(
    count_data = count_data,
    location_data = location_data
  )
  
  merged_df[[".total"]] <- set_label_attr(
    x = merged_df[[".total"]],
    label = total_label
  )
  
  list(
    df = merged_df,
    x_var = location_data$x_var,
    y_var = location_data$y_var,
    fill_var = ".total",
    group_var = location_data$label_col
  )
}

#' @export
filter_locations <- function(count_data, location_data, selected_locations){
  id_col_sym <- rlang$sym(location_data$id_col)
  label_col_sym <- rlang$sym(location_data$label_col)
  count_location_id_col <- rlang$sym(count_data$location_id_col)
  
  location_data$df <- location_data$df |>
    dplyr$select(!!id_col_sym, !!label_col_sym) |>
    dplyr$distinct()
  
  merge_counts_and_locations(
    count_data = count_data,
    location_data = location_data
  ) |>
    dplyr$filter(!!label_col_sym %in% selected_locations) |>
    dplyr$select(-!!count_location_id_col)
}

merge_counts_and_locations <- function(count_data, location_data){
  count_col_sym <- rlang$sym(count_data$count_col)
  location_col_sym <- rlang$sym(count_data$location_id_col)
  
  merge_def <- structure(
    location_data$id_col,
    names = count_data$location_id_col
  )
  
  count_data$df |>
    dplyr$left_join(location_data$df, by = merge_def)
}

#' @export
map_dictionary <- function(ids, dict, keep_factors = FALSE){
  idx <- match(
    x = ids, 
    table = dict$df[[dict$id_col]]
  )
  
  if (anyNA(idx)){
    warning("some IDs don't have label for column ", dict$explained_col)
    return(ids)
  }
  
  is_factor <- is.factor(ids)
  
  labels_df <- dict$df[idx, ]
  dict_labels <- labels_df[[dict$label_col]]
  res <- if (isTRUE(dict$show_id)){
    dict_ids <- labels_df[[dict$id_col]]
    paste(dict_ids, dict_labels, sep = " - ")
  } else {
    dict_labels
  }
  
  if (keep_factors && is_factor){
    mixsort_factor(res)
  } else {
    res
  }
}

#' @export
prettify_data <- function(df, dictionaries){
  labels <- get_column_labels(df)
  
  for (key in names(dictionaries)){
    df[[key]] <- map_dictionary(
      ids = df[[key]],
      dict = dictionaries[[key]],
      keep_factors = TRUE
    )
  }
  
  names(df) <- labels
  
  df
}
