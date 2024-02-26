box::use(
  shiny = shiny[icon],
  checkmate = checkmate[check_data_frame]
)

#' @export
help_icon <- function(text){
  res <- shiny$icon("question-circle")
  res$attribs[["data-toggle"]] <- "tooltip"
  res$attribs[["title"]] <- text
  
  res
}

#' @export
check_filled_df <- function(df){
  isTRUE(checkmate$check_data_frame(
    x = df,
    min.rows = 1,
    min.cols = 1
  ))
}
