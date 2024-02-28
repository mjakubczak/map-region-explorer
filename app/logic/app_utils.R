box::use(
  shiny = shiny[icon],
  checkmate = checkmate[check_data_frame]
)

#' Help icon
#'
#' Produces a help icon with on-hover text
#' 
#' @param text character string
#' @return shiny.tag
#' @export
help_icon <- function(text){
  res <- shiny$icon("question-circle")
  res$attribs[["data-toggle"]] <- "tooltip"
  res$attribs[["title"]] <- text
  
  res
}

#' Check if the DF is not empty
#'
#' @param df data.frame
#' @return logical
#' @export
check_filled_df <- function(df){
  isTRUE(checkmate$check_data_frame(
    x = df,
    min.rows = 1,
    min.cols = 1
  ))
}
