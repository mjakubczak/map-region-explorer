box::use(
  shiny = shiny[icon]
)

#' @export
help_icon <- function(text){
  res <- shiny$icon("question-circle")
  res$attribs[["data-toggle"]] <- "tooltip"
  res$attribs[["title"]] <- text
  
  res
}
