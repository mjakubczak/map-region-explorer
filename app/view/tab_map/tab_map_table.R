box::use(
  shiny = shiny[NS, moduleServer, req, textOutput, renderText],
  bslib = bslib[card, card_header, card_body],
  DT = DT[dataTableOutput, renderDataTable, datatable]
)

box::use(
  data_utils = app/logic/data_utils[get_column_labels]
)

#' @export
ui <- function(id, title) {
  ns <- shiny$NS(id)
  
  bslib$card(
    bslib$card_header(shiny$div(
      shiny$span(title), 
      shiny$textOutput(
        outputId = ns("selection_text"),
        inline = TRUE
      )
    )),
    bslib$card_body(
      fillable = FALSE,
      fill = FALSE,
      min_height = "125px",
      DT$dataTableOutput(ns("table"))
    )
  )
}

#' @export
server <- function(id, Input_data, Selection_title_text, selection = "none", filter = "top", ...){
  shiny$moduleServer(
    id = id,
    module = function(input, output, session){
      output$selection_text <- shiny$renderText({
        Selection_title_text()
      })
      
      output$table <- DT$renderDataTable({
        df <- Input_data()
        shiny$req(is.data.frame(df))
        
        DT$datatable(
          data = preprocess_df(df),
          selection = selection,
          filter = filter,
          ...
        )
      })
    }
  )
}

preprocess_df <- function(df){
  labels <- data_utils$get_column_labels(df)
  names(df) <- labels
  
  df
}
