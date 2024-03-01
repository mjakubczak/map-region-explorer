box::use(
  shiny = shiny[NS, moduleServer, req, textOutput, renderText],
  bslib = bslib[card, card_header, card_body],
  DT = DT[dataTableOutput, renderDataTable, datatable]
)

box::use(
  download = app/view/commons/table_download_dropdown
)

#' @export
ui <- function(id, title) {
  ns <- shiny$NS(id)

  bslib$card(
    bslib$card_header(
      class = "d-flex justify-content-between align-items-center",
      shiny$div(
        shiny$span(title),
        shiny$textOutput(
          outputId = ns("selection_text"),
          inline = TRUE
        )
      ),
      download$ui(ns("download"))
    ),
    bslib$card_body(
      fillable = FALSE,
      fill = FALSE,
      min_height = "125px",
      DT$dataTableOutput(ns("table"))
    )
  )
}

#' @export
server <- function(id, Input_data, Selection_title_text, selection = "none", filter = "top", ...) {
  shiny$moduleServer(
    id = id,
    module = function(input, output, session) {
      download$server(
        id = "download",
        Input_data = Input_data
      )

      output$selection_text <- shiny$renderText({
        Selection_title_text()
      })

      output$table <- DT$renderDataTable({
        df <- Input_data()
        shiny$req(df)

        DT$datatable(
          data = df,
          selection = selection,
          filter = filter,
          ...
        )
      })
    }
  )
}
