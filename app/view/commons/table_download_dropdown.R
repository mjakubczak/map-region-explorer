box::use(
  shiny = shiny[NS, moduleServer, div, tags, icon, downloadLink, downloadHandler, observe],
  readr = readr[write_csv],
  openxlsx = openxlsx[write.xlsx],
  shinyjs = shinyjs[toggleState, disabled]
)

#' @export
ui <- function(id, inline = TRUE) {
  ns <- shiny$NS(id)

  button_id <- ns("button")

  button_html <- shiny$tags$button(
    class = "btn btn-secondary dropdown-toggle",
    type = "button",
    id = button_id,
    `data-bs-toggle` = "dropdown",
    `aria-expanded` = "false",
    shiny$icon("download"),
    "Download"
  )

  shiny$div(
    class = "dropdown",
    shinyjs$disabled(button_html),
    shiny$tags$ul(
      class = "dropdown-menu",
      `aria-labelledby` = button_id,
      dropdown_item(
        id = ns("csv"),
        label = "CSV"
      ),
      dropdown_item(
        id = ns("xlsx"),
        label = "XLSX"
      )
    )
  )
}

#' @export
server <- function(id, Input_data, file_name = "table") {
  shiny$moduleServer(
    id = id,
    module = function(input, output, session) {
      shiny$observe({
        df <- try(
          expr = Input_data(),
          silent = TRUE
        )

        shinyjs$toggleState(
          id = "button",
          condition = is.data.frame(df)
        )
      })

      output$csv <- shiny$downloadHandler(
        filename = function() {
          paste0(file_name, ".csv")
        },
        content = function(file) {
          df <- Input_data()

          readr$write_csv(
            x = df,
            file = file
          )
        }
      )

      output$xlsx <- shiny$downloadHandler(
        filename = function() {
          paste0(file_name, ".xlsx")
        },
        content = function(file) {
          df <- Input_data()

          openxlsx$write.xlsx(
            x = df,
            file = file
          )
        }
      )
    }
  )
}

dropdown_item <- function(id, label) {
  shiny$tags$li(
    shiny$downloadLink(
      outputId = id,
      class = "dropdown-item",
      label
    )
  )
}
