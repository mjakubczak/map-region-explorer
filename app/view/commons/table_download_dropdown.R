box::use(
  shiny = shiny[NS, moduleServer, div, tags, icon, downloadLink, downloadHandler,
                uiOutput, renderUI],
  readr = readr[write_csv],
  openxlsx = openxlsx[write.xlsx]
)

#' @export
ui <- function(id, inline = TRUE){
  ns <- shiny$NS(id)
  
  shiny$uiOutput(
    outputId = ns("container"),
    inline = inline
  )
}

#' @export
server <- function(id, Input_data, file_name = "table"){
  shiny$moduleServer(
    id = id,
    module = function(input, output, session){
      output$container <- shiny$renderUI({
        df <- try(
          expr = Input_data(), 
          silent = TRUE
        )
        
        if (is.data.frame(df)){
          dropdown_ui(session$ns)
        } # else NULL
      })
      
      output$csv <- shiny$downloadHandler(
        filename = function(){
          paste0(file_name, ".csv")
        },
        content = function(file){
          df <- Input_data()
          
          readr$write_csv(
            x = df,
            file = file
          )
        }
      )
      
      output$xlsx <- shiny$downloadHandler(
        filename = function(){
          paste0(file_name, ".xlsx")
        },
        content = function(file){
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

dropdown_item <- function(id, label){
  shiny$tags$li(
    shiny$downloadLink(
      outputId = id, 
      class = "dropdown-item",
      label
    )
  )
}

dropdown_ui <- function(ns){
  button_id <- ns("button")
  
  shiny$div(
    class = "dropdown",
    shiny$tags$button(
      class = "btn btn-secondary dropdown-toggle",
      type = "button",
      id = button_id,
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false",
      shiny$icon("download"),
      "Download"
    ),
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
