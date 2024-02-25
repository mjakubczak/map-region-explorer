box::use(
  shiny = shiny[NS, moduleServer, sliderInput, reactive]
)

#' @export
ui <- function(id, label, min, max) {
  ns <- shiny$NS(id)
  
  shiny$sliderInput(
    inputId = ns("slider"),
    label = label,
    min = min,
    max = max,
    value = c(min, max)
  )
}

#' @export
server <- function(id, colname, Input_data){
  shiny$moduleServer(
    id = id,
    module = function(input, output, session){
      Filtered_data <- shiny$reactive({
        input_data <- Input_data()
        value <- input$slider
        shiny$req(value)
        
        df <- input_data$df
        col_values <- df[[colname]]
        
        input_data$df <- df[col_values >= value[1] & col_values <= value[2], ]
        input_data
      })
      
      Filtered_data
    }
  )
}
