box::use(
  shiny = shiny[NS, moduleServer, reactive],
  shinyWidgets = shinyWidgets[pickerInput, pickerOptions],
  gtools = gtools[mixedsort]
)

#' @export
ui <- function(id, label, choices, actions_box = TRUE, live_search = TRUE, size = 10, 
               selected_text_format = "count > 3", multiple = TRUE) {
  ns <- shiny$NS(id)
  
  choices <- gtools$mixedsort(choices)
  
  shinyWidgets$pickerInput(
    inputId = ns("picker"),
    label = label,
    choices = choices,
    selected = choices,
    options = shinyWidgets$pickerOptions(
      actionsBox = actions_box,
      liveSearch = live_search,
      size = size,
      selectedTextFormat = selected_text_format
    ),
    multiple = multiple
  )
}

#' @export
server <- function(id, colname, Input_data){
  shiny$moduleServer(
    id = id,
    module = function(input, output, session){
      Filtered_data <- shiny$reactive({
        input_data <- Input_data()
        value <- input$picker
        df <- input_data$df
        
        input_data$df <- df[df[[colname]] %in% value, ]
        input_data
      })
      
      Filtered_data
    }
  )
}
