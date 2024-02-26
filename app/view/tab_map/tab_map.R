box::use(
  shiny = shiny[NS, moduleServer, reactive, req, validate],
  bslib = bslib[layout_sidebar, sidebar]
)

box::use(
  data_utils = app/logic/data_utils[join_summarized_counts_and_locations, filter_locations,
                                    prettify_data]
)

box::use(
  interactive_map = app/view/commons/interactive_map,
  filters = app/view/tab_map/tab_map_filters,
  table = app/view/tab_map/tab_map_table
)

#' @export
ui <- function(id, title = "The map") {
  ns <- shiny$NS(id)
  
  bslib$layout_sidebar(
    fillable = FALSE,
    sidebar = bslib$sidebar(
      width = 300,
      filters$ui(ns("filters"))
    ),
    interactive_map$ui(
      id = ns("map"),
      title = title
    ),
    table$ui(
      id = ns("table"),
      title = "Table"
    )
  )
}

#' @export
server <- function(id, Count_data, Location_data) {
  shiny$moduleServer(
    id = id, 
    module = function(input, output, session) {
      Filtered_count_data <- filters$server(
        id = "filters",
        Input_data = Count_data
      )
      
      Joined_data <- shiny$reactive({
        count_data <- Filtered_count_data()
        location_data <- Location_data()
        shiny$req(count_data, location_data)
        
        data_utils$join_summarized_counts_and_locations(
          count_data = count_data,
          location_data = location_data
        )
      })
      
      Selected_locations <- interactive_map$server(
        id = "map",
        Input_data = Joined_data
      )
      
      Selection_title_text <- shiny$reactive({
        selected_locations <- Selected_locations()
        shiny$req(selected_locations)
        
        paste0("for selected region(s): ", paste(selected_locations, collapse = ", "))
      })
      
      Table_data <- shiny$reactive({
        selected_locations <- Selected_locations()
        shiny$validate(
          shiny$need(selected_locations, "click on the map region")
        )
        count_data <- Filtered_count_data()
        location_data <- Location_data()
        
        df <- data_utils$filter_locations(
          count_data = count_data,
          location_data = location_data,
          selected_locations = selected_locations
        )
        
        data_utils$prettify_data(
          df = df,
          dictionaries = count_data$dictionaries
        )
      })
      
      table$server(
        id = "table",
        Input_data = Table_data,
        Selection_title_text = Selection_title_text
      )
    }
  )
}
