box::use(
  shiny = shiny[NS, moduleServer, div, tags, bindCache],
  bslib = bslib[page_navbar, nav_panel, nav_spacer, nav_item, bs_theme],
  config = config[get]
)

box::use(
  data_utils = app/logic/data_utils[prepare_count_data, prepare_location_data]
)

box::use(
  tab_map = app/view/tab_map/tab_map
)

settings <- config$get()
data_settings <- config$get(
  file = settings[["data"]][["config_file"]],
  config = settings[["data"]][["profile"]]
)

# app -------------------------------------------------------------------------
#' @export
ui <- function(id) {
  ns <- NS(id)
  
  bslib$page_navbar(
    title = settings[["app_name"]],
    
    theme = bslib$bs_theme(
      version = 5,
      preset = "yeti"
    ),
    
    bslib$nav_panel(
      title = "Map",
      tab_map$ui(
        id = ns("map"),
        title = data_settings[["title"]]
      )
    ),
    # bslib$nav_panel(
    #   title = "Plots",
    #   "to be developed"
    # ),
    bslib$nav_spacer(),
    bslib$nav_item(shiny$div(
      paste0("v", settings[["version"]])
    ))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    Count_data <- shiny$reactive({
      data_utils$prepare_count_data(data_settings)
    }) |>
      shiny$bindCache("dummy", cache = "app")
    
    Location_data <- shiny$reactive({
      data_utils$prepare_location_data(data_settings)
    }) |>
      shiny$bindCache("dummy", cache = "app")
    
    tab_map$server(
      id = "map",
      Count_data = Count_data,
      Location_data = Location_data
    )
  })
}
