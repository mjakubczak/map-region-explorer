box::use(
  shiny = shiny[NS, moduleServer, reactive, req],
  bslib = bslib[card, card_header, card_body],
  shinycssloaders = shinycssloaders[withSpinner],
  plotly = plotly[plotlyOutput, renderPlotly, highlight_key, ggplotly, highlight],
  htmlwidgets = htmlwidgets[onRender],
  glue = glue[glue],
  stats = stats[as.formula],
  dplyr = dplyr[rename]
)

box::use(
  app_utils = app/logic/app_utils[help_icon],
  map_utils = app/logic/map_utils[generate_map_plot],
  data_utils = app/logic/data_utils[get_column_labels],
)

#' @export
ui <- function(id, title, width = 700, height = 700) {
  ns <- shiny$NS(id)
  
  help_text <- paste0(
    "Click on the map region to highlight it. ",
    "Use shift key to select multiple regions.\n",
    "Double click outside the regions to reset."
  )
  
  bslib$card(
    bslib$card_header(shiny$div(
      shiny$span(paste("Interactive map:", title)), 
      app_utils$help_icon(help_text)
    )),
    bslib$card_body(
      fillable = FALSE,
      shiny$div(
        class = "map-container d-flex justify-content-center",
        plotly$plotlyOutput(
          outputId = ns("map"),
          width = width,
          height = height
        ) |> 
          shinycssloaders$withSpinner(type = 4)
      )
    )
  )
}

#' @export
server <- function(id, Input_data, title) {
  shiny$moduleServer(
    id = id, 
    module = function(input, output, session) {
      input_id <- "selected_items"
      Selected_items <- shiny$reactive({
        input[[input_id]]
      })
      
      output$map <- plotly$renderPlotly({
        input_data <- Input_data()
        shiny$req(input_data)
        
        prepare_interactive_map(
          df = input_data$df,
          input_id = session$ns(input_id),
          x_var = input_data$x_var,
          y_var = input_data$y_var,
          fill_var = input_data$fill_var,
          group_var = input_data$group_var
        )
      })
      
      Selected_items
    }
  )
}

prepare_interactive_map <- function(df, input_id, fill_var = "count", group_var = "group", ..., 
                                    off_event = "plotly_doubleclick", selection_color = "green"){
  
  col_labels <- data_utils$get_column_labels(df)
  fill_label <- col_labels[[fill_var]]
  group_label <- col_labels[[group_var]]
  
  df <- df |>
    dplyr$rename(
      !!fill_label := fill_var,
      !!group_label := group_var
    )
  
  group_formula <- stats$as.formula(paste0("~`", group_label, "`"))
  hl_df <- plotly$highlight_key(
    x = df, 
    key = group_formula
  )
  
  map_utils$generate_map_plot(
    polygon_data = hl_df,
    fill_var = fill_label,
    group_var = group_label,
    ...
  ) |>
    plotly$ggplotly() |>
    plotly$highlight(
      off = off_event, 
      color = selection_color
    ) |>
    htmlwidgets$onRender(
      jsCode = "App.register_custom_click_handler",
      data = list(
        input_id = input_id,
        off_event = off_event
      )
    )
}
