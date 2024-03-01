box::use(
  shiny = shiny[NS, moduleServer, uiOutput, renderUI, observeEvent],
  gtools = gtools[mixedsort]
)

box::use(
  data_utils = app/logic/data_utils[get_column_label]
)

box::use(
  slider = app/view/filters/filter_slider,
  picker = app/view/filters/filter_picker
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$uiOutput(ns("container"))
}

#' @export
server <- function(id, Input_data) {
  shiny$moduleServer(
    id = id,
    module = function(input, output, session) {
      filter_chain <- list()

      Module_uis <- shiny$reactiveVal()
      Filtered_data <- shiny$reactiveVal()
      module_counter <- 0

      shiny$observeEvent(
        eventExpr = Input_data(),
        handlerExpr = {
          filter_chain <<- list() # clear chain
          uis <- list()

          d <- Input_data()
          filter_cols <- get_filter_columns(d)

          uis <- lapply(
            X = seq_along(names(filter_cols)),
            FUN = function(i) {
              key <- names(filter_cols)[i]
              module_counter <<- module_counter + 1 # to keep unique IDs

              Submodule_input <- if (i == 1) {
                Input_data
              } else {
                filter_chain[[i - 1]] # previous submodule
              }

              res <- generate_filter_module(
                id = paste0("submodule_", module_counter),
                column = filter_cols[[key]],
                key = key,
                Input_data = Submodule_input,
                ns = session$ns
              )

              filter_chain[[i]] <<- res$server
              res$ui
            }
          )

          Module_uis(uis)
          Chain_output <- filter_chain[[length(filter_chain)]]

          shiny$observeEvent(
            eventExpr = Chain_output(), # the last submodule output, in fact
            handlerExpr = {
              Filtered_data(Chain_output())
            },
            ignoreNULL = FALSE
          )
        }
      )

      output$container <- shiny$renderUI({
        Module_uis()
      })

      Filtered_data
    }
  )
}

get_filter_columns <- function(d) {
  df <- d$df
  skipped_cols <- c(d$count_col, d$location_id_col)

  df[, !colnames(df) %in% skipped_cols]
}

generate_filter_module <- function(id, column, key, Input_data, ns) {
  label <- data_utils$get_column_label(
    column = column,
    key = key
  )

  if (is.logical(column) || is.character(column) || is.factor(column)) {
    choices <- gtools$mixedsort(unique(column))
    ui <- picker$ui(
      id = ns(id),
      label = label,
      choices = choices
    )

    server_fun <- picker$server
  } else if (is.numeric(column)) {
    col_range <- range(column, na.rm = TRUE)
    ui <- slider$ui(
      id = ns(id),
      label = label,
      min = col_range[1],
      max = col_range[2]
    )

    server_fun <- slider$server
  }

  server <- server_fun(
    id = id,
    colname = key,
    Input_data = Input_data
  )

  list(
    ui = ui,
    server = server
  )
}
