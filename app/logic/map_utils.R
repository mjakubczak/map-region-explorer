box::use(
  ggplot2 = ggplot2[ggplot, aes, geom_polygon, scale_fill_gradient, theme, element_blank, ggtitle],
  checkmate = checkmate[
    assert, check_data_frame, check_class, check_string, check_null,
    assert_data_frame, assert_string, assert_numeric, assert_subset
  ],
  rlang = rlang[sym]
)

box::use(
  data_utils = app/logic/data_utils[get_column_labels, get_available_colnames]
)

#' Generate map plot
#'
#' @param polygon_data data.frame or SharedData object
#' @param title optional character string
#' @param x_var character string, polygon_data column with X data
#' @param y_var character string, polygon_data column with Y data
#' @param fill_var character string, polygon_data column with fill data
#' @param group_var character string, polygon_data column with group data
#' @param fill_low_color character string, low color in scale_fill_gradient
#' @param fill_high_color character string, high color in scale_fill_gradient
#' @param color character string, region border color
#' @param linewidth positive numeric, region border width
#' @return ggplot object
#' @export
generate_map_plot <- function(polygon_data, title = NULL, x_var = "x", y_var = "y",
                              fill_var = "count", group_var = "group",
                              fill_low_color = "yellow", fill_high_color = "red",
                              color = "gray70", linewidth = 0.1) {
  checkmate$assert(
    checkmate$check_data_frame(polygon_data),
    checkmate$check_class(polygon_data, "SharedData")
  )
  checkmate$assert(
    checkmate$check_string(title),
    checkmate$check_null(title)
  )
  checkmate$assert_string(x_var)
  checkmate$assert_string(y_var)
  checkmate$assert_string(fill_var)
  checkmate$assert_string(group_var)
  checkmate$assert_string(fill_low_color)
  checkmate$assert_string(fill_high_color)
  checkmate$assert_string(color)
  checkmate$assert_numeric(
    x = linewidth,
    lower = 0.1,
    finite = TRUE,
    len = 1
  )

  checkmate$assert_subset(
    x = c(x_var, y_var, fill_var, group_var),
    choices = get_available_colnames(polygon_data)
  )

  x_var_sym <- rlang$sym(x_var)
  y_var_sym <- rlang$sym(y_var)
  fill_var_sym <- rlang$sym(fill_var)
  group_var_sym <- rlang$sym(group_var)

  ggplot2$ggplot() +
    ggplot2$geom_polygon(
      data = polygon_data,
      color = color,
      linewidth = linewidth,
      ggplot2$aes(
        x = !!x_var_sym,
        y = !!y_var_sym,
        group = !!group_var_sym,
        fill = !!fill_var_sym
      )
    ) +
    ggplot2$scale_fill_gradient(
      low = fill_low_color,
      high = fill_high_color
    ) +
    ggplot2$theme(
      axis.title = ggplot2$element_blank(),
      axis.ticks = ggplot2$element_blank(),
      axis.text = ggplot2$element_blank(),
      panel.background = ggplot2$element_blank(),
      panel.grid = ggplot2$element_blank()
    ) +
    ggplot2$ggtitle(title)
}
