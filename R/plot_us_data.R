#' plot_us_data
#'
#' This function produces a ggplot2 based choropleth map of a discrete/continuous variable across all/selected US states
#'   including viewable placement of Alaska, Hawaii, and Puerto Rico.  The function accepts a data frame with a column of
#'   state names and a column with their respective values.  The function offers several options for
#'   control of state geographies and variable legend.
#'
#' Because scaling is done manually, "scale_breaks" is a required parameter. For discrete values this must
#'   must be a character vector. For continuous values this must be a numeric vector.
#'
#' This function depends extensively on \code{RcensusPkg::tiger_states_sf()} for obtaining state geometries, so many of
#'   that function's parameters are repeated in this function. Also \code{RspatialPkg::get_geom_sf()} is called upon for
#'   displaying the shapefile geometries.
#'
#' @param df The data frame with a column of full state names and a second variable column of their respective values.
#'   The column name for the states must be "NAME".
#' @param states_col A string that sets the column name from \code{df} containing the state names of interest. These are
#'   full state names, either capitalized or lower case. This is a required parameter.
#' @param value_col A string that sets the column name from \code{df} where values(discrete or continuous) are defined.
#'   If the column has discrete values then it must be a factor. This is a required parameter.
#' @param title A string that sets the plot title.
#' @param text_col An optional string that sets the column name from \code{df} for labelling each state polygon.
#' @param text_size A numeric value that sets the size of labeled state text.
#' @param text_color A string that sets the color of labeled state text.
#' @param text_fontface A string that sets the fontface of labeled state text.
#'  Acceptable values: "plain", "bold", "italic", "bold.italic". The default is "plain".
#' @param output_dir A full directory path where the shapefile and its associated files will be downloaded.
#'   The default is the directory defined by the value returned by \code{tempdir()}.
#' @param vintage A numeric that sets the vintage of interest. The default is 2020.
#' @param general A logical which if TRUE will download a less detailed, more generalized version of the state geometries.
#' @param resol If \code{general} is TRUE, then the resolution to return. Acceptable values are strings
#'   "500k", "5m", "20m".
#' @param hide_x_tics A logical that controls the appearance of the x axis tics.
#' @param hide_y_tics A logical that controls the appearance of the y axis tics.
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are "right",
#'  "top", "bottom".
#' @param na_rm A logical which if TRUE, missing observations are removed. If FALSE, the default,
#'   missing observations are removed with a warning.
#' @param scale_breaks A string/numeric vector that defines the scale breaks. This is a required parameter.
#' @param scale_values A string/numeric vector that defines the possible values. For factor values, this is required
#'   and is a vector string of colors.
#' @param scale_limits A string/numeric vector that defines the scale limits.
#' @param scale_labels An optional string vector that defines the scale labels. Vector must be the same length
#' as \code{scale_breaks}.
#' @param scale_colors Vector of colors to use for n-color gradient.
#' @param scale_na_value A string that sets the color for missing values.
#' @param own_scale A logical which if TRUE, then your own scaling may be appended to the plot without using the above
#'   scale_* parameters.
#' @param sf_color A string that sets the polygon border line color.
#' @param sf_fill A string that sets the polygon area fill color.
#' @param sf_linewidth A numeric that sets the border line thickness.
#' @param sf_alpha A numeric that sets the alpha level attribute of the polygon fill.
#' @param display_plot A logical that if TRUE will display the plot. The default is TRUE.
#'   If FALSE, then a ggplot2 object is returned
#'
#' @importFrom sf st_transform
#' @importFrom sf st_crs
#' @importFrom sf st_as_sf
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_grob
#' @importFrom RspatialPkg get_geom_sf
#' @importFrom ggplotify as.ggplot
#' @import ggplot2
#' @import magrittr
#'
#' @author Rick Dean
#'
#' @return A list of ggplot2 objects if \code{display_plot} is FALSE. Included in the
#'   list is the plot of all the states ("us_states") along with the original ggplot2
#'   geom_sf plots of the lower 48 ("lower_48"), Alaska ("alaska"), Hawaii ("hawaii") and Puerto Rico ("puerto_rico").
#'
#' @export
plot_us_data <- function(
  df = NULL,
  states_col = NULL,
  value_col = NULL,
  title = NULL,
  text_col = NULL,
  text_size = 3.0,
  text_color = "black",
  text_fontface = "plain",
  output_dir = tempdir(check = T),
  vintage = 2020,
  general = FALSE,
  resol = "500k",
  hide_x_tics = TRUE,
  hide_y_tics = TRUE,
  show_legend = TRUE,
  legend_pos = "right",
  na_rm = FALSE,
  scale_breaks = waiver(),
  scale_values = NULL,
  scale_limits = NULL,
  scale_labels = NULL,
  scale_colors = heat.colors(8),
  scale_na_value = "gray50",
  own_scale = FALSE,
  sf_color = "black",
  sf_fill = "gray",
  sf_linewidth = 0.1,
  sf_alpha = 1.0,
  display_plot = TRUE
){
  # Check aes_fill
  if(is.null(states_col) | is.null(value_col)){
    stop("Both the states and value column names from the data frame must be assigned")
  }
  if(is.null(scale_breaks)){
    stop("The scale_breaks parameter must be assigned")
  }

  return_lst <- list()

  lower_48_crs <- 5070
  alaska_crs <- 4425
  hawaii_crs <- 26962
  pureto_crs <- 32161

  # Get the simple feature geometries for the states
  states_sf <- RcensusPkg::tiger_states_sf(
    vintage = vintage,
    resol = resol,
    general = T,
    sf_info = F,
    output_dir = output_dir
  )

  # join states_sf with df
  dt <- data.table::as.data.table(df)
  # reformat the state names column so we're on the same page
  dt[, NAME := tolower(dt[[states_col]])]

  states_dt <- data.table::as.data.table(states_sf) %>%
    .[, NAME := tolower(NAME)]

  data.table::setkeyv(dt, cols = "NAME")
  data.table::setkeyv(states_dt, cols = "NAME")
  data_sf <- states_dt[dt, nomatch = 0] %>%
    sf::st_as_sf(.)

  # Remove AK,HI,PR from data_sf
  states_lower_48_sf <- data_sf %>%
    data.table::as.data.table(.) %>%
    .[!NAME %in% c("alaska","hawaii","puerto rico"),] %>%
    sf::st_as_sf(.) %>%
    sf::st_transform(lower_48_crs)
    # for debug purposes to find crs
    # lower_crs <- crsuggest::suggest_crs(states_lower_48_sf)
    # lower_top_crs <- crsuggest::suggest_top_crs(states_lower_48_sf)
    # browser()

  # Get plot grob for lower 48 states
  states_plot <- RspatialPkg::get_geom_sf(
    sf = states_lower_48_sf,
    aes_fill = value_col,
    aes_text = text_col,
    text_size = text_size,
    text_color = text_color,
    text_fontface = text_fontface,
    title = title,
    center_titles = T,
    hide_x_tics = T,
    hide_y_tics = T,
    sf_color = sf_color,
    sf_fill = sf_fill,
    sf_linewidth = sf_linewidth,
    sf_alpha = sf_alpha,
    scale_breaks = scale_breaks,
    scale_values = scale_values,
    scale_limits = scale_limits,
    scale_labels = scale_labels,
    scale_colors = scale_colors,
    scale_na_value = scale_na_value,
    own_scale = own_scale,
    show_legend = show_legend,
    legend_pos = legend_pos
  ) + theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.margin = unit(rep(0.1,4),"cm")
  )

  return_lst[["us_states"]] = states_plot

  # Convert ggplot2 object to grob
  states_grob <- ggplot2::ggplotGrob(states_plot)

  # Get geometries/grobs for "outer" states
  if("alaska" %in% data_sf$NAME){
    alaska_sf <- data_sf %>%
      data.table::as.data.table(.) %>%
      .[NAME == "alaska",] %>%
      sf::st_as_sf(.) %>%
      sf::st_transform(alaska_crs)

    alaska_plot <- RspatialPkg::get_geom_sf(
      sf = alaska_sf,
      aes_fill = value_col,
      aes_text = text_col,
      text_size = text_size,
      text_color = text_color,
      text_fontface = text_fontface,
      hide_x_tics = T,
      hide_y_tics = T,
      sf_color = sf_color,
      sf_fill = sf_fill,
      sf_linewidth = sf_linewidth,
      sf_alpha = sf_alpha,
      scale_breaks = scale_breaks,
      scale_values = scale_values,
      scale_limits = scale_limits,
      scale_labels = scale_labels,
      scale_colors = scale_colors,
      scale_na_value = scale_na_value,
      own_scale = own_scale,
      show_legend = F
    ) + theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.margin = unit(rep(0.1,4),"cm")
    )

    return_lst[["alaska"]] <- alaska_plot
    alaska_grob <- ggplot2::ggplotGrob(alaska_plot)
  }

  if("hawaii" %in% data_sf$NAME){
    hawaii_sf <- data_sf %>%
      data.table::as.data.table(.) %>%
      .[NAME == "hawaii",] %>%
      sf::st_as_sf(.) %>%
      sf::st_transform(hawaii_crs)

    hawaii_plot <- RspatialPkg::get_geom_sf(
      sf = hawaii_sf,
      aes_fill = value_col,
      aes_text = text_col,
      text_size = text_size,
      text_color = text_color,
      text_fontface = text_fontface,
      hide_x_tics = T,
      hide_y_tics = T,
      sf_color = sf_color,
      sf_fill = sf_fill,
      sf_linewidth = sf_linewidth,
      sf_alpha = sf_alpha,
      scale_breaks = scale_breaks,
      scale_values = scale_values,
      scale_limits = scale_limits,
      scale_labels = scale_labels,
      scale_colors = scale_colors,
      scale_na_value = scale_na_value,
      own_scale = own_scale,
      show_legend = F
    ) + theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.margin = unit(rep(0.1,4),"cm")
    )

    return_lst[["hawaii"]] = hawaii_plot
    hawaii_grob <- ggplot2::ggplotGrob(hawaii_plot)
  }
  if("puerto rico" %in% data_sf$NAME){
    puerto_sf <- data_sf %>%
      data.table::as.data.table(.) %>%
      .[NAME == "puerto rico",] %>%
      sf::st_as_sf(.) %>%
      sf::st_transform(pureto_crs)

    puerto_plot <- RspatialPkg::get_geom_sf(
      sf = puerto_sf,
      aes_fill = value_col,
      aes_text = text_col,
      text_size = text_size,
      text_color = text_color,
      text_fontface = text_fontface,
      hide_x_tics = T,
      hide_y_tics = T,
      sf_color = sf_color,
      sf_fill = sf_fill,
      sf_linewidth = sf_linewidth,
      sf_alpha = sf_alpha,
      scale_breaks = scale_breaks,
      scale_values = scale_values,
      scale_limits = scale_limits,
      scale_labels = scale_labels,
      scale_colors = scale_colors,
      scale_na_value = scale_na_value,
      own_scale = own_scale,
      show_legend = F
    ) + theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.margin = unit(rep(0.1,4),"cm")
    )

    return_lst[["puerto_rico"]] = puerto_plot
    puerto_grob <- ggplot2::ggplotGrob(puerto_plot)
  }

  plots_table <- gtable::gtable(
    name = "plots_table",
    widths = unit(rep(1,15),"null"),
    heights = unit(rep(1,15),"null")
  )
  # Debug arrangement
  #gtable::gtable_show_layout(plots_table)

  plots_table <- gtable::gtable_add_grob(
    plots_table,
    grobs = list(
      states_grob
    ),
    t = 5,
    l = 5,
    r = 15,
    b = 12
  )
  if("alaska" %in% data_sf$NAME){
    plots_table <- gtable::gtable_add_grob(
      plots_table,
      grobs = list(
        alaska_grob
      ),
      t = 2.5,
      l = 1,
      r = 5,
      b = 6.5
    )
  }
  if("hawaii" %in% data_sf$NAME){
    plots_table <- gtable::gtable_add_grob(
      plots_table,
      grobs = list(
        hawaii_grob
      ),
      t = 7,
      l = 1,
      r = 4.5,
      b = 11
    )
  }
  if("puerto rico" %in% data_sf$NAME){
    plots_table <- gtable::gtable_add_grob(
      plots_table,
      grobs = list(
        puerto_grob
      ),
      t = 12.9,
      l = 13,
      r = 13.2,
      b = 13.1
    )
  }

  # for debug purposes
  #plot(plots_table)

  # Display plot table?
  a_plot <- ggplotify::as.ggplot(plots_table)
  if(display_plot){
    a_plot
  }else{
    return(return_lst)
  }
}
