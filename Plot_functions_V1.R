# First try to Standarise plot functions with consistent styling

require(ggplot2)
require(wesanderson)
require(ggrepel)
require(scales)

#' Common theme styling settings for all plots
#' @return A ggplot theme object
create_standard_theme <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.position = "right",
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}

#' Standard color palette generator
#' @param n Number of colors needed
#' @return Vector of color values
get_standard_colors <- function(n) {
  wes_palette("Zissou1", n = n, type = "continuous")
}

#' Standard text label settings for ggrepel
#' @return List of ggrepel parameters
get_standard_repel_settings <- function() {
  list(
    size = 3,
    fontface = "bold",
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    segment.color = "grey50",
    segment.size = 0.2,
    force = 1,
    max.overlaps = Inf
  )
}

#' Create a time series line plot with labels
#' @param data Dataframe containing the data
#' @param x_var Name of x-axis variable (typically Year)
#' @param y_var Name of y-axis variable to plot
#' @param group_var Grouping variable (typically Location or Category)
#' @param title Plot title
#' @param y_label Y-axis label
#' @param label_format Function to format labels
create_time_series_plot <- function(data, x_var, y_var, group_var, 
                                    title, y_label, 
                                    label_format = scales::comma) {
  label_data <- data %>%
    group_by(!!sym(group_var)) %>%
    slice(c(1, n())) %>%
    ungroup()
  
  repel_settings <- get_standard_repel_settings()
  
  ggplot(data, 
         aes(x = !!sym(x_var), 
             y = !!sym(y_var), 
             color = !!sym(group_var),
             group = !!sym(group_var))) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    geom_text_repel(
      data = label_data,
      aes(label = paste(!!sym(group_var), label_format(!!sym(y_var)))),
      direction = "y",
      hjust = ifelse(label_data[[x_var]] == min(label_data[[x_var]]), 1, 0),
      nudge_x = ifelse(label_data[[x_var]] == min(label_data[[x_var]]), -0.5, 0.5),
      do.call(what = ggrepel::geom_text_repel, repel_settings)
    ) +
    scale_color_manual(values = get_standard_colors(length(unique(data[[group_var]])))) +
    labs(title = title,
         x = "Year",
         y = y_label) +
    create_standard_theme() +
    theme(legend.position = "none")
}

#' Create a stacked percentage composition plot over time
#' @param data Dataframe containing the data
#' @param x_var Name of x-axis variable (typically Year)
#' @param fill_var Category variable for filling
#' @param value_var Name of the value variable
#' @param location_filter Optional location to filter by
#' @param title Plot title
create_composition_plot <- function(data, x_var, fill_var, value_var,
                                    location_filter = NULL, title) {
  if (!is.null(location_filter)) {
    data <- data %>% filter(Location == location_filter)
  }
  
  repel_settings <- get_standard_repel_settings()
  
  ggplot(data, 
         aes(x = !!sym(x_var), 
             y = !!sym(value_var), 
             fill = !!sym(fill_var))) +
    geom_area(position = "fill") +
    scale_fill_manual(values = get_standard_colors(length(unique(data[[fill_var]])))) +
    labs(title = title,
         x = "Year",
         y = "Percentage",
         fill = "Category") +
    create_standard_theme() +
    scale_y_continuous(labels = scales::percent_format()) +
    geom_text_repel(
      aes(label = scales::percent(!!sym(value_var), accuracy = 0.1)),
      position = position_fill(vjust = 0.5),
      do.call(what = ggrepel::geom_text_repel, repel_settings)
    )
}

#' Create a single-year stacked bar plot
#' @param data Dataframe containing the data
#' @param x_var Name of x-axis variable
#' @param y_var Name of value variable
#' @param fill_var Category variable for filling
#' @param title Plot title
#' @param y_label Y-axis label
create_stacked_bar_plot <- function(data, x_var, y_var, fill_var,
                                    title, y_label) {
  repel_settings <- get_standard_repel_settings()
  
  ggplot(data, 
         aes(x = "", y = !!sym(y_var), fill = !!sym(fill_var))) +
    geom_bar(stat = "identity", width = 0.6) +
    scale_fill_manual(values = get_standard_colors(length(unique(data[[fill_var]])))) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 100),
      breaks = seq(0, 100, 10)
    ) +
    labs(title = title,
         x = NULL,
         y = y_label,
         fill = "Category") +
    create_standard_theme() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    geom_text_repel(
      aes(label = paste0(!!sym(fill_var), "\n", 
                         scales::percent(!!sym(y_var), accuracy = 0.1))),
      position = position_stack(vjust = 0.5),
      do.call(what = ggrepel::geom_text_repel, repel_settings)
    )
}

#' Create a faceted composition plot
#' @param data Dataframe containing the data
#' @param x_var Name of x-axis variable
#' @param y_var Name of value variable
#' @param fill_var Category variable for filling
#' @param facet_var Variable to facet by
#' @param title Plot title
create_faceted_composition_plot <- function(data, x_var, y_var, fill_var,
                                            facet_var, title) {
  repel_settings <- get_standard_repel_settings()
  
  ggplot(data, 
         aes(x = !!sym(x_var), 
             y = !!sym(y_var), 
             fill = !!sym(fill_var))) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(values = get_standard_colors(length(unique(data[[fill_var]])))) +
    labs(title = title,
         x = "Location",
         y = "Percentage") +
    create_standard_theme() +
    facet_wrap(as.formula(paste("~", facet_var)), 
               scales = "free", 
               labeller = label_both) +
    scale_y_continuous(labels = scales::percent_format()) +
    geom_text_repel(
      aes(label = scales::percent(!!sym(y_var), accuracy = 0.1)),
      position = position_fill(vjust = 0.5),
      do.call(what = ggrepel::geom_text_repel, repel_settings)
    )
}

# Function for saving plots
save_plot <- function(plot, filename, width = 12, height = 8) {
  ggsave(
    filename = here::here("Figures", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}
