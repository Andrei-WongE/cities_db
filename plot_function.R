#' Create a Population Plot with Category-Specific Color Gradients
#'
#' @param data A data frame containing the population data
#' @param location_var Character string specifying the column name for locations
#' @param category_var Character string specifying the column name for categories
#' @param year_var Character string specifying the column name for years
#' @param variable_name Character string specifying the column name for the values
#' @param title Character string for the plot title
#' @param x_label Character string for x-axis label
#' @param y_label Character string for y-axis label
#' @param palette Character string specifying the Wes Anderson palette
#' @param line_size Numeric value for line thickness
#' @param label_size Numeric value for label text size
#' @param title_size Numeric value for title text size
#' @param category_order Character, either "asc" or "desc" for category ordering
#' @param within_group_order Character, either "asc" or "desc" for within-group ordering
#' @param save_plot Logical indicating whether to save the plot
#' @param filename Character string for the output filename if saving
#' @param width Numeric value for plot width in inches when saving
#' @param height Numeric value for plot height in inches when saving
#'
#' @return A ggplot2 object containing the visualization 
# Load required packages
library(ggplot2)
library(dplyr)
library(scales)
library(wesanderson)
library(ggrepel)
library(here)

library(ggplot2)
library(dplyr)
library(scales)
library(wesanderson)
library(ggrepel)
library(here)

create_population_plot <- function(data, 
                                   location_var = "Location",
                                   category_var = "category_var", 
                                   year_var = "Year", 
                                   variable_name = "var_name",
                                   value_format = scales::label_number(
                                     unit = "m", 
                                     scale = 1e-3,
                                     accuracy = 0.1),
                                   title = "Population by Location within Categories",
                                   subtitle = NULL,
                                   source_text = NULL,
                                   source_size = 8,
                                   x_label = "Categories",
                                   y_label = "Population (thousands)",
                                   palette = "Zissou1",
                                   line_size = 1.2,
                                   label_size = 3,
                                   title_size = 16,
                                   category_order = c("asc", "desc", "by_name")[3],
                                   within_group_order = "by_name",
                                   save_plot = FALSE,
                                   filename = "TOT_POP.png",
                                   width = 10,
                                   height = 8) {
  
  required_cols <- c(location_var, category_var, year_var, variable_name)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  if (nrow(data) == 0) {
    stop("Input data frame is empty")
  }
  
  if (all(is.na(data[[category_var]]))) {
    stop(sprintf("All values in %s are NA", category_var))
  }
  
  data <- data %>%
    dplyr::filter(!is.na(!!sym(category_var))) %>%
    dplyr::mutate(
      !!sym(variable_name) := as.numeric(!!sym(variable_name)),
      !!sym(year_var) := as.numeric(!!sym(year_var))
    )
  
  # Sort locations alphabetically and assign fixed colors
  unique_locations <- sort(unique(data[[location_var]]))
  n_locations <- length(unique_locations)
  location_colors <- setNames(
    colorRampPalette(wes_palette(palette, type = "continuous"))(n_locations),
    unique_locations
  )
  
  # Create location-category mapping with fixed colors
  data <- data %>%
    mutate(
      location_with_category = paste0(!!sym(location_var), " (", !!sym(category_var), ")"),
      location_color = location_colors[!!sym(location_var)]
    )
  
  if (nrow(data) == 0) {
    stop("No valid data remains after removing NA values")
  }
  
  is_single_year <- length(unique(data[[year_var]])) == 1
  
  if (is_single_year) {
    if (category_order == "by_name") {
      data <- data %>%
        arrange(
          !!sym(category_var),
          if(within_group_order == "desc") desc(!!sym(variable_name)) else !!sym(variable_name)
        )
    } else {
      data <- data %>%
        group_by(!!sym(category_var)) %>%
        mutate(category_total = sum(!!sym(variable_name), na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(
          if(category_order == "desc") desc(category_total) else category_total,
          !!sym(category_var),
          if(within_group_order == "desc") desc(!!sym(variable_name)) else !!sym(variable_name)
        )
    }
    
    data[[category_var]] <- factor(data[[category_var]], 
                                   levels = unique(data[[category_var]]))
    data$location_with_category <- factor(data$location_with_category,
                                          levels = unique(data$location_with_category))
    
    p <- ggplot(data, 
                aes(x = !!sym(category_var),
                    y = !!sym(variable_name),
                    fill = !!sym(location_var))) +
      geom_bar(stat = "identity",
               position = position_dodge(width = 0.9),
               width = 0.8) +
      scale_fill_manual(values = location_colors,
                        name = "Location") +
      geom_text(aes(label = value_format(!!sym(variable_name))),
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = label_size)
    
  } else {
    p <- ggplot(data, 
                aes(x = !!sym(year_var),
                    y = !!sym(variable_name),
                    color = !!sym(location_var),
                    group = location_with_category)) +
      geom_line(linewidth = line_size) +
      geom_point(size = 2) +
      scale_color_manual(values = location_colors,
                         name = "Location") +
      scale_x_continuous(breaks = unique(data[[year_var]])) +
      geom_text_repel(data = data %>% 
                        group_by(location_with_category) %>%
                        slice_max(!!sym(year_var)),
                      aes(label = paste0(location_with_category, "\n",
                                         value_format(!!sym(variable_name)))),
                      nudge_x = 0.5,
                      direction = "y",
                      hjust = 0,
                      size = label_size)
  }
  
  p <- p + theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = title_size, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(t = 20, r = 20, b = 30, l = 20),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      plot.caption = element_text(size = source_size, color = "gray30", hjust = 1)
    ) +
    scale_y_continuous(labels = value_format,
                       expand = expansion(mult = c(0, 0.15))) +
    labs(title = title,
         subtitle = subtitle,
         caption = source_text,
         x = x_label,
         y = y_label)
  
  if (save_plot) {
    ggsave(filename = here("Output", "India", filename), 
           plot = p, 
           width = width, 
           height = height,
           dpi = 600)
  }
  
  return(p)
}
