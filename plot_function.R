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

create_population_plot <- function(data, 
                                   location_var = "Location",
                                   category_var = "category_var", 
                                   year_var = "Year", 
                                   variable_name = "var_name",
                                   title = "Population by Location within Categories",
                                   x_label = "Categories",
                                   y_label = "Population (thousands)",
                                   palette = "Zissou1",
                                   line_size = 1.2,
                                   label_size = 3,
                                   title_size = 16,
                                   category_order = "asc",
                                   within_group_order = "asc",
                                   save_plot = FALSE,
                                   filename = "TOT_POP.png",
                                   width = 10,
                                   height = 8) {
  
  # Input validation
  required_cols <- c(location_var, category_var, year_var, variable_name)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Validate data structure before processing
  if (nrow(data) == 0) {
    stop("Input data frame is empty")
  }
  
  # Handle NA values in category_var more carefully
  if (all(is.na(data[[category_var]]))) {
    stop(sprintf("All values in %s are NA", category_var))
  }
  
  # Remove rows where category_var is NA and ensure numeric values
  data <- data %>%
    dplyr::filter(!is.na(!!sym(category_var))) %>%
    dplyr::mutate(
      !!sym(variable_name) := as.numeric(!!sym(variable_name)),
      !!sym(year_var) := as.numeric(!!sym(year_var))
    )
  
  # Validate data after cleaning
  if (nrow(data) == 0) {
    stop("No valid data remains after removing NA values")
  }
  
  # Check if single year
  is_single_year <- length(unique(data[[year_var]])) == 1
  
  if (is_single_year) {
    # Create custom color palette for locations
    n_locations <- length(unique(data[[location_var]]))
    location_colors <- colorRampPalette(wes_palette(palette, type = "continuous"))(n_locations)
    
    # Order the data by the specified criteria
    data <- data %>%
      group_by(!!sym(category_var)) %>%
      mutate(category_total = sum(!!sym(variable_name), na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(
        if(category_order == "desc") desc(category_total) else category_total,
        !!sym(category_var),
        if(within_group_order == "desc") desc(!!sym(variable_name)) else !!sym(variable_name)
      )
    
    # Update factor levels based on ordering
    data[[category_var]] <- factor(data[[category_var]], 
                                   levels = unique(data[[category_var]]))
    data[[location_var]] <- factor(data[[location_var]], 
                                   levels = unique(data[[location_var]]))
    
    # Create the plot
    p <- ggplot(data, 
                aes(x = !!sym(category_var),
                    y = !!sym(variable_name),
                    fill = !!sym(location_var))) +
      geom_bar(stat = "identity",
               position = position_dodge(width = 0.9),
               width = 0.8) +
      scale_fill_manual(values = location_colors,
                        name = "Location") +
      geom_text(aes(label = format(round(!!sym(variable_name)), big.mark = ",")),
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = label_size) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = title_size, hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95")
      ) +
      scale_y_continuous(labels = comma_format(),
                         expand = expansion(mult = c(0, 0.15))) +
      labs(title = title,
           x = x_label,
           y = y_label)
    
  } else {
    # Create plot for multiple years
    p <- ggplot(data, 
                aes(x = !!sym(year_var),
                    y = !!sym(variable_name),
                    color = !!sym(location_var),
                    group = !!sym(location_var))) +
      geom_line(linewidth = line_size) +
      geom_point(size = 2) +
      scale_color_manual(values = wes_palette(palette, n = length(unique(data[[location_var]])), type = "continuous"),
                         name = "Location") +
      scale_x_continuous(breaks = unique(data[[year_var]])) +
      geom_text_repel(data = data %>% 
                        group_by(!!sym(location_var)) %>%
                        slice_max(!!sym(year_var)),
                      aes(label = paste0(!!sym(location_var), "\n",
                                         format(!!sym(variable_name), big.mark = ","))),
                      nudge_x = 0.5,
                      direction = "y",
                      hjust = 0,
                      size = label_size) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = title_size, hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        legend.position = "right",
        legend.title = element_text(face = "bold")
      ) +
      scale_y_continuous(labels = comma_format(),
                         expand = expansion(mult = c(0, 0.15))) +
      labs(title = title,
           x = x_label,
           y = y_label)
  }
  
  if (save_plot) {
    ggsave(filename = here("Output", "India", filename), 
           plot = p, 
           width = width, 
           height = height)
  }
  
  return(p)
}
