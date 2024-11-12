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
                                   category_var = "cities_list", 
                                   year_var = "Year", 
                                   variable_name = "POPTOTT",
                                   horizontal = FALSE,
                                   value_format = scales::label_number(
                                     scale = 1,
                                     accuracy = 0,
                                     big.mark = ",",
                                     decimal.mark = "."),
                                   title = "Population by Location within Categories",
                                   subtitle = NULL,
                                   source_text = NULL,
                                   source_size = 8,
                                   x_label = NULL,
                                   y_label = "Total Population (thousands)",
                                   palette = "Zissou1",
                                   line_size = 1.2,
                                   label_size = 3,
                                   title_size = 16,
                                   category_order = c("asc", "desc", "by_name", "as_is")[4],
                                   within_group_order = c("asc", "desc", "by_name", "as_is")[4],
                                   ordered_categories = NULL,  # Nested list for reference order
                                   save_plot = FALSE,
                                   filename = "TOT_POP.png",
                                   width = 10,
                                   height = 8) {
  
  # Ensure data is properly formatted
  data <- data %>%
    dplyr::filter(!is.na(!!sym(variable_name))) %>%
    dplyr::mutate(
      !!sym(variable_name) := as.numeric(!!sym(variable_name)),
      !!sym(year_var) := as.numeric(!!sym(year_var)),
      !!sym(category_var) := gsub("_", " ", !!sym(category_var)),
      !!sym(location_var) := gsub("_", " ", !!sym(location_var))
    )
  
  # Extract reference orders from ordered_categories if provided
  if (!is.null(ordered_categories)) {
    reference_category_order <- names(ordered_categories)
    
    # Create a named vector to map locations to their categories
    location_to_category <- unlist(lapply(names(ordered_categories), function(cat) {
      setNames(rep(cat, length(ordered_categories[[cat]])), ordered_categories[[cat]])
    }))
    
    # Get the complete location order across all categories
    reference_location_order <- unlist(ordered_categories)
  }
  
  # Handle both category and location ordering based on order parameters
  if(category_order == "as_is" && within_group_order == "as_is" && !is.null(ordered_categories)) {
    # First, convert locations to factor with the overall reference order
    data <- data %>%
      mutate(
        !!sym(location_var) := factor(!!sym(location_var), 
                                      levels = reference_location_order),
        !!sym(category_var) := factor(!!sym(category_var),
                                      levels = reference_category_order)
      )
  } else {
    # Handle other ordering combinations
    if(within_group_order == "asc") {
      data <- data %>%
        group_by(!!sym(category_var)) %>%
        arrange(!!sym(location_var), .by_group = TRUE) %>%
        ungroup()
    } else if(within_group_order == "desc") {
      data <- data %>%
        group_by(!!sym(category_var)) %>%
        arrange(desc(!!sym(location_var)), .by_group = TRUE) %>%
        ungroup()
    }
    
    if(category_order == "asc") {
      data <- data %>% arrange(!!sym(category_var))
    } else if(category_order == "desc") {
      data <- data %>% arrange(desc(!!sym(category_var)))
    }
  }
  
  # Set up colors using the final order of locations
  unique_locations <- levels(data[[location_var]])
  if(is.null(unique_locations)) unique_locations <- unique(data[[location_var]])
  n_locations <- length(unique_locations)
  location_colors <- setNames(
    colorRampPalette(wes_palette(palette, type = "continuous"))(n_locations),
    unique_locations
  )
  
  # Calculate plot dimensions and spacing
  max_value <- max(data[[variable_name]], na.rm = TRUE)
  longest_label <- max(nchar(as.character(data[[location_var]])))
  n_bars <- nrow(data)
  
  # Dynamic spacing calculations
  label_space_factor <- 0.08
  value_label_space <- max_value * 0.05
  location_label_space <- max_value * (longest_label * label_space_factor)
  
  # Calculate y-axis limits
  y_min <- 0
  y_max <- max_value * 1.15
  
  # Create position dodge object
  dodge_width <- 0.9
  pos_dodge <- position_dodge(width = dodge_width)
  
  # Calculate bottom margin based on label length and angle
  bottom_margin <- 10 + (longest_label * 2)
  
  # Create the plot
  p <- ggplot(data, 
              aes(x = !!sym(category_var),
                  y = !!sym(variable_name),
                  fill = !!sym(location_var))) +
    geom_bar(stat = "identity",
             position = pos_dodge,
             width = 0.8) +
    geom_text(aes(label = value_format(!!sym(variable_name))),
              position = pos_dodge,
              vjust = -0.5,
              size = label_size) +
    geom_text(aes(y = 0,
                  label = !!sym(location_var)),
              position = pos_dodge,
              angle = 45,
              hjust = 1,
              vjust = 2,
              fontface = "bold",
              color = "black",
              size = label_size) +
    scale_fill_manual(values = location_colors) +
    scale_y_continuous(
      labels = value_format,
      limits = c(y_min, y_max),
      expand = expansion(mult = c(0.2, 0.1))
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = title_size, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title.x = element_text(face = "bold", margin = margin(t = 50)),
      axis.title.y = element_text(face = "bold"),
      axis.text.x = element_blank(),
      axis.text.y = element_text(face = "plain"),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(
        t = 30,
        r = 30,
        b = bottom_margin,
        l = 50
      ),
      legend.position = "none",
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      plot.caption = element_text(size = source_size, color = "gray30", hjust = 1)
    ) +
    labs(title = title,
         subtitle = subtitle,
         caption = source_text,
         x = x_label,
         y = y_label)
  
  if (save_plot) {
    ggsave(filename = here("Output", "MENA", filename), 
           plot = p, 
           width = width, 
           height = height,
           dpi = 600)
  }
  
  return(p)
}