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
                                   category_order = c("asc", "desc", "by_name")[3],
                                   within_group_order = "by_name",
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
    
    # Order the data
    if(category_order == "by_name") {
      data <- data %>%
        arrange(!!sym(category_var), !!sym(location_var))
    }
    
    # Set up colors
    unique_locations <- unique(data[[location_var]])
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
    label_space_factor <- 0.08  # Adjust this value to increase/decrease label space
    value_label_space <- max_value * 0.05  # Space for value labels above bars
    location_label_space <- max_value * (longest_label * label_space_factor)  # Space for location labels
    
    # Calculate y-axis limits
    y_min <- 0
    y_max <- max_value * 1.15  # Space for value labels
    
    # Create position dodge object
    dodge_width <- 0.9
    pos_dodge <- position_dodge(width = dodge_width)
    
    # Calculate bottom margin based on label length and angle
    bottom_margin <- 10 + (longest_label * 2)  # Base margin + additional space per character
    
    # Create the plot
    p <- ggplot(data, 
                aes(x = !!sym(category_var),
                    y = !!sym(variable_name),
                    fill = !!sym(location_var))) +
      # Add bars
      geom_bar(stat = "identity",
               position = pos_dodge,
               width = 0.8) +
      # Add value labels above bars
      geom_text(aes(label = value_format(!!sym(variable_name))),
                position = pos_dodge,
                vjust = -0.5,
                size = label_size) +
      # Add location labels below bars
      geom_text(aes(y = 0,
                    label = !!sym(location_var)),
                position = pos_dodge,
                angle = 45,
                hjust = 1,
                vjust = 2,  # Adjusted to move labels lower
                fontface = "bold",
                color = "black",
                size = label_size) +
      # Set scales
      scale_fill_manual(values = location_colors) +
      scale_y_continuous(
        labels = value_format,
        limits = c(y_min, y_max),
        expand = expansion(mult = c(0.2, 0.1))  # Add more space at bottom
      ) +
      # Prevent clipping
      coord_cartesian(clip = "off") +
      # Theme
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
        # Dynamic margins based on content
        plot.margin = margin(
          t = 30,  # Top margin for title
          r = 30,  # Right margin
          b = bottom_margin,  # Dynamic bottom margin
          l = 50   # Left margin for y-axis labels
        ),
        legend.position = "none",
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        plot.caption = element_text(size = source_size, color = "gray30", hjust = 1)
      ) +
      # Labels
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