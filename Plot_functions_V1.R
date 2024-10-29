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

## Generic bar charts comparing cities
#' Generate a bar plot for specified years and variable
#' @param data Dataframe containing the data
#' @param years Single year or vector of years to include in the plot
#' @param variable_name Name of the variable to plot (column name in data)
#' @param title Optional custom title for the plot
#' @param subtitle Optional custom subtitle for the plot
#' @param y_axis_label Optional custom y-axis label
#' @param show_values Logical, whether to show value labels on bars (default: TRUE)
#' @param value_format Function to format values (default: comma format)
#' @param source_text Text to display as source (default: NULL)
#' @param source_size Font size for source text (default: 8)
#' @return A ggplot object
generate_bar_plot <- function(data, 
                              years,
                              variable_name,
                              title = NULL,
                              subtitle = NULL,
                              y_axis_label = NULL,
                              show_values = TRUE,
                              value_format = scales::comma,
                              source_text = NULL,
                              source_size = 8) {
  
  # Convert single year to vector if necessary
  years <- unique(as.numeric(years))
  
  # Validate years
  if (length(years) == 0) {
    stop("No valid years provided")
  }
  
  # Filter data for specified years
  plot_data <- data[data$year %in% years, ]
  
  # Check if we have data
  if (nrow(plot_data) == 0) {
    stop("No data found for the specified years")
  }
  
  # Set default title if not provided
  if (is.null(title)) {
    if (length(years) == 1) {
      title <- paste(variable_name, "in", years)
    } else {
      title <- paste("Distribution of", variable_name, "by Year")
    }
  }
  
  # Set default y-axis label if not provided
  if (is.null(y_axis_label)) {
    y_axis_label <- variable_name
  }
  
  # Calculate plot height based on whether source is present
  plot_margin <- if (!is.null(source_text)) {
    margin(t = 20, r = 20, b = 40, l = 20)  # Extra bottom margin for source
  } else {
    margin(20, 20, 20, 20)
  }
  
  # Create the base plot
  p <- ggplot(plot_data, aes_string(x = "year", y = variable_name)) +
    geom_bar(stat = "identity", 
             fill = get_standard_colors(1),
             alpha = 0.8,
             width = 0.7) +
    create_standard_theme()
  
  # Adjust x-axis based on number of years
  if (length(years) == 1) {
    p <- p + 
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),  # Center align x-axis label
        panel.grid.major.x = element_blank()  # Remove vertical grid for single year
      )
  }
  
  # Add remaining theme elements and labels
  p <- p +
    theme(plot.margin = plot_margin) +
    labs(title = title,
         subtitle = subtitle,
         x = if(length(years) == 1) "" else "Year",  # Remove x-axis label for single year
         y = y_axis_label)
  
  # Add value labels if requested
  if (show_values) {
    p <- p + 
      geom_text(aes_string(label = paste0("value_format(", variable_name, ")")),
                vjust = -0.5,
                size = 3)
  }
  
  # Scale y-axis using comma format
  p <- p + scale_y_continuous(labels = value_format)
  
  # Add source if provided
  if (!is.null(source_text)) {
    p <- p + 
      labs(caption = paste0("Source: ", source_text)) +
      theme(
        plot.caption = element_text(
          size = source_size,
          hjust = 0,  # Left align
          margin = margin(t = 20)  # Add space above source
        )
      )
  }
  
  return(p)
}

#' Save a plot to a specified directory
#' @param plot ggplot object to save
#' @param filename Name of the file to save (including extension)
#' @param path Directory path where the plot should be saved (default: NULL)
#' @param width Width of the plot in inches (default: 12)
#' @param height Height of the plot in inches (default: 8)
#' @param create_dir Logical, whether to create directory if it doesn't exist (default: TRUE)
#' @return None (saves plot to file)
save_plot <- function(plot, 
                      filename, 
                      path = NULL,
                      width = 12, 
                      height = 8,
                      create_dir = TRUE) {
  
  # If no path is provided, use the default "Figures" directory
  if (is.null(path)) {
    path <- "Figures"
  }
  
  # Convert to absolute path if relative path is provided
  full_path <- here::here(path)
  
  # Create directory if it doesn't exist and create_dir is TRUE
  if (!dir.exists(full_path)) {
    if (create_dir) {
      dir.create(full_path, recursive = TRUE)
      message(sprintf("Created directory: %s", full_path))
    } else {
      stop(sprintf("Directory does not exist: %s", full_path))
    }
  }
  
  # Construct full file path
  file_path <- file.path(full_path, filename)
  
  # Save the plot
  ggsave(
    filename = file_path,
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
  
  # Confirm save location
  message(sprintf("Plot saved to: %s", file_path))
}

