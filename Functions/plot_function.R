create_population_plot <- function(data, 
                                   location_var = "Location", 
                                   category_var = "category_var", 
                                   year_var = "Year", 
                                   variable_name = "var_name",
                                   title = "Total Population by Category and City",
                                   x_label = "Year",
                                   y_label = "Total Population (thousands)",
                                   palette = "Zissou1",
                                   line_size = 1.2,
                                   label_size = 3,
                                   title_size = 16,  
                                   save_plot = FALSE,
                                   filename = "TOT_POP.png",
                                   width = 10,
                                   height = 8,
                                   locations,
                                   numeric_columns) {
  
  # Order of locations based on their last data point
  location_order <- data_merged %>%
    group_by(!!sym(location_var)) %>%
    summarize(last_value = last(!!sym(variable_name))) %>%
    arrange(desc(last_value)) %>%
    pull(!!sym(location_var))
  
  data_merged[[location_var]] <- factor(data_merged[[location_var]], levels = location_order)
  
  # Create interaction factor
  data_merged$interaction_factor <- interaction(data_merged[[location_var]], data_merged[[category_var]], drop = TRUE)
  
  # Get unique levels of the interaction factor
  interaction_levels <- levels(data_merged$interaction_factor)
  
  # Adjust the number of colors to match the number of interaction levels
  colors <- wes_palette(palette, n = length(interaction_levels), type = "continuous")
  
  # Get the last year in the dataset
  first_year <- min(data_merged[[year_var]])
  last_year <- max(data_merged[[year_var]])
  
  # Calculate breaks to include first and last year
  year_breaks <- unique(c(first_year, 
                          seq(first_year + (5 - first_year %% 5), last_year - 1, by = 5), 
                          last_year))
  
  # Create the plot
  p <- ggplot(data_merged, aes(x = !!sym(year_var),
                               y = !!sym(variable_name),
                               color = interaction_factor,
                               group = interaction_factor)) +
    geom_line(linewidth = line_size) +
    geom_point() +
    scale_color_manual(values = colors,
                       breaks = interaction_levels,
                       labels = interaction_levels) +
    scale_y_continuous(labels = comma_format()) +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = title_size, hjust = 0.5),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      plot.margin = margin(10, 40, 10, 10)
    )
  
  # Function to format numbers with comma separators
  format_number <- function(x) {
    format(round(x), big.mark = ",", scientific = FALSE)
  }
  
  # Filter data for labels, keeping only the highest category for each location
  label_data <- data_merged %>%
    group_by(!!sym(location_var)) %>%
    filter(!!sym(category_var) == last(!!sym(category_var))) %>%
    ungroup()
  
  # Labels for start of lines
  start_label_data <- label_data %>%
    group_by(!!sym(location_var), !!sym(category_var)) %>%
    slice_min(!!sym(year_var)) %>%
    ungroup()
  
  # Labels for end of lines
  end_label_data <- label_data %>%
    group_by(!!sym(location_var), !!sym(category_var)) %>%
    slice_max(!!sym(year_var)) %>%
    ungroup()
  
  p <- p +
    geom_text_repel(data = start_label_data,
                    aes(label = format_number(!!sym(variable_name))),
                    nudge_x = -1,
                    direction = "y",
                    hjust = 1,
                    segment.size = 0.2,
                    segment.color = "grey50",
                    box.padding = 0.5,
                    point.padding = 0.5,
                    force = 2,
                    size = label_size,
                    fontface = "bold",
                    bg.color = "white",
                    bg.r = 0.15) +
    geom_text_repel(data = end_label_data,
                    aes(label = paste0(!!sym(location_var), "-", !!sym(category_var), "\n", 
                                       format_number(!!sym(variable_name)))),
                    nudge_x = 1,
                    direction = "y",
                    hjust = 0,
                    segment.size = 0.2,
                    segment.color = "grey50",
                    box.padding = 0.5,
                    point.padding = 0.5,
                    force = 2,
                    size = label_size,
                    fontface = "bold",
                    bg.color = "white",
                    bg.r = 0.15) +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(first_year - 1, last_year + 5),
                       breaks = year_breaks,
                       labels = year_breaks)
  if (save_plot) {
    ggsave(filename = here("Figures", filename), plot = p, width = width, height = height)
  }
  
  return(p)
}