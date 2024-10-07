#@
#@

create_population_plot <- function(data, 
                                   location_var = "Location", 
                                   category_var = "category_var", 
                                   year_var = "Year", 
                                   variable_name = "var_name",
                                   title = "Total Population by Category and City",
                                   x_label = "Year",
                                   y_label = "Total Population (thousands)",
                                   palette = "Zissou1",
                                   line_size = 1.5,
                                   label_size = 3,
                                   save_plot = FALSE,
                                   filename = "TOT_POP.png",
                                   width = 10,
                                   height = 8) {
  
  # Process the data
  data_merged <- data %>%
    mutate(variable_name = as.numeric(!!sym(variable_name)))
  
  # Order of locations based on their last data point
  location_order <- data_merged %>%
    group_by(!!sym(location_var)) %>%
    summarize(last_value = last(variable_name)) %>%
    arrange(desc(last_value)) %>%
    pull(!!sym(location_var))
  
  data_merged[[location_var]] <- factor(data_merged[[location_var]], levels = location_order)
  
  # Create interaction factor
  data_merged$interaction_factor <- interaction(data_merged[[location_var]], data_merged[[category_var]], drop = TRUE)
  
  # Get unique levels of the interaction factor
  interaction_levels <- levels(data_merged$interaction_factor)
  
  # Adjust the number of colors to match the number of interaction levels
  colors <- wes_palette(palette, n = length(interaction_levels), type = "continuous")
  
  # Create the plot
  p <- ggplot(data_merged, aes(x = !!sym(year_var),
                               y = !!sym(variable_name),
                               color = interaction_factor,
                               group = interaction_factor)) +
    geom_line(size = line_size) +
    geom_point() +
    scale_color_manual(values = colors,
                       breaks = interaction_levels,
                       labels = interaction_levels) +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal()
  
  # Labels for ggrepel
  label_data <- data_merged %>%
    group_by(!!sym(location_var), !!sym(category_var)) %>%
    slice_max(!!sym(year_var)) %>%
    ungroup()
  
  # Function to format numbers with comma separators
  format_number <- function(x) {
    format(round(x), big.mark = ",", scientific = FALSE)
  }
  
  p <- p +
    geom_text_repel(data = label_data,
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
                    size = label_size) +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(min(data_merged[[year_var]]), max(data_merged[[year_var]]) + 1))
  
  if (save_plot) {
    ggsave(filename = here("Figures", filename), plot = p, width = width, height = height)
  }
  
  return(p)
}