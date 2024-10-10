create_structure_plot <- function(data, output_file, is_decimal = TRUE, palette_name = "Zissou1") {
  # Internal function to generate colors
  generate_colors <- function(n_categories, palette_name) {
    base_colors <- wes_palette(palette_name, n = n_categories, type = "continuous")
    if (length(base_colors) < n_categories) {
      color_function <- colorRampPalette(base_colors)
      base_colors <- color_function(n_categories)
    }
    return(base_colors)
  }
  
  # Main plotting function
  data$Year <- as.numeric(as.character(data$Year))
  
  # Convert percentage to decimal if it's not already
  if (!is_decimal) {
    data$Percentage <- data$Percentage / 100
  }
  
  data <- data %>%
    group_by(Location, Year) %>%
    arrange(desc(Employment_category)) %>%  # Reverse the order for stacking
    mutate(CumulativePercentage = cumsum(Percentage),
           YPosition = CumulativePercentage - Percentage/2) %>%
    ungroup()
  
  n_categories <- length(unique(data$Employment_category))
  colors <- generate_colors(n_categories, palette_name)
  
  years <- unique(data$Year)
  start_year <- min(years)
  end_year <- max(years)
  
  label_data <- data %>%
    filter(Year %in% c(start_year, end_year)) %>%
    mutate(Label = scales::percent(Percentage, accuracy = 0.1),
           x_position = if_else(Year == start_year, Year - 0.5, Year + 0.5))
  
  p <- ggplot(data, aes(x = Year, y = Percentage, fill = Employment_category)) +
    geom_area(position = "stack") +
    scale_fill_manual(values = colors) +
    facet_wrap(~ Location, ncol = 1, scales = "free_y") +
    labs(title = "Employment by Sector (percentage) in Colombo (2011-2019)",
         x = "Year",
         y = "Percentage") +
    theme_minimal() +
    scale_x_continuous(breaks = years) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16, face = "bold"))
  
  p <- p + geom_text_repel(
    data = label_data,
    aes(x = x_position, y = YPosition, label = Label, color = Employment_category),
    direction = "y",
    segment.color = "grey50",
    show.legend = FALSE,
    size = 3,
    force = 1,
    max.overlaps = Inf
  )
  
  p <- p + scale_color_manual(values = colors)
  
  ggsave(filename = output_file, width = 12, height = 8)
  return(p)
  gc()
}