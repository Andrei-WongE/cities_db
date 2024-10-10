library(dplyr)
library(ggplot2)
library(ggrepel)
library(wesanderson)
library(here)

create_stacked_bar_plot <- function(data, year, variable, category_var = NULL, 
                                    title, y_label, output_filename) {
  # Error checking
  if (!is.data.frame(data)) {
    stop("The 'data' parameter must be a data frame.")
  }
  
  required_cols <- c("Year", "Location", variable)
  if (!is.null(category_var)) {
    required_cols <- c(required_cols, category_var)
  }
  
  if (!all(required_cols %in% names(data))) {
    stop(paste("The data frame must contain columns:", 
               paste(required_cols, collapse = ", ")))
  }
  
  # Filter data and calculate percentages
  plot_data <- data %>%
    filter(Year == year) %>%
    group_by(Location)
  
  if (!is.null(category_var)) {
    plot_data <- plot_data %>%
      summarise(
        Value = sum(!!sym(variable), na.rm = TRUE),
        category_var = first(!!sym(category_var))
      )
  } else {
    plot_data <- plot_data %>%
      summarise(
        Value = sum(!!sym(variable), na.rm = TRUE)
      )
  }
  
  plot_data <- plot_data %>%
    ungroup() %>%
    mutate(
      Total = sum(Value, na.rm = TRUE),
      Percentage = Value / Total * 100
    ) %>%
    arrange(desc(Percentage))
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = "", y = Percentage, fill = Location)) +
    geom_bar(stat = "identity", width = 0.6) +
    scale_fill_manual(values = wes_palette("Zissou1", n = nrow(plot_data), type = "continuous")) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits