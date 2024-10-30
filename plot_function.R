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
  
  # Create color gradients for each category using transparency
  create_color_gradient <- function(base_color, n) {
    if(n == 1) return(base_color)
    alpha_values <- seq(0.4, 1, length.out = n)  # Adjust range as needed
    sapply(alpha_values, function(alpha) {
      adjustcolor(base_color, alpha.f = alpha)
    })
  }
  
  # Validate ordering parameters
  if (!category_order %in% c("asc", "desc")) {
    stop("category_order must be either 'asc' or 'desc'")
  }
  if (!within_group_order %in% c("asc", "desc")) {
    stop("within_group_order must be either 'asc' or 'desc'")
  }
  
  # Remove rows where category_var is NA and ensure numeric values
  data <- data %>%
    dplyr::filter(!is.na(!!sym(category_var))) %>%
    mutate(
      !!sym(variable_name) := as.numeric(!!sym(variable_name)),
      !!sym(year_var) := as.numeric(!!sym(year_var))
    )
  
  # Check if we have single or multiple years
  unique_years <- unique(data[[year_var]])
  is_single_year <- length(unique_years) == 1
  
  # Order categories and get the number of unique categories
  category_summary <- data %>%
    group_by(!!sym(category_var)) %>%
    summarize(total_value = sum(!!sym(variable_name), na.rm = TRUE)) %>%
    arrange(if(category_order == "desc") desc(total_value) else total_value)
  
  category_order_levels <- category_summary %>% pull(!!sym(category_var))
  n_categories <- length(category_order_levels)
  
  # Apply category ordering
  data[[category_var]] <- factor(data[[category_var]], levels = category_order_levels)
  
  # Generate base colors for categories using the Wes Anderson palette
  base_colors <- wes_palette(palette, n = n_categories, type = "discrete")
  
  # Order locations within each category and assign colors
  ordered_data <- data %>%
    group_by(!!sym(category_var), !!sym(location_var)) %>%
    summarize(value = sum(!!sym(variable_name), na.rm = TRUE), .groups = "keep") %>%
    arrange(!!sym(category_var), 
            if(within_group_order == "desc") desc(value) else value) %>%
    group_by(!!sym(category_var)) %>%
    mutate(
      location_order = row_number(),
      n_locations = n()
    ) %>%
    ungroup()
  
  # Create color mapping for each location within its category
  color_mapping <- ordered_data %>%
    group_by(!!sym(category_var)) %>%
    group_modify(~ {
      n_locs <- nrow(.x)
      base_color <- base_colors[which(category_order_levels == first(.x[[category_var]]))]
      gradient_colors <- create_color_gradient(base_color, n_locs)
      .x %>% mutate(color = gradient_colors[1:n_locs])
    }) %>%
    ungroup()
  
  # Join colors back to the main data
  data <- data %>%
    left_join(color_mapping %>% 
                dplyr::select(!!sym(category_var), 
                       !!sym(location_var), 
                       location_order,
                       color),
              by = c(category_var, location_var))
  
  # Create ordered location-category factor
  data$location_category <- factor(
    paste(data[[location_var]], "-", data[[category_var]]),
    levels = unique(color_mapping %>%
                      arrange(!!sym(category_var), location_order) %>%
                      mutate(loc_cat = paste(!!sym(location_var), "-", !!sym(category_var))) %>%
                      pull(loc_cat))
  )
  
  if (is_single_year) {
    p <- ggplot(data, 
                aes(x = !!sym(category_var),
                    y = !!sym(variable_name),
                    fill = location_category)) +
      geom_bar(stat = "identity", 
               position = position_dodge(width = 0.9),
               width = 0.8) +
      scale_fill_manual(values = setNames(color_mapping$color,
                                          paste(color_mapping[[location_var]], "-",
                                                color_mapping[[category_var]])),
                        name = "Location") +
      geom_text(aes(label = format(!!sym(variable_name), big.mark = ",")),
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = label_size) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else {
    p <- ggplot(data, 
                aes(x = !!sym(year_var),
                    y = !!sym(variable_name),
                    color = location_category,
                    group = location_category)) +
      geom_line(linewidth = line_size) +
      geom_point(size = 2) +
      scale_color_manual(values = setNames(color_mapping$color,
                                           paste(color_mapping[[location_var]], "-",
                                                 color_mapping[[category_var]])),
                         name = "Location") +
      scale_x_continuous(breaks = unique_years) +
      geom_text_repel(data = data %>% 
                        group_by(location_category) %>%
                        slice_max(!!sym(year_var)),
                      aes(label = paste0(location_category, "\n",
                                         format(!!sym(variable_name), big.mark = ","))),
                      nudge_x = 0.5,
                      direction = "y",
                      hjust = 0,
                      size = label_size)
  }
  
  # Common plot elements
  p <- p +
    scale_y_continuous(labels = comma_format(),
                       expand = expansion(mult = c(0, 0.1))) +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = title_size, hjust = 0.5),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    )
  
  if (save_plot) {
    ggsave(filename = here("Output", "India", filename), 
           plot = p, 
           width = width, 
           height = height)
  }
  
  return(p)
}