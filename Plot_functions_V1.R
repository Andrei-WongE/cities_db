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
#' Generate a visually enhanced bar plot
#' @param data Dataframe containing the data
#' @param x_var Name of the categorical or year variable for x-axis
#' @param y_var Name of the numeric variable to plot
#' @param orientation Direction of the bars ("vertical" or "horizontal")
#' @param sort_bars Whether to sort bars ("descending", "ascending", or "none")
#' @param x_lab Optional custom x-axis label (defaults to x_var)
#' @param y_lab Optional custom y-axis label (defaults to y_var)
#' @param title Optional custom title for the plot
#' @param subtitle Optional custom subtitle for the plot
#' @param show_values Logical, whether to show value labels on bars (default: TRUE)
#' @param value_format Function to format values (default: comma format)
#' @param source_text Text to display as source (default: NULL)
#' @param source_size Font size for source text (default: 8)
#' @param rotate_x_labels Logical, whether to rotate x-axis labels (default: TRUE)
#' @param label_size Size of value labels (default: 3)
#' @param bar_fill Fill color for bars (default: NULL, uses custom gradient)
#' @param bar_alpha Transparency of bars (default: 0.9)
#' @return A ggplot object
generate_bar_plot <- function(data, 
                              x_var,
                              y_var,
                              orientation = "vertical",
                              sort_bars = "descending",
                              x_lab = NULL,
                              y_lab = NULL,
                              title = NULL,
                              subtitle = NULL,
                              show_values = TRUE,
                              value_format = scales::comma,
                              source_text = NULL,
                              source_size = 8,
                              rotate_x_labels = TRUE,
                              label_size = 3,
                              bar_fill = NULL,
                              bar_alpha = 0.9) {
  
  # Validation of inputs 
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame")
  }
  
  orientation <- match.arg(orientation, c("vertical", "horizontal"))
  sort_bars <- match.arg(sort_bars, c("descending", "ascending", "none"))
  
  if (!x_var %in% names(data)) {
    stop(sprintf("Column '%s' not found in the data frame. Available columns are: %s", 
                 x_var, paste(names(data), collapse = ", ")))
  }
  
  if (!y_var %in% names(data)) {
    stop(sprintf("Column '%s' not found in the data frame. Available columns are: %s", 
                 y_var, paste(names(data), collapse = ", ")))
  }
  
  # Create a copy of the data
  plot_data <- data
  
  # Determine if x_var contains years
  is_year <- all(!is.na(suppressWarnings(as.numeric(as.character(unique(plot_data[[x_var]]))))))
  
  # Handle sorting
  if (sort_bars != "none" && !is_year) {
    sorted_data <- plot_data[order(plot_data[[y_var]], 
                                   decreasing = sort_bars == "descending"), ]
    plot_data[[x_var]] <- factor(plot_data[[x_var]], 
                                 levels = unique(sorted_data[[x_var]]))
  } else if (is_year) {
    plot_data[[x_var]] <- factor(plot_data[[x_var]], 
                                 levels = sort(unique(plot_data[[x_var]])))
  }
  
  # Set default labels
  if (is.null(x_lab)) x_lab <- x_var
  if (is.null(y_lab)) y_lab <- y_var
  
  # Set default title
  if (is.null(title)) {
    if (is_year) {
      if (length(unique(plot_data[[x_var]])) == 1) {
        title <- paste(y_var, "in", unique(plot_data[[x_var]]))
      } else {
        title <- paste(y_var, "Trend")
      }
    } else {
      title <- paste(y_var, "by", x_var)
    }
  }
  
  # Enhanced theme with better spacing and aesthetics
  enhanced_theme <- create_standard_theme() +
    theme(
      plot.margin = margin(t = 20, r = 60, b = 40, l = 20),
      panel.grid.major = element_line(color = "gray95", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "gray30", linewidth = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 16, face = "bold", margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 10)
    )
  
  # Create gradient colors if no fill specified
  if (is.null(bar_fill)) {
    n_bars <- length(unique(plot_data[[x_var]]))
    bar_fill <- colorRampPalette(c("#2C3E50", "#3498DB"))(n_bars)
  }
  
  if (orientation == "horizontal") {
    p <- ggplot(plot_data) +
      aes(y = .data[[x_var]], x = .data[[y_var]], fill = .data[[x_var]]) +
      geom_bar(stat = "identity", 
               width = 0.7,
               alpha = bar_alpha) +
      enhanced_theme +
      theme(
        axis.text.y = element_text(angle = 0, hjust = 1),
        panel.grid.major.y = element_blank(),
        legend.position = "none"
      )
    
    # Add value labels
    if (show_values) {
      p <- p + 
        geom_text(
          aes(label = value_format(.data[[y_var]])),
          hjust = -0.2,
          size = label_size,
          fontface = "bold"
        )
    }
    
    # Expand x-axis for labels
    p <- p + scale_x_continuous(
      labels = value_format,
      expand = expansion(mult = c(0.05, 0.15))
    )
    
  } else {
    p <- ggplot(plot_data) +
      aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[x_var]]) +
      geom_bar(stat = "identity", 
               width = 0.7,
               alpha = bar_alpha) +
      enhanced_theme +
      theme(legend.position = "none")
    
    # Add value labels
    if (show_values) {
      p <- p + 
        geom_text(
          aes(label = value_format(.data[[y_var]])),
          vjust = -0.5,
          size = label_size,
          fontface = "bold"
        )
    }
    
    # Expand y-axis for labels
    p <- p + scale_y_continuous(
      labels = value_format,
      expand = expansion(mult = c(0.05, 0.15))
    )
    
    if (rotate_x_labels && !is_year) {
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  }
  
  # Add gradient fill scale
  p <- p + scale_fill_manual(values = bar_fill)
  
  # Add labels and titles
  p <- p + labs(
    title = title,
    subtitle = subtitle,
    x = if(orientation == "horizontal") y_lab else x_lab,
    y = if(orientation == "horizontal") x_lab else y_lab
  )
  
  # Add source if provided
  if (!is.null(source_text)) {
    p <- p + 
      labs(caption = paste0("Source: ", source_text)) +
      theme(
        plot.caption = element_text(
          size = source_size,
          hjust = 0,
          margin = margin(t = 20),
          color = "gray30"
        )
      )
  }
  
  # Add subtle border shadow effect using annotation
  p <- p + 
    annotate("rect", 
             xmin = -Inf, xmax = Inf, 
             ymin = -Inf, ymax = Inf,
             color = "gray90",
             linewidth = 0.5,
             fill = NA)
  
  return(p)
}

#' Add group categories to data frame
#' @param data Dataframe containing the variable to be categorized
#' @param categories List of category definitions
#' @param var_col Name of the variable column to categorize
#' @param new_col Name for the new category column
#' @param warn_unmapped Logical, whether to warn about unmapped values (default: TRUE)
#' @return Dataframe with added category column
add_group_category <- function(data, 
                               categories,
                               var_col,
                               new_col,
                               warn_unmapped = TRUE) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!var_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in the data frame", var_col))
  }
  
  if (!is.list(categories)) {
    stop("Categories must be provided as a list")
  }
  
  if (length(categories) == 0) {
    stop("Categories list cannot be empty")
  }
  
  # Create a mapping vector for easier assignment
  group_mapping <- unlist(sapply(names(categories), function(group) {
    setNames(rep(group, length(categories[[group]])), categories[[group]])
  }))
  
  # Create a copy of the input data
  result <- data
  
  # Create the case_when expressions dynamically based on the categories
  case_expressions <- lapply(seq_along(categories), function(i) {
    quo(!!sym(var_col) %in% categories[[!!i]] ~ names(categories)[!!i])
  })
  
  # Add default case
  case_expressions <- c(case_expressions, quo(TRUE ~ NA_character_))
  
  # Add the new column using case_when for consistent group assignment
  result <- result %>%
    group_by(across(all_of(var_col))) %>%
    mutate(
      !!new_col := case_when(!!!case_expressions)
    ) %>%
    ungroup() %>%
    mutate(!!new_col := factor(!!sym(new_col), levels = names(categories)))
  
  # Check only for items in the categories list that weren't successfully mapped
  all_category_items <- unlist(categories)
  unmapped_items <- all_category_items[!all_category_items %in% unique(data[[var_col]])]
  
  if (warn_unmapped && length(unmapped_items) > 0) {
    warning(sprintf(
      "The following items from your categories were not found in the data:\n%s",
      paste(unmapped_items, collapse = "\n")
    ))
  } else {
    message("All categories successfully mapped")
  }
  
  return(result)
}