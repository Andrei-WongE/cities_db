require(fs)

figures_path <- here("Figures")

# List all PNG files that end with FINAL-----
# (final_pngs <- list.files(
#   path = figures_path,
#   pattern = "FINAL\\.png$",
#   ignore.case = TRUE,
#   full.names = FALSE
# ))
# 
# file_info <- file_info(final_pngs) %>%
#   mutate(path = as.character(path)) %>%
#   dplyr::select(path, modification_time) %>% 
#   arrange(desc(modification_time))

# Display the table of Locations -----
# countries_locations <- data_filtered_mena %>%
#   filter(Location != Country) %>%  # This line excludes rows where Location == Country
#   group_by(Country) %>%
#   summarize(Locations = paste(unique(Location), collapse = ", ")) %>%
#   arrange(Country)
# 
# kable(countries_locations, format = "markdown", col.names = c("Country", "Cities"))

# Search variable in dataset-----
search_variable <- function(data, search_term, partial_match = FALSE, case_sensitive = FALSE) {
  # Check if the input is a dataframe or tibble
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    stop("Input must be a dataframe or tibble")
  }
  
  # Get column names
  col_names <- names(data)
  
  # Apply case sensitivity
  if (!case_sensitive) {
    search_term <- tolower(search_term)
    col_names <- tolower(col_names)
  }
  
  # Perform search
  if (partial_match) {
    matches <- grep(search_term, col_names, value = TRUE, ignore.case = !case_sensitive)
  } else {
    matches <- col_names[col_names == search_term]
  }
  
  # Prepare result
  result <- list(
    exists = length(matches) > 0,
    matches = matches,
    total_matches = length(matches)
  )
  
  # Print results
  cat("Search Results:\n")
  cat("Variable Exists:", result$exists, "\n")
  cat("Total Matches:", result$total_matches, "\n")
  if (result$total_matches > 0) {
    cat("Matching Variables:\n")
    print(result$matches)
  }
  
  # Return result invisibly for further use if needed
  invisible(result)
}

# Calculate moving average and percentage change-----
moving_average_change <- function(data, variable, years, calculate_pct_change = FALSE) {
  # Checks
  if (!require(dplyr)) install.packages("dplyr")
  library(dplyr)
  
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!variable %in% names(data)) {
    stop(paste("Variable", variable, "not found in the data frame"))
  }
  
  # Calculate the moving average
  result <- data %>%
    mutate(
      moving_avg = rollmean(!!sym(variable), k = years, fill = NA, align = "right")
    )
  
  # Calculate percentage change if requested
  if (calculate_pct_change) {
    result <- result %>%
      mutate(
        pct_change = (!!sym(variable) - moving_avg) / moving_avg * 100
      )
  }
  
  return(result)
}

# Analyse missing values-----
analyze_missing_values <- function(data, years = NULL, multi_year = FALSE, show_pct = TRUE) {
  require(naniar)
  require(dplyr)
  require(ggplot2)
  
  # Validate input
  if (!is.null(years) && !all(years %in% unique(data$Year))) {
    stop("Specified year(s) not found in the dataset")
  }
  
  # Filter years if specified and create secuence if specified, CHECK!
  if (!is.null(years)) {
    
    years <- if (length(years) == 2) seq(years[1], years[2]) else years
    data <- data[data$Year %in% years, ]
  }
  
  # Create missing patterns summary
  missing_patterns <- data %>%
    group_by(Year, Location) %>%
    miss_var_summary() %>%
    arrange(desc(n_miss)) %>% 
    View()
  
  # Create visualization
  if (multi_year) {
    
    # Multiple years plot
    plot <- gg_miss_var(data, facet = Year, show_pct = TRUE) +
      theme_minimal() +
      labs(title = paste("Missing Values by Variable for Years:", 
                         paste(years, collapse = ", "))) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, 100)  
    
  } else {
    
    # Single year plot
    plot <- gg_miss_var(data, show_pct = TRUE) +
      theme_minimal() +
      labs(title = paste("Missing Values by Variable", 
                         ifelse(!is.null(years), 
                                paste("for Years:", paste(years, collapse = ", ")), 
                                ""))) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, 100)  
  }
  
  # Return both the summary and plot
  return(list(
    missing_summary = missing_patterns,
    plot = plot
  ))
}

# Function to export dataset in excel-----
show_in_excel <- function(.data){
  require(writexl)
  tmp <- paste0(tempfile(), ".xlsx")
  # If .data is a list of data frames, write as separate sheets
  if(is.list(.data) && !is.data.frame(.data)) {
    writexl::write_xlsx(x = setNames(.data, paste0("Sheet", seq_along(.data))), path = tmp)
  } else {
    writexl::write_xlsx(x = .data, path = tmp)
  }
  browseURL(tmp)
}

# Function to find partial matches-----
find_partial_matches <- function(x, choices) {
  
  sapply(x, function(i) {
    # Check for NA or empty values first
    if (is.na(i) || i == "") {
      return(NA_character_)
    }
    
    
    matches <- stringr::str_detect(choices, regex(i, ignore_case = TRUE)) |
      stringr::str_detect(i, regex(choices, ignore_case = TRUE))
    
    if (any(matches, na.rm = TRUE)) {
      return(choices[matches][1])
    } else {
      return(NA_character_)
    }
    
  })
}

#  Function to remove all-NA columns and report dropped variables-----
remove_all_na_columns <- function(df) {
  # Store the original column names
  original_cols <- names(df)
  
  # Remove the all-NA columns
  df_cleaned <- df %>% dplyr::select(where(~ !all(is.na(.))))
  
  # Identify dropped columns
  dropped_vars <- setdiff(original_cols, names(df_cleaned))
  
  message("Dropped variables: ", paste(dropped_vars, collapse = ", "))
  
  # Return both the cleaned dataframe and info about dropped variables
  return(list(
    cleaned_data = df_cleaned,
    dropped_variables = dropped_vars
  ))
}

#  Read all layers or selected layers from gpkg file -----

read_gpkg_layers <- function(gpkg_path, selected_layers = NULL, quiet = TRUE) {
  
  # Get layers
  layers <- st_layers(gpkg_path)
  available_layers <- layers$name
  
  # If selected_layers is NULL, use all layers
  # If not NULL, handle both names and indices
  layers_to_read <- if (is.null(selected_layers)) {
    available_layers
  } else {
    # Convert numeric indices to layer names
    if (is.numeric(selected_layers)) {
      # Check if indices are valid
      if (any(selected_layers < 1) || any(selected_layers > length(available_layers))) {
        stop("Invalid layer index. Available indices: 1 to ", length(available_layers))
      }
      available_layers[selected_layers]
    } else {
      # Validate layer names
      if (!all(selected_layers %in% available_layers)) {
        invalid_layers <- selected_layers[!selected_layers %in% available_layers]
        stop("Invalid layer(s): ", paste(invalid_layers, collapse = ", "), 
             "\nAvailable layers: ", paste(available_layers, collapse = ", "))
      }
      selected_layers
    }
  }
  
  # Read layers
  result <- list()
  for(layer in layers_to_read) {
    if (!quiet) message("Reading layer: ", layer)
    result[[layer]] <- st_read(gpkg_path, layer = layer, quiet = quiet)
  }
  
  # If only one layer, return it directly
  if (length(result) == 1) {
    return(result[[1]])
  }
  
  return(result)
}
