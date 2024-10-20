require(fs)

figures_path <- here("Figures")

# List all PNG files that end with FINAL
(final_pngs <- list.files(
  path = figures_path,
  pattern = "FINAL\\.png$",
  ignore.case = TRUE,
  full.names = FALSE
))

file_info <- file_info(final_pngs) %>%
  mutate(path = as.character(path)) %>%
  dplyr::select(path, modification_time) %>% 
  arrange(desc(modification_time))

# Display the table of Locations
countries_locations <- data_filtered_mena %>%
  filter(Location != Country) %>%  # This line excludes rows where Location == Country
  group_by(Country) %>%
  summarize(Locations = paste(unique(Location), collapse = ", ")) %>%
  arrange(Country)

kable(countries_locations, format = "markdown", col.names = c("Country", "Cities"))

# Search variable in dataset
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