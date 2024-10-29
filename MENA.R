# MENA
mismatches <- oe_mena %>%
  filter(Country != matched_country) %>%
  dplyr::select(Country, matched_country) %>%
  distinct() %>% 
  print()

oe_mena %>% filter(Country != Location) %>% pull(Location) %>% 
  unique()

oe_mena %>%
  filter(Country == Location) %>%
  distinct(Location, GDPTOTUSC) %>%
  mutate(gdp_quintile = ntile(GDPTOTUSC, 5))


oe_mena %>%
  filter(Country == Location) %>%
  distinct(Location, Year, GDPTOTUSC) %>%
  group_by(Year) %>%
  mutate(
    GDPTOTUSC = as.numeric(GDPTOTUSC),
    gdp_quintile = ntile(GDPTOTUSC, 5)
  ) %>%
  group_by(Year, gdp_quintile) %>%
  mutate(avg_gdp_quintile = mean(GDPTOTUSC, na.rm = TRUE)) %>%
  dplyr::select(Year, Location, gdp_quintile, avg_gdp_quintile) %>%
  arrange(Year, gdp_quintile) %>% 
  View()

#Function to create quintile year tables
create_gdp_quintiles <- function(data, selected_year, export_csv = FALSE) {
  table_output <- data %>%
    filter(Country != Location, Year == selected_year) %>%
    distinct(Location, GDPTOTUSC) %>%
    mutate(
      GDPTOTUSC = as.numeric(GDPTOTUSC),
      gdp_quintile = ntile(GDPTOTUSC, 5)
    ) %>%
    group_by(gdp_quintile) %>%
    mutate(
      avg_gdp_quintile = mean(GDPTOTUSC, na.rm = TRUE),
      # Format GDP with commas
      avg_gdp_quintile = format(round(avg_gdp_quintile, 2), big.mark = ",")
    ) %>%
    dplyr::select(Location, gdp_quintile, avg_gdp_quintile) %>%
    arrange(gdp_quintile)
  
  # Create markdown title
  cat(paste("\n### GDP Quintiles for", selected_year, "\n\n"))
  
  # Print table in markdown format
  print(knitr::kable(table_output, 
                     format = "markdown",
                     col.names = c("Location", "Quintile", "Average GDP (thousands)"),
                     align = c('l', 'c', 'r'))) # Left, center, right alignment
  
  # Export to CSV if option is selected
  if(export_csv) {
    filename <- paste0("gdp_quintiles_", selected_year, ".csv")
    write.csv(table_output, filename, row.names = FALSE)
    cat(paste("\nCSV file exported:", filename, "\n"))
  }
  
  return(invisible(table_output))
}

# For multiple years
years <- c(2017, 2018)
tables <- lapply(years, function(year) create_gdp_quintiles(oe_mena, year))

## Plot

# # Create scatterplot with log scales
# oe_mena %>%
#   filter(Country != Location) %>%
#   filter(Year == 2018) %>%
#   ggplot(aes(x = as.numeric(POPTOTT), y = as.numeric(GDPTOTUSC))) +
#   geom_point(aes(color = factor(Year))) +
#   scale_x_log10(labels = scales::comma) +
#   scale_y_log10(labels = scales::comma) +
#   labs(
#     title = "City Population vs GDP (Log Scales) 2018",
#     x = "Population (log scale)",
#     y = "GDP in thousands (log scale)",
#     color = "Year"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     plot.title = element_text(hjust = 0.5)
#   ) +
#  geom_text_repel(aes(label = Location), size = 3)

# First create dataframe with quintile information
cities_with_quintiles <- oe_mena %>%
  filter(Country == Location) %>%
  distinct(Location, Year, GDPTOTUSC) %>%
  group_by(Year) %>%
  mutate(
    gdp_quintile = ntile(as.numeric(GDPTOTUSC), 5)
  )

# Create scatterplot with updated labels
oe_mena %>%
  filter(Country != "") %>%  # Ensure no blank entries
  mutate(
    city_type = ifelse(Country == Location, "Frontier City", "Other City")
  ) %>%
  ggplot(aes(x = as.numeric(POPTOTT), y = as.numeric(GDPTOTUSC))) +
  geom_point(aes(color = city_type)) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("Frontier City" = "darkgreen", "Other City" = "grey")) +
  facet_wrap(~Year) +
  labs(
    title = "City Population vs GDP (Log Scales)",
    x = "Population (log scale)",
    y = "GDP in thousands (log scale)",
    color = "City Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )
