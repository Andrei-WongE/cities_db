# Request for MENA cities data

# Load packages and data, see Main file
require(here)

source(here("Master_variables.R"))

if(!exists("data")) stop("Data not found")

source("Plot_functions_V1.R")
source("plot_function.R")


# Create output folder
dir.create("Output/MENA", showWarnings = FALSE)

# Filter data for MENA region

mena_capitals <- c(
  "Algiers",         # Algeria
  "Manama",          # Bahrain
  "Cairo",           # Egypt
  "Tehran",          # Iran
  "Baghdad",         # Iraq
  "Jerusalem",       # Israel (Note: Status disputed, Tel Aviv hosts most embassies)
  "Amman",           # Jordan
  "Kuwait City",     # Kuwait
  "Beirut",          # Lebanon
  "Tripoli",         # Libya
  "Nouakchott",      # Mauritania
  "Rabat",           # Morocco
  "Muscat",          # Oman
  "Doha",            # Qatar
  "Riyadh",          # Saudi Arabia
  "Damascus",        # Syria
  "Tunis",           # Tunisia
  "Abu Dhabi",       # United Arab Emirates
  "Sana'a",          # Yemen (Note: De facto capital is currently Aden due to ongoing conflict)
  "Ramallah"         # Palestine (Note: Status disputed, de facto administrative capital)
)

mena_countries <- c(
  "Algeria",
  "Bahrain",
  "Egypt",
  "Iran",
  "Iraq",
  "Israel",
  "Jordan",
  "Kuwait",
  "Lebanon",
  "Libya",
  "Mauritania",
  "Morocco",
  "Oman",
  "Qatar",
  "Saudi Arabia",
  "Syria",
  "Tunisia",
  "UAE",
  "Yemen",
  "Palestine"
)


oe_mena <- data %>% dplyr::filter(Country %in% mena_countries)

oe_mena %>% filter(Country!=Location) %>%
  distinct(.$Location) %>% 
  View() %>% #51 (not considering national level)

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


## Frontier distance----
data_frontier <- data %>%
  mutate(GDPTOTUSC = as.numeric(GDPTOTUSC),
         POPTOTT = as.numeric(POPTOTT),
         GDP_per_capita = as.numeric(GDPTOTPPPC)) %>%
  filter(Year %in% c(2019)) %>%
  filter(Location != Country) %>% 
  dplyr::filter(!is.na(GDP_per_capita)) %>%
  mutate(Region = case_when(
    Country %in% mena_countries ~ "MENA",
    TRUE ~ "Not_MENA"
  ))

# Step 2: Identify frontier cities
# Create population percentiles and find max GDP_growth for each percentile
# Ensure non-missing GDP_growth and create deciles
# Manually define decile breaks to ensure uniqueness
unique_breaks <- unique(quantile(data_frontier$POPTOTT
                                 , probs = seq(0, 1, 0.01)
                                 , na.rm = TRUE))

# Ensure non-missing GDP_per_capita and create deciles
frontier_cities <- data_frontier %>%
  dplyr::filter(!is.na(GDP_per_capita)) %>%
  arrange(POPTOTT) %>%
  mutate(pop_percentile = cut(POPTOTT, 
                              breaks = c(unique_breaks[1], unique_breaks[-1] + 1e-7),
                              include.lowest = TRUE, labels = FALSE)) %>%
  group_by(pop_percentile) %>%
  slice_max(GDP_per_capita, n = 1) %>%
  ungroup()

View(frontier_cities)

# Step 3: Estimate frontier regression
frontier_model <- lm(log(GDP_per_capita) ~ log(POPTOTT), data = frontier_cities)

# Step 4: Calculate distances to frontier for all cities
# Create the labeled data
results <- data_frontier %>%
  mutate(
    # Predicted frontier GDP_per_capita for each city's population
    predicted_frontier = exp(predict(frontier_model, 
                                     newdata = data.frame(POPTOTT = POPTOTT))),
    # Distance to frontier as percentage difference
    frontier_distance = ((GDP_per_capita - predicted_frontier) / predicted_frontier) * 100
  )

# Step 5: Calculate distances specifically for MENA cities #and find top 50%# all
mena_closest_cities <- results %>%
  filter(Region== "MENA") %>%
  # Calculate distance to frontier (negative numbers mean below frontier)
  arrange(desc(frontier_distance)) %>%  # Sort from smallest gap to largest
  mutate(
    percentile_rank = ntile(frontier_distance, 100)  # Calculate percentile
  ) %>%
  filter(percentile_rank >= 15) %>%  # Select top 15
  dplyr::select(Location, POPTOTT, GDP_per_capita, predicted_frontier, frontier_distance) %>%
  arrange(desc(frontier_distance))

View(mena_closest_cities)

# Calculate if points are above or below frontier line
mena_closest_cities <- mena_closest_cities %>%
  mutate(
    above_line = log(GDP_per_capita) > predict(frontier_model,
                                               newdata = data.frame(POPTOTT = POPTOTT))
  )

(frontier_plot <- ggplot() +
    # Base cities (grey)
    geom_point(data = results %>% 
               filter(Region != "MENA"),
               aes(x = log(POPTOTT), y = log(GDP_per_capita)), 
               color = "grey", 
               alpha = 0.3) +
    # All MENA cities (light orange)
    geom_point(data = results %>% 
                 filter(Region == "MENA") %>%
                 filter(!Location %in% mena_closest_cities$Location),
               aes(x = log(POPTOTT), y = log(GDP_per_capita)), 
               color = "orange", 
               alpha = 0.3) +
    # Top 50% MENA cities (darker orange)
    geom_point(data = mena_closest_cities,
               aes(x = log(POPTOTT), y = log(GDP_per_capita)),
               color = "darkorange",
               size = 3,
               alpha = 0.7) +
    # Frontier cities (green)
    geom_point(data = frontier_cities,
               aes(x = log(POPTOTT), y = log(GDP_per_capita)),
               color = 'lightgreen', 
               size = 3, 
               alpha = 0.7) +
    # Labels for top 50% MENA cities with smart placement
    geom_text_repel(
      data = mena_closest_cities,
      aes(x = log(POPTOTT), 
          y = log(GDP_per_capita), 
          label = paste0(Location, "\n(", round(frontier_distance, 1), "%)"),
          vjust = ifelse(above_line, -0.2, 1.2)),  # Adjust vertical position based on point location
      size = 3,
      force = 10,
      box.padding = 0.8,
      point.padding = 0.3,
      max.overlaps = Inf,
      direction = "y",    # Keep vertical direction for better spacing
      segment.size = 0.3,
      segment.color = "grey50",
      segment.linetype = "dotted",
      min.segment.length = 0,
      nudge_y = ifelse(mena_closest_cities$above_line, 0.8, -0.8)  # Push labels up or down based on point position
    ) +
    # Frontier line
    stat_smooth(data = frontier_cities %>%
                  filter(log(POPTOTT) >= 6 & log(POPTOTT) <= 10),
                aes(x = log(POPTOTT), y = log(GDP_per_capita)),
                method = "lm", 
                color = "darkgreen", 
                se = FALSE) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      axis.text.x = element_text(face = "plain"),
      axis.text.y = element_text(face = "plain"),
      legend.position = "none",
      plot.margin = margin(t = 1, r = 2, b = 1, l = 1, unit = "cm")
    ) +
    scale_x_continuous(limits = c(6, 10), breaks = seq(6, 10, 1)) +
    scale_y_continuous(limits = c(6, 14), breaks = seq(6, 14, 2)) +
    coord_cartesian(clip = "off") +
    labs(x = "Log Population (2019)",
         y = "Log GDP per capita, real, PPP adjusted (2019)",
         title = "Economic Frontier Analysis",
         subtitle = "Green points represent frontier cities, dark orange points show top 15 MENA cities in dataset")
  
)

ggsave(filename = here::here("Output","MENA", "Frontier-Cities_2019_MENA.png"),
       plot = frontier_plot, width = 12, height = 10, dpi = 600)

