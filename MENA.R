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
  dplyr::select(Location, POPTOTT, GDPTOTPPPC, GDP_per_capita, predicted_frontier, frontier_distance) %>%
  arrange(desc(frontier_distance))

View(mena_closest_cities)

# Calculate if points are above or below frontier line
mena_closest_cities <- mena_closest_cities %>%
  mutate(
    above_line = log(GDP_per_capita) > predict(frontier_model,
                                               newdata = data.frame(POPTOTT = POPTOTT))
  )

avg_frontier_distance <- mean(mena_closest_cities$frontier_distance, na.rm = TRUE)


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
    stat_smooth(data = frontier_cities,
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
    expand_limits(
      x = range(log(data_frontier$POPTOTT)),
      y = range(log(data_frontier$GDP_per_capita))
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
    coord_cartesian(clip = "off") +
    labs(x = "Log Population (2019)",
         y = "Log GDP per capita, real, PPP adjusted (2019)",
         title = paste0("Economic Frontier Analysis: MENA ", 
                        " (Avg. Distance: ", round(avg_frontier_distance, 1), "%)"),
         subtitle = "Green points represent frontier cities, dark orange points show top 15 MENA cities in dataset")
  
)

ggsave(filename = here::here("Output","MENA", "Frontier-Cities_2019_MENA.png"),
       plot = frontier_plot, width = 12, height = 10, dpi = 600)

# Other comparisons groups with frontier cities
# Define country groups properly
country_groups <- list(
  GCC = c(
    "Bahrain",
    "Kuwait",
    "Oman",
    "Qatar",
    "Saudi Arabia",
    "UAE"
  ),
  
  Maghreb = c(
    "Algeria",
    "Libya",
    "Mauritania",
    "Morocco",
    "Tunisia"
  ),
  
  Mashreq = c("Bahrain",
              "Egypt",
              "Iraq",
              "Jordan",
              "Kuwait",
              # "Lebanon",
              "Oman",
              "Palestine",
              "Qatar",
              "Saudi Arabia",
              "Sudan",
              "Syria",
              "UAE",
              "Yemen")
)

# Function to create frontier plot for a specific group
create_frontier_plot <- function(data_frontier, frontier_cities, frontier_model,
                                 country_group, group_name, group_color = "darkorange") {
  
  # Print for debugging
  print(paste("Processing", group_name))
  print(paste("Number of cities in group:", 
              nrow(data_frontier %>% filter(Country %in% country_group))))
  
  # Calculate frontier distances for the group cities
  group_cities <- data_frontier %>% 
    filter(Country %in% country_group) %>%
    filter(Location != Country) %>%  # Only cities, not countries
    mutate(
      predicted_frontier = exp(predict(frontier_model, 
                                       newdata = data.frame(POPTOTT = POPTOTT))),
      frontier_distance = ((GDP_per_capita - predicted_frontier) / predicted_frontier) * 100,
      above_line = log(GDP_per_capita) > predict(frontier_model,
                                                 newdata = data.frame(POPTOTT = POPTOTT))
    )
  
  # Calculate average frontier distance for the group
  avg_frontier_distance <- mean(group_cities$frontier_distance, na.rm = TRUE)
  
  # Create the plot
  ggplot() +
    # Base cities (grey)
    geom_point(data = data_frontier %>% 
                 filter(!Country %in% country_group),
               aes(x = log(POPTOTT), y = log(GDP_per_capita)), 
               color = "grey90", 
               alpha = 0.2,
               size = 1) +
    
    # Group's cities
    geom_point(data = group_cities,
               aes(x = log(POPTOTT), y = log(GDP_per_capita)), 
               color = group_color,
               size = 2) +
    
    # Frontier cities (green)
    geom_point(data = frontier_cities,
               aes(x = log(POPTOTT), y = log(GDP_per_capita)),
               color = '#90EE90', 
               size = 2.5,
               alpha = 0.8) +
    
    # Frontier line
    stat_smooth(data = frontier_cities,
                aes(x = log(POPTOTT), y = log(GDP_per_capita)),
                method = "lm", 
                color = "darkgreen", 
                se = FALSE,
                size = 0.5) +
    
    # Labels for group cities
    geom_text_repel(
      data = group_cities,
      aes(x = log(POPTOTT), 
          y = log(GDP_per_capita), 
          label = paste0(Location, "\n(", round(frontier_distance, 1), "%)"),
          vjust = ifelse(above_line, -0.2, 1.2)),
      size = 3,
      force = 10,
      box.padding = 0.8,
      point.padding = 0.3,
      max.overlaps = Inf,
      direction = "y",
      segment.size = 0.3,
      segment.color = "grey50",
      segment.linetype = "dotted",
      min.segment.length = 0,
      nudge_y = ifelse(group_cities$above_line, 0.8, -0.8)
    ) +
    
    # Formatting
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 10),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    ) +
    expand_limits(
      x = range(log(data_frontier$POPTOTT)),
      y = range(log(data_frontier$GDP_per_capita))
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
    coord_cartesian(clip = "off") +
    labs(x = "Log Population (2019)",
         y = "Log GDP per capita, real, PPP adjusted (2019)",
         title = paste0("Economic Frontier Analysis: ", group_name, 
                        " (Avg. Distance: ", round(avg_frontier_distance, 1), "%)"),
         subtitle = paste("Green points represent frontier cities,", 
                          tolower(group_name), "cities shown in",
                          case_when(
                            group_name == "GCC" ~ "orange",
                            group_name == "Maghreb" ~ "purple",
                            group_name == "Mashreq" ~ "blue"
                          )))
}
# Create the plots
plots <- list(
  GCC = create_frontier_plot(data_frontier, frontier_cities, frontier_model,
                             country_groups$GCC, "GCC", "#FFA500"),
  
  Maghreb = create_frontier_plot(data_frontier, frontier_cities, frontier_model,
                                 country_groups$Maghreb, "Maghreb", "#800080"),
  
  Mashreq = create_frontier_plot(data_frontier, frontier_cities, frontier_model,
                                 country_groups$Mashreq, "Mashreq", "#0000FF")
)

# Save all plots
walk2(
  names(plots),
  plots,
  ~ggsave(
    filename = here::here("Output", "MENA", sprintf("Frontier-Cities_2019_%s.png", .x)),
    plot = .y,
    width = 12,
    height = 10,
    dpi = 600
  )
)

# General comparison charts -----
oe_mena <- oe_mena %>% 
  mutate(POPTOTT = as.numeric(POPTOTT)) %>% 
  mutate(GDPTOTUSC = as.numeric(GDPTOTUSC)) %>% 
  mutate(GDP_per_capita_PPP = GDPTOTPPPC / POPTOTT) %>%
  filter(Location != Country)

## Population

p01 <- generate_bar_plot(subset(oe_mena, Year == 2019), 
                         x_var = "Location",
                         y_var = "POPTOTT",
                         x_lab = NULL,
                         y_lab = "Population (millions)",
                         orientation = "vertical",
                         sort_bars = "descending",
                         title = "Population of selected MENA cities, 2019",
                         subtitle = "(millions)",
                         show_values = TRUE,
                         value_format = scales::label_number(
                           unit = "m", 
                           scale = 1e-3,
                           accuracy = 0.1),
                         source_text = "Oxford City Database, 2022",
                         source_size = 10,
                         rotate_x_labels = TRUE) 

ggsave(filename = here::here("Output","MENA", "Total_Population_2019.png"),
       plot = p01, width = 12, height = 15, dpi = 600)

## GDP
p02 <- generate_bar_plot(subset(oe_mena, Year == 2019), 
                         x_var = "Location",
                         y_var = "GDPTOTUSC",
                         x_lab = NULL,
                         y_lab = "GDP (millions)",
                         orientation = "vertical",
                         sort_bars = "descending",
                         title = "GDP of selected MENA cities, 2019",
                         subtitle = "Real, PPP adjusted (millions)",
                         show_values = TRUE,
                         value_format = scales::label_number(
                           unit = "m", 
                           scale = 1e-3,
                           accuracy = 0.1),
                         source_text = "Oxford City Database, 2022",
                         source_size = 10,
                         rotate_x_labels = TRUE) 

ggsave(filename = here::here("Output","MENA", "Total_GDP_2019.png"),
       plot = p02, width = 12, height = 15, dpi = 600)

## GDP per capita
p03 <- generate_bar_plot(subset(oe_mena, Year == 2019), 
                         x_var = "Location",
                         y_var = "GDP_per_capita_PPP",
                         x_lab = NULL,
                         y_lab = "GDP per capita (thousands)",
                         orientation = "vertical",
                         sort_bars = "descending",
                         title = "GDP per capita of selected MENA cities, 2019",
                         subtitle = "Real, PPP adjusted (thousands)",
                         show_values = TRUE,
                         value_format = scales::label_number(
                           unit = "th", 
                           scale = 1,
                           accuracy = 0.1),
                         source_text = "Oxford City Database, 2022",
                         source_size = 10,
                         rotate_x_labels = TRUE) 

ggsave(filename = here::here("Output","MENA", "GDPpc_2019.png"),
       plot = p03, width = 12, height = 15, dpi = 600)


## GDP growth
growth_rates <- oe_mena %>% 
  dplyr::filter(Year %in% c(2001, 2019)) %>%
  group_by(Location) %>%
  mutate(GDP_growth = if_else(Year == 2019,
                              (GDPTOTUSC[Year == 2019] - GDPTOTUSC[Year == 2001]) / GDPTOTUSC[Year == 2001],
                              NA_real_)) %>%  # Only created in Location[Year == 2019]
  dplyr::filter(Year == c(2001, 2019)) %>%
  dplyr::select(Location, Year, GDP_growth)
  # # 7 out of 51 cities have no data for 2001
  # filter(Year == 2001 & is.na(GDPTOTUSC)) %>%
  # View()

# Find in which Year GDPTOTUSC has the least NA, before 2019
# oe_mena %>%
#   filter(Year < 2019) %>%
#   group_by(Year) %>%
#   summarize(
#     na_count = sum(is.na(GDPTOTUSC)),
#     total_rows = n(),
#     percent_complete = (1 - na_count/total_rows) * 100
#   ) %>%
#   arrange(na_count) %>%
#   View() # 2001, changing this in growth_rates calculations

oe_mena <- oe_mena %>%
  left_join(growth_rates, by = c("Location", "Year"))

p04 <- generate_bar_plot(subset(oe_mena, Year == 2019) %>% 
                           dplyr::filter(!is.na(GDP_growth)), 
                         x_var = "Location",
                         y_var = "GDP_growth",
                         x_lab = NULL,
                         y_lab = "GDP growth rate",
                         orientation = "vertical",
                         sort_bars = "descending",
                         title = "GDP growth rate by selected MENA cities, 2001-2019",
                         subtitle = "Percentage change between 2001-2019",
                         show_values = TRUE,
                         value_format = scales::label_number(
                           unit = "%", 
                           scale = 1,
                           accuracy = 0.1,
                           decimal.mark = "."),
                         source_text = "Oxford City Database, 2022",
                         source_size = 10,
                         rotate_x_labels = TRUE) 

ggsave(filename = here::here("Output","MENA", "GDP_growth_2001-2019.png"),
       plot = p04, width = 12, height = 15, dpi = 600)


