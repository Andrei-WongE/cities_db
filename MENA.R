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

# OE DATA
oe_mena <- data %>% dplyr::filter(Country %in% mena_countries)

oe_mena %>% filter(Country!=Location) %>%
  distinct(.$Location) %>% 
  View()  #51 (not considering national level)

# NTL DATA
require(haven)
ntl_data <- read_dta(here("Data","NTL","OE_OECD_FUA_ntl_2019_matched.dta"))  

ntl_data_merge <- ntl_data %>% 
                  # dplyr::filter(mena == 1) %>% 
                  dplyr::select(efua_name, mask_2019_fua, mena) %>% 
                  rename(Location = efua_name)

# Correlate OE GDP and NTL
require(ggpmisc)
oe_ntl_data <-  data %>% dplyr::filter(Country!=Location, Year == 2019) %>% 
                          dplyr::select(Country, Location, GDPTOTUSC, POPTOTT, GDPTOTPPPC) %>%
                          left_join(ntl_data_merge, by = "Location") %>% 
                          dplyr::filter(!is.na(mask_2019_fua)) %>% 
                          mutate(GDPTOTUSC = as.numeric(GDPTOTUSC)) 

filtered_data <- oe_ntl_data %>% dplyr::filter(mena == 1)

model <- lm(GDPTOTUSC ~ mask_2019_fua, data = filtered_data)

filtered_data_corr <- filtered_data %>%
                 dplyr::filter(mena == 1) %>% 
                 mutate(residual = GDPTOTUSC - predict(model))

top_3_above <- filtered_data_corr %>%
  arrange(desc(residual)) %>%
  slice(1:3)

(corr_data <- ggplot(filtered_data_corr, aes(x = mask_2019_fua, y = GDPTOTUSC)) +
             geom_point() +
             geom_smooth(method = "lm", se = FALSE) +
             geom_text(data = top_3_above, aes(label = Location) 
                       , color = "red"
                       , vjust = -1) +  # Max value label
             ggpmisc::stat_poly_eq(aes(label = paste(..eq.label..
                                                     , ..rr.label..
                                                     , sep = "~~~")), 
                            formula = y ~ x, 
                            parse = TRUE) +
             labs(title = "OE GDP and NTL correlation",
                  x = "OE GDP",
                  y = "NTL") +
             theme_minimal()
)

(correlation <- cor(corr_data$data$GDPTOTUSC, corr_data$data$mask_2019_fua, use = "complete.obs"))

filter_corr_data <- filtered_data_corr %>% 
                    filter(!Location %in% top_3_above$Location)

(correlation <- cor(filter_corr_data$GDPTOTUSC, filter_corr_data$mask_2019_fua, use = "complete.obs"))


# mismatches <- oe_mena %>%
#   filter(Country != matched_country) %>%
#   dplyr::select(Country, matched_country) %>%
#   distinct() %>% 
#   print()

oe_mena %>% filter(Country != Location) %>% pull(Location) %>% 
  unique()

View(labels_vector)

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
  filter(Country != Location) %>%
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
  mutate(GDPTOTUSC = as.numeric(GDPTOTUSC)  * 1e3, # Convert to thousands
         POPTOTT = as.numeric(POPTOTT),  
         GDP = as.numeric(GDPTOTPPPC)) %>%
  filter(Year %in% c(2019)) %>%
  filter(Location != Country) %>% 
  dplyr::filter(!is.na(GDP)) %>%
  mutate(Region = case_when(
    Country %in% mena_countries ~ "MENA",
    TRUE ~ "Not_MENA"
  ))

# Using NTL data, run first line 56 to 85, obtaining oe_ntl_data

data_frontier <- oe_ntl_data %>%
  mutate(GDPTOTUSC = as.numeric(GDPTOTUSC)  * 1e3, # Convert to thousands
         POPTOTT = as.numeric(POPTOTT),  
         GDP = as.numeric(mask_2019_fua)) %>% # Using NTL data!!!!!!!
  # filter(Year %in% c(2019)) %>%
  filter(Location != Country) %>% 
  dplyr::filter(!is.na(GDP)) %>%
  mutate(Region = case_when(
    Country %in% mena_countries ~ "MENA",
    TRUE ~ "Not_MENA"
  ))

# Step 2: Identify frontier cities
# Create population percentiles and find max GDP for each percentile
# Ensure non-missing GDP and create deciles
# Manually define decile breaks to ensure uniqueness
unique_breaks <- unique(quantile(data_frontier$POPTOTT
                                 , probs = seq(0, 1, 0.01)
                                 , na.rm = TRUE))

# Ensure non-missing GDP and create deciles
frontier_cities <- data_frontier %>%
  dplyr::filter(!is.na(GDP)) %>%
  arrange(POPTOTT) %>%
  mutate(pop_percentile = cut(POPTOTT, 
                              breaks = c(unique_breaks[1], unique_breaks[-1] + 1e-7),
                              include.lowest = TRUE, labels = FALSE)) %>%
  group_by(pop_percentile) %>%
  slice_max(GDP, n = 1) %>%
  ungroup()

View(frontier_cities)

# Step 3: Estimate frontier regression
frontier_model <- lm(log(GDP) ~ log(POPTOTT), data = frontier_cities)

# Step 4: Calculate distances to frontier for all cities
results <- data_frontier %>%
  mutate(
    # Predicted frontier GDP for each city's population
    predicted_frontier = predict(frontier_model, newdata = data.frame(POPTOTT = POPTOTT)),
    # Distance to frontier as percentage difference
    frontier_distance = ((predicted_frontier - log(GDP)) / predicted_frontier) * 100
    )

# Step 5: Calculate distances specifically for MENA cities and find top 10% all
# unique_deciles <- unique(quantile(data_frontier$POPTOTT
#                                  , probs = seq(0, 1, 0.1)
#                                  , na.rm = TRUE))
# 
# mena_closest_cities <- results %>%
#   filter(Region== "MENA") %>%
#   dplyr::filter(!is.na(GDP)) %>%
#   arrange(POPTOTT) %>%
#   mutate(pop_percentile = cut(POPTOTT, 
#                               breaks = c(unique_deciles[1], unique_deciles[-1] + 1e-7),
#                               include.lowest = TRUE, labels = FALSE)) %>%
#   group_by(pop_percentile) %>%
#   slice_max(GDP, n = 1) %>%
#   ungroup() 
# 
# # mena_closest_cities <- mena_closest_cities %>%
# #   # Calculate distance to frontier (negative numbers mean below frontier)
# #   arrange(desc(frontier_distance)) %>%  # Sort from smallest gap to largest
# #   # mutate(
# #   #   percentile_rank = ntile(frontier_distance, 100)  # Calculate percentile
# #   # ) %>%
# #   # filter(percentile_rank >= 10) %>%  # Select top 10
# #   dplyr::select(Location, POPTOTT, GDPTOTPPPC, GDP, predicted_frontier, frontier_distance) %>%
# #   arrange(desc(frontier_distance))
# 
# View(mena_closest_cities)

# Calculate if points are above or below frontier line
# mena_closest_cities <- mena_closest_cities %>%
#   filter(Region== "MENA") %>%
#   mutate(
#     above_line = log(GDP) > predict(frontier_model,
#                                                newdata = data.frame(POPTOTT = POPTOTT))
#   )
# 
# avg_frontier_distance <- mean(mena_closest_cities$frontier_distance, na.rm = TRUE)

# closest_cities <- results %>%
#   # filter(Region== "MENA") %>%
#   mutate(
#     above_line = resid(frontier_model) > 0
#   )

# Step 6: Estimate frontier regression for frontier cities
frontier_model2 <- lm(log(GDP) ~ log(POPTOTT), data = frontier_cities)

results2 <- results %>%
  mutate(
    predicted_gdp_frontier2 = predict(frontier_model2, newdata = results),
    residuals_frontier2 = log(GDP) - log(predicted_gdp_frontier2),
    frontier_distance2 = ((predicted_gdp_frontier2 - log(GDP)) /
                            predicted_gdp_frontier2 * 100)
  )

# Step 7: Detect if MENA cities are above the frontier_model2 line
closest_cities <- results2 %>%
  filter(Region == "MENA") %>%
  mutate(
    above_line = residuals_frontier2 < 0
  )

# Average distance to frontier of ALL cities
avg_frontier_distance <- mean(results2$frontier_distance2, na.rm = TRUE) 
avg_frontier_distance_MENA <- mean(results2$frontier_distance2[results2$Region == "MENA"], na.rm = TRUE) 


(frontier_plot <- ggplot() +
  # Non-MENA, non-frontier cities
  geom_point(data = results2 %>% 
               filter(Region != "MENA", !Location %in% frontier_cities$Location),
             aes(x = log(POPTOTT), y = log(GDP), color = "Non-MENA non-frontier"),
             size = 2, alpha = 0.8) + 
  # Non-MENA frontier cities
  geom_point(data = frontier_cities %>% filter(Region != "MENA"),
             aes(x = log(POPTOTT), y = log(GDP), color = "Non-MENA frontier"), 
             size = 3, alpha = 0.7) +
  # MENA frontier cities
  geom_point(data = frontier_cities %>% filter(Region == "MENA"),
             aes(x = log(POPTOTT), y = log(GDP), color = "MENA frontier"),
             shape = 21, size = 4, alpha = 0.7) +
  # MENA cities above frontier but NOT frontier cities
  geom_point(data = closest_cities %>% 
               filter(Region == "MENA", above_line == TRUE, 
                      !Location %in% frontier_cities$Location),
             aes(x = log(POPTOTT), y = log(GDP), color = "MENA above frontier"),
             size = 4, alpha = 0.7) +
  # Other MENA cities
  geom_point(data = closest_cities %>% 
               filter(Region == "MENA", above_line == FALSE, 
                      !Location %in% frontier_cities$Location),
             aes(x = log(POPTOTT), y = log(GDP), color = "Other MENA"),
             size = 4, alpha = 0.9) +

  # Labels for MENA frontier cities
  geom_text_repel(
    data = results2 %>% 
      filter(Location %in% (frontier_cities %>% 
                             filter(Region == "MENA") %>% 
                             pull(Location))),
    aes(x = log(POPTOTT), 
        y = log(GDP), 
        label = paste0(Location, "\n(", round(frontier_distance2, 1), "%)")),
    size = 4, force = 10, box.padding = 0.8,
    point.padding = 0.3, max.overlaps = Inf,
    direction = "y", segment.size = 0.3,
    segment.color = "grey50", segment.linetype = "dotted",
    min.segment.length = 0, nudge_y = 0.8
  ) +
  # Labels for MENA cities above frontier but not frontier cities
  geom_text_repel(
    data = closest_cities %>% 
      filter(Region == "MENA", above_line == TRUE, 
             !Location %in% frontier_cities$Location),
    aes(x = log(POPTOTT), 
        y = log(GDP), 
        label = paste0(Location, "\n(", round(frontier_distance2, 1), "%)")),
    size = 4, force = 10, box.padding = 0.8,
    point.padding = 0.3, max.overlaps = Inf,
    direction = "y", segment.size = 0.3,
    segment.color = "grey50", segment.linetype = "dotted",
    min.segment.length = 0, nudge_y = -0.8
  ) +
  # Frontier line
  # stat_smooth(data = data_frontier, 
  #            aes(x = log(POPTOTT), y = log(GDP)),
  #            method = "lm", color = "darkgreen", se = FALSE) +
  geom_smooth(data = frontier_cities, 
              aes(x = log(POPTOTT), y = log(GDP)),
              method = "lm", color = "darkgreen", se = FALSE) +
  # Scales and labels
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_cartesian(clip = "off") +
  labs(x = "Log Population (2019)",
       # y = "Log Total GDP, real, PPP adjusted (2019)",
       y = "Log Total NTL",
       title = paste0("Economic Frontier Analysis: MENA ",
                     "(Avg. Distance: ", round(avg_frontier_distance, 1), "%)"),
       subtitle = "Frontier MENA cities and high performing MENA cities with their distance to frontier line") +
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(fill = "white", color = "gray80"),
    legend.margin = margin(5, 5, 5, 5),
    plot.margin = margin(1, 2, 1, 1, unit = "cm"),
    axis.line = element_line(color = "black")
  ) +
  # Color scale
  scale_color_manual(
    values = c(
      "Non-MENA frontier" = "#117a65",
      "MENA frontier" = "#abebc6",
      "MENA above frontier" = "#FFA07A",
      "Other MENA" = "#a04000",
      "Non-MENA non-frontier" = "grey90"
    ),
    name = NULL
  )
)

ggsave(filename = here::here("Output","MENA", "Frontier-Cities_2019_MENA.png"),
       plot = frontier_plot, width = 12, height = 10, dpi = 800)

ggsave(filename = here::here("Output","MENA", "Frontier-Cities_2019_MENA_NTL.png"),
       plot = frontier_plot, width = 12, height = 10, dpi = 800)


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
                                 country_groups, group_name, group_color = "darkorange") {
  
  # Print for debugging
  print(paste("Processing", group_name))
  print(paste("Number of cities in group:", 
              nrow(data_frontier %>% filter(Country %in% country_groups))))
  
  # Calculate if cities are above the frontier line
  group_cities <- results %>%
    mutate(
      above_line = resid(frontier_model) > 0  
    )
  # Calculate average frontier distance for the group
  avg_frontier_distance_country_group <- mean(group_cities$frontier_distance[group_cities$Country %in% country_groups], na.rm = TRUE)
  count_country_group <- nrow(data_frontier %>% filter(Country %in% country_groups))
  
  # Create the plot
  ggplot() +
    # Base cities (grey)
    geom_point(data = data_frontier %>% 
                 filter(!Country %in% country_groups) %>% 
                 filter(!Location %in% frontier_cities$Location),
               aes(x = log(POPTOTT), y = log(GDP)), 
               color = "grey90", 
               alpha = 0.8,
               size = 2) +
   
     # Frontier cities (green)
    geom_point(data = frontier_cities,
               aes(x = log(POPTOTT), y = log(GDP)),
               color = '#90EE90', 
               size = 4,
               alpha = 0.9) +  
    
    # Group's cities
    geom_point(data = data_frontier %>% filter(Country %in% country_groups),
               aes(x = log(POPTOTT), y = log(GDP)), 
               color = group_color,
               size = 2,
               alpha = 0.7) +
    
    # Frontier line
    stat_smooth(data = data_frontier %>% filter(Location %in% frontier_cities$Location),
                aes(x = log(POPTOTT), y = log(GDP)),
                method = "lm", 
                color = "darkgreen", 
                se = FALSE,
                size = 0.5) +
    
    # Labels for group cities
    geom_text_repel(
      data = group_cities %>% filter(Country %in% country_groups),
      aes(x = log(POPTOTT), 
          y = log(GDP), 
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
      y = range(log(data_frontier$GDP))
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
    coord_cartesian(clip = "off") +
    labs(x = "Log Population (2019)",
         y = "Log Total GDP, real, PPP adjusted (2019)",
         title = paste0("Economic Frontier Analysis: ", group_name, 
                        " (Avg. Distance: ", round(avg_frontier_distance_country_group, 1), "%, N=", count_country_group, ")"),
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
  mutate(GDP_per_capita_PPP = GDPTOTPPPC) %>%
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

# Leading cities and comparators plots------
# Get frontier cities in MENA
frontier_cities_MENA <- results2 %>% # Changed from mena_closest_cities
  arrange(desc(frontier_distance)) %>%  
  slice_head(n = 15) %>%               # Take the top 15 cities
  pull("Location")                     

# Get other not in frontier cities
other_MENA_cities <- oe_mena %>% 
  mutate(Region = case_when(
    Country %in% mena_countries ~ "MENA",
    TRUE ~ "Not_MENA"
  )) %>% 
  filter(Region == "MENA" & Year == 2019) %>% 
  filter(!(Location %in% frontier_cities_MENA)) %>%
  pull(Location)

cities_list_MENA <- list(
  Frontier_cities = frontier_cities_MENA,
  Other_cities = other_MENA_cities
)

oe_comparators2 <- oe_mena

oe_comparators2 <-
  add_group_category(oe_comparators2, 
                     categories = cities_list_MENA,
                     var_col = "Location",
                     new_col = "cities_list_MENA",
                     warn_unmapped = TRUE)%>% 
  dplyr::filter(cities_list_MENA != " ")

## Population
create_population_plot(subset(oe_comparators2, Year == 2019),
                       location_var = "Location",
                       category_var = "cities_list_MENA", 
                       year_var = "Year", 
                       variable_name = "POPTOTT",
                       value_format = scales::label_number(
                         scale = 1,
                         accuracy = 1,
                         big.mark = ",",
                         decimal.mark = ".",
                         suffix = "k"),
                       title = "Total Population by Frontier Cities and Rest of MENA Cities Cities, 2019",
                       subtitle = "Total population in thousands",
                       source_text = "Oxford City Database, 2022",  
                       source_size = 8,     
                       x_label = NULL,
                       y_label = "Total Population (thousands)",
                       category_order = "as_is",
                       within_group_order = "as_is",
                       palette = "Zissou1",
                       line_size = 1.2,
                       label_size = 3,
                       title_size = 16,  
                       save_plot = TRUE,
                       filename = "Total_Population_Frontier-Cities_Rest-of-MENA-Cities_2019.png",
                       width = 10,
                       height = 8
)
## GDP
create_population_plot(subset(oe_comparators2, Year == 2019),
                       location_var = "Location",
                       category_var = "cities_list_MENA", 
                       year_var = "Year", 
                       variable_name = "GDPTOTUSC",
                       value_format = scales::label_number(
                         scale = 1,
                         accuracy = NULL,
                         big.mark = ",",
                         decimal.mark = "."),
                       title = "GDP by Frontier Cities and Rest of MENA Cities Cities, 2019",
                       subtitle = NULL,
                       source_text = "Oxford City Database, 2022",  
                       source_size = 8,    
                       x_label = NULL,
                       y_label = "Real, PPP adjusted (millions)",
                       category_order = "as_is",
                       within_group_order = "as_is",
                       palette = "Zissou1",
                       line_size = 1.2,
                       label_size = 3,
                       title_size = 16,  
                       save_plot = TRUE,
                       filename = "Total_GDP_Frontier-Cities_Rest-of-MENA-Cities_2019.png",
                       width = 10,
                       height = 8
)

## GDP per capita
create_population_plot(subset(oe_comparators2, Year == 2019),
                       location_var = "Location",
                       category_var = "cities_list_MENA", 
                       year_var = "Year", 
                       variable_name = "GDP_per_capita_PPP",
                       value_format = scales::label_number(
                         scale = 1,
                         accuracy = 0.1,
                         big.mark = ",",
                         decimal.mark = "."),
                       title = "GDP per capita by Frontier Cities and Rest of MENA Cities Cities, 2019",
                       subtitle = NULL,
                       source_text = "Oxford City Database, 2022",  
                       source_size = 8,    
                       x_label = NULL,
                       y_label = "Real, PPP adjusted (thousands)",
                       category_order = "as_is",
                       within_group_order = "as_is",
                       palette = "Zissou1",
                       line_size = 1.2,
                       label_size = 3,
                       title_size = 16,  
                       save_plot = TRUE,
                       filename = "GDP_per_capita_Frontier-Cities_Rest-of-MENA-Cities_2019.png",
                       width = 10,
                       height = 8
)

## GDP growth
growth_rates_comparators2 <- oe_comparators2 %>% 
  dplyr::filter(Year %in% c(2001, 2019)) %>%
  group_by(Location) %>%
  mutate(GDP_growth = if_else(Year == 2019,
                              (GDPTOTUSC[Year == 2019]/GDPTOTUSC[Year == 2001])^(1/18) - 1,  # CAGR formula
                              NA_real_)) %>%
  dplyr::filter(Year == c(2001, 2019)) %>%
  dplyr::select(Location, Year, GDP_growth)

# Find in which Year GDPTOTUSC has the least NA, before 2019
# oe_comparators2 %>%
#   filter(Year < 2019) %>%
#   filter(cities_list_MENA != " ") %>%
#   group_by(Year) %>%
#   summarize(
#     na_count = sum(is.na(GDPTOTUSC)),
#     total_rows = n(),
#     percent_complete = (1 - na_count/total_rows) * 100
#   ) %>%
#   arrange(na_count) %>%
#   View() # 2001, only 1 missing, changing this in growth_rates calculation

# oe_comparators2 %>%
#   filter(cities_list_MENA != " ") %>%
#   summarise(total_rows = n(),
#             missing_gdp = sum(is.na(GDP_growth)),
#             pct_missing = round(sum(is.na(GDP_growth))/n()*100, 1)) %>%
#   glimpse()

oe_comparators2 <- oe_comparators2 %>%
  left_join(growth_rates_comparators2, by = c("Location", "Year"))

create_population_plot(subset(oe_comparators2, Year == 2019),
                       location_var = "Location",
                       category_var = "cities_list_MENA", 
                       year_var = "Year", 
                       variable_name = "GDP_growth",
                       value_format = scales::label_number(
                         unit = "%", 
                         scale = 100,
                         accuracy = 0.01,
                         decimal.mark = ".",
                         suffix = "%"),
                       title = "GDP growth rate by Frontier Cities and Rest of MENA Cities Cities, 2001-2019",
                       subtitle = "Compound Average Growth Rate (CAGR)",
                       source_text = "Oxford City Database, 2022",  
                       source_size = 8,    
                       x_label = NULL,
                       y_label = "Percentage change between 2001-2019",
                       category_order = "as_is",
                       within_group_order = "as_is",
                       palette = "Zissou1",
                       line_size = 1.2,
                       label_size = 3,
                       title_size = 16,  
                       save_plot = TRUE,
                       filename = "GDP_growth_Frontier-Cities_Rest-of-MENA-Cities_2001-2019.png",
                       width = 10,
                       height = 8
)

## Structure of GVA
oe_comparators2 <- oe_comparators2 %>%
  mutate(GVATOTPPPC = as.numeric(GVATOTPPPC),
         GVAGIR_UPPPC = as.numeric(GVAGIR_UPPPC),
         GVAAPPPC = as.numeric(GVAAPPPC),
         GVAK_NPPPC = as.numeric(GVAK_NPPPC),
         GVAB_FPPPC = as.numeric(GVAB_FPPPC),
         GVAO_QPPPC = as.numeric(GVAO_QPPPC),
         GVAHJPPPC = as.numeric(GVAHJPPPC),
  ) %>%
  mutate(Agriculture_GVA_Pct = GVAAPPPC / GVATOTPPPC
         , Consumer_Services_GVA_Pct = GVAGIR_UPPPC / GVATOTPPPC
         , Financial_Business_Services_GVA_Pct = GVAK_NPPPC / GVATOTPPPC
         , Industry_GVA_Pct = GVAB_FPPPC / GVATOTPPPC          
         , Public_Services_GVA_Pct =  GVAO_QPPPC / GVATOTPPPC 
         , Transport_Information_Communic_Services_GVA_Pct =  GVAHJPPPC / GVATOTPPPC
  ) # Decimal format

# Find in which Year sector vars have the least NA, before 2019
# oe_comparators2 %>%
#   filter(Year < 2020) %>%
#   filter(cities_list_MENA != " ") %>%
#   group_by(Year) %>%
#   summarize(across(ends_with("GVA_Pct"), 
#                    ~sum(is.na(.)), 
#                    .names = "{.col}_NA"),
#             total_rows = n()) %>%
#   arrange(Year) %>% 
#   View()



columns_to_pivot <- c("Agriculture_GVA_Pct", "Consumer_Services_GVA_Pct",
                      "Financial_Business_Services_GVA_Pct", "Industry_GVA_Pct",
                      "Public_Services_GVA_Pct", "Transport_Information_Communic_Services_GVA_Pct")

# Check that percentage columns sum to 100% for each location and year
# pie_data_check  <- oe_comparators2 %>%
#   pivot_longer(cols = all_of(columns_to_pivot), names_to = "Sector", values_to = "Percentage") %>%
#   group_by(Location, Year) %>%
#   mutate(Total = sum(Percentage)) %>%
#   filter(abs(Total - 100) > 1) %>% 
#   dplyr::select(Year, Location, Sector, Percentage, Total) %>%
# # Filter for locations/years where total isn't within 0.1% of 100%
#   View()

# Check number of columns per Location and Year
# pie_data_check <- oe_comparators2 %>%
#   pivot_longer(cols = all_of(columns_to_pivot), names_to = "Sector", values_to = "Percentage") %>%
#   group_by(Location, Year) %>%
#   summarize(num_cols = n_distinct(Sector)) %>% 
#   filter(num_cols != 6) %>% 
#   View()

# Only 2019
# Filter data for 2019
pie_data <- oe_comparators2 %>%
  pivot_longer(cols = all_of(columns_to_pivot), names_to = "Sector", values_to = "Percentage") %>% 
  filter(Year == 2019)

# Ensure percentages sum to 100 for each location
pie_data <- pie_data %>%
  group_by(Location) %>%
  mutate(Total = sum(Percentage)) %>%
  mutate(Percentage = Percentage / Total * 100) %>%
  mutate(Sector = str_remove(Sector, "_GVA_Pct"),
         Sector = str_replace_all(Sector, "_", " ")) %>% 
  ungroup() 

# Create a single plot with all locations
p05  <- ggplot(pie_data, aes(x = Location, y = Percentage, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(wes_palette("Zissou1", n = length(unique(pie_data$Sector)), type = "continuous"), "#D3D3D3")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), 
                     breaks = seq(0, 100, 20)) +
  facet_wrap(~cities_list_MENA, scales = "free_x", ncol = 1) +
  labs(title = "GVA Contribution by Frontier Cities and Rest of MENA Cities Cities, 2019",
       x = NULL,
       y = "Percentage",
       fill = "Sector") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    legend.text = element_text(size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  geom_text(aes(label = ifelse(Percentage >= 5, paste0(round(Percentage, 1), "%"), "")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white")

# Add percentage labels
p05  <- p05  + geom_text(aes(label = ifelse(Percentage >= 5, paste0(round(Percentage, 1), "%"), "")), 
                         position = position_stack(vjust = 0.5), 
                         size = 3, color = "white")

# Save the plot
ggsave(
  filename = here::here("Output", "MENA", "GVA_Sector_Frontier-Cities_Rest-of-MENA-Cities_2019.png"), 
  plot = p05, 
  width = 18,
  height = 12,
  dpi = 600
)

# For each city, for 2001-2019

# Create a plot for each location
create_gva_visualizations <- function(data, year_range = c(2001, 2019), 
                                      output_dir = here::here("Output", "MENA")) {
  
  # Data preparation
  pie_data <- data %>%
    pivot_longer(
      cols = ends_with("_GVA_Pct"), 
      names_to = "Sector", 
      values_to = "Percentage"
    ) %>%
    filter(between(Year, year_range[1], year_range[2])) %>%
    mutate(
      Sector = str_remove(Sector, "_GVA_Pct"),
      Sector = str_replace_all(Sector, "_", " "),
      Year = as.numeric(Year)
    )
  
  # Function to create single location plot
  create_location_plot <- function(loc_data) {
    # Get location name
    location <- unique(loc_data$Location)
    
    # Get years for labels
    start_year <- min(loc_data$Year)
    end_year <- max(loc_data$Year)
    
    # Prepare label data with correct positioning
    label_data <- loc_data %>%
      group_by(Year) %>%
      arrange(Year, desc(Sector)) %>%  # Important: consistent ordering
      mutate(
        # Calculate cumulative percentages for area positions
        ymax = cumsum(Percentage),
        ymin = lag(ymax, default = 0),
        pos = (ymax + ymin) / 2,  # Center position for labels
        perc = scales::percent(Percentage, accuracy = 0.1)
      ) %>%
      ungroup()
    
    # Create plot based on the number of years
    if (length(unique(loc_data$Year)) == 1) {
      # Single year: create a stacked bar chart
      p <- ggplot(loc_data, aes(x = factor(Year), y = Percentage, fill = Sector)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        scale_fill_manual(
          values = c(
            wes_palette("Zissou1", n = length(unique(loc_data$Sector)), 
                        type = "continuous"), 
            "#D3D3D3"
          )
        ) +
        labs(
          title = paste("GVA Contribution by Sector in", location, 
                        paste0("(", start_year, ")")),
          x = NULL,
          y = "Percentage",
          fill = "Sector"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank()
        ) +
        scale_y_continuous(labels = scales::percent_format()) +
        geom_text( data = label_data,
          aes(x = factor(Year), y = pos, label = perc, group = Sector),
          size = 5,
          fontface = "bold"
        ) 
      
    } else {
      
      # Multiple years: create an area chart
      p <- ggplot(loc_data, aes(x = Year, y = Percentage, fill = Sector)) +
        geom_area(position = "fill", alpha = 0.8) +
        scale_fill_manual(
          values = c(
            wes_palette("Zissou1", n = length(unique(loc_data$Sector)), 
                        type = "continuous"), 
            "#D3D3D3"
          )
        ) +
        labs(
          title = paste("GVA Contribution by Sector in", location, 
                        paste0("(", start_year, "-", end_year, ")")),
          x = NULL,
          y = "Percentage",
          fill = "Sector"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12, 
                                     face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank()
        ) +
        scale_y_continuous(labels = scales::percent_format()) +
        scale_x_continuous(breaks = unique(loc_data$Year)) +
        # Add labels and connecting lines with corrected positioning
        geom_text(
          data = label_data %>% 
            filter(Year == start_year | Year == end_year),
          aes(x = Year, y = pos, label = perc, group = Sector,
              hjust = ifelse(Year == start_year, 1.1, -0.1)),  # Adjusted hjust
          size = 5,
          fontface = "bold"
        ) +
        geom_line(
          data = label_data %>% 
            filter(Year == start_year | Year == end_year),
          aes(x = Year, y = pos, group = Sector),
          linetype = "dotted", 
          color = "gray50"
        )
    }
    
    # Save plot
    filename <- paste0(
      "GVA_Sector_Frontier-Cities_Rest-of-MENA-Cities_", 
      start_year, if (end_year != start_year) paste0("-", end_year), "_",
      gsub(" ", "_", location), 
      ".png"
    )
    
    ggsave(
      filename = file.path(output_dir, filename),
      plot = p,
      width = 12,
      height = 8,
      dpi = 600
    )
    
    return(p)
  }
  
  # Create plots for each location
  plots <- pie_data %>%
    group_by(Location) %>%
    group_map(~ create_location_plot(.x), .keep = TRUE)
  
  return(plots)
}

create_gva_visualizations(oe_comparators2
                          # , year_range =  c(2001, 2019)
                          , year_range =  c(2019, 2019)
                          , output_dir = here::here("Output", "MENA")
)

# Employment fo each city, for 2001-2019-----
# Create a plot for each city in MENA

oe_mena <- oe_mena %>%
  mutate(Total_Emp = as.numeric(EMPTOTT),
         Public_Services_Emp = as.numeric(EMPO_Q),
         Industry_Emp = as.numeric(EMPB_F),
         Financial_Business_Services_Emp = as.numeric(EMPK_N),
         Consumer_Services_Emp = as.numeric(EMPGIR_U),
         Agriculture_Emp = as.numeric(EMPA),
         Transport_Information_Communic_Services_Emp = as.numeric(EMPHJ)) %>%
  mutate(Public_Services_EMP_Pct = Public_Services_Emp / Total_Emp,
         Industry_EMP_Pct = Industry_Emp / Total_Emp ,
         Financial_Business_Services_EMP_Pct = Financial_Business_Services_Emp / Total_Emp,
         Consumer_Services_EMP_Pct = Consumer_Services_Emp / Total_Emp,
         Agriculture_EMP_Pct = Agriculture_Emp / Total_Emp,
         Transport_Information_Communic_Services_EMP_Pct = Transport_Information_Communic_Services_Emp / Total_Emp)

# 6 warnings!!

create_emp_visualizations <- function(data, year_range = c(2001, 2019), 
                                      output_dir = here::here("Output", "MENA")) {
  
  # Data preparation
  pie_data <- data %>%
    pivot_longer(
      cols = ends_with("_EMP_Pct"), 
      names_to = "Sector", 
      values_to = "Percentage"
    ) %>%
    filter(between(Year, year_range[1], year_range[2])) %>%
    mutate(
      Sector = str_remove(Sector, "_EMP_Pct"),
      Sector = str_replace_all(Sector, "_", " "),
      Year = as.numeric(Year)
    )
  
  # Function to create single location plot
  create_location_plot <- function(loc_data) {
    
    # Get location name
    location <- unique(loc_data$Location)
    
    # Get years for labels
    start_year <- min(loc_data$Year)
    end_year <- max(loc_data$Year)
    
    # Prepare label data with correct positioning
    label_data <- loc_data %>%
      group_by(Year) %>%
      arrange(Year, desc(Sector)) %>%  # Important: consistent ordering
      mutate(
        # Calculate cumulative percentages for area positions
        ymax = cumsum(Percentage),
        ymin = lag(ymax, default = 0),
        pos = (ymax + ymin) / 2,  # Center position for labels
        perc = scales::percent(Percentage, accuracy = 0.1)
      ) %>%
      ungroup()
    
    # Create plot based on the number of years
    if (length(unique(loc_data$Year)) == 1) {
      # Single year: create a stacked bar chart
      p <- ggplot(loc_data, aes(x = factor(Year), y = Percentage, fill = Sector)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        scale_fill_manual(
          values = c(
            wes_palette("Zissou1", n = length(unique(loc_data$Sector)), 
                        type = "continuous"), 
            "#D3D3D3"
          )
        ) +
        labs(
          title = paste("Employment by Sector in", location, 
                        paste0("(", start_year, ")")),
          x = NULL,
          y = "Percentage",
          fill = "Sector"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank()
        ) +
        scale_y_continuous(labels = scales::percent_format()) +
        geom_text( data = label_data,
          aes(x = factor(Year), y = pos, label = perc, group = Sector),
          size = 5,
          fontface = "bold"
        ) 
      
    } else {
      # Multiple years: create an area chart
      p <- ggplot(loc_data, aes(x = Year, y = Percentage, fill = Sector)) +
        geom_area(position = "fill", alpha = 0.8) +
        scale_fill_manual(
          values = c(
            wes_palette("Zissou1", n = length(unique(loc_data$Sector)), 
                        type = "continuous"), 
            "#D3D3D3"
          )
        ) +
        labs(
          title = paste("Employment by Sector in", location, 
                        paste0("(", start_year, "-", end_year, ")")),
          x = NULL,
          y = "Percentage",
          fill = "Sector"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12, 
                                     face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank()
        ) +
        scale_y_continuous(labels = scales::percent_format()) +
        scale_x_continuous(breaks = unique(loc_data$Year)) +
        # Add labels and connecting lines with corrected positioning
        geom_text(
          data = label_data %>% 
            filter(Year == start_year | Year == end_year),
          aes(x = Year, y = pos, label = perc, group = Sector,
              hjust = ifelse(Year == start_year, 1.1, -0.1)),  # Adjusted hjust
          size = 5,
          fontface = "bold"
        ) +
        geom_line(
          data = label_data %>% 
            filter(Year == start_year | Year == end_year),
          aes(x = Year, y = pos, group = Sector),
          linetype = "dotted", 
          color = "gray50"
        )
    }
    
    # Save plot
    filename <- paste0(
      "Employment_Sector_Frontier-Cities_Rest-of-MENA-Cities_", 
      start_year, if (end_year != start_year) paste0("-", end_year), "_",
      gsub(" ", "_", location), 
      ".png"
    )
    
    ggsave(
      filename = file.path(output_dir, filename),
      plot = p,
      width = 12,
      height = 8,
      dpi = 600
    )
    
    return(p)
  }
  
  # Create plots for each location
  plots <- pie_data %>%
    group_by(Location) %>%
    group_map(~ create_location_plot(.x), .keep = TRUE)
  
  return(plots)
}

create_emp_visualizations(oe_mena 
                          # , year_range =  c(2001, 2019)
                            , year_range = c(2019, 2019)
                          , output_dir = here::here("Output", "MENA")
                         )



