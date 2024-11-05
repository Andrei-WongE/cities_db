# Request for Indian cities data

# Load packages and data, see Main file
require(here)

source(here("Master_variables.R"))

if(!exists("data")) stop("Data not found")

source("Plot_functions_V1.R")
source("plot_function.R")


# Create output folder
dir.create("Output/India", showWarnings = FALSE)

# Filter data for India
oe_india <- data %>% dplyr::filter(Country == "India")

oe_india %>% distinct(.$Location) %>% View() #72 (not considering national

# Plot graphs
# Cities: All cities in OE.

# Charts, all cities
# 
# 1 chart, with bar chart of population in decr- <- easing order, for 2019.  (for first three try to make sure the labels are readable)
# 1 chart, with bar chart of GDP (real, PPP) in decreasing order, for 2019.
# 1 chart, with bar chart of GDP per capita in decreasing order, for 2019.
# 1 chart, with line chart of total GDP growth 2000-2019, highlight highest grower and worst performer, and focal cities
# Distance for frontiers, all cities in the dataset, color Indian cities, 
# color for each size band of population with top 3% (use orange for Indian cities, 
# and bright green for the frontier), the other think you can do here is estimate simple regressions of Y=Bp+r (where Y is GDP, and p is population) – you estimate B using regression  for only Indian cities, and B using regressions for only frontier cities, and I can use this to estimate the “distance to fronteer”.)
# Label cities that are standing out.
# 
 
# Charts, focus and comparators
# 
 
# For this group keep consistent color coding
# 
# - 1 chart, with bar chart of population in decreasing order, for 2019.
# - 1 chart, with bar chart of GDP (real, PPP) in decreasing order, for 2019.
# 
# - 1 chart, with bar chart of GDP per capita in decreasing order, for 2019.
# 
# - 1 chart, with line chart of total GDP growth 2000-2019, highlight highest grower and worst performer, and focal cities
# 
# 1 chart, bar chart 100%, structure of GVA
# 10 charts (or however many cities and comparators we get) , stacked plot, change in GVA, 2000-2019

# Data wrangling
oe_india <- oe_india %>% 
  mutate(POPTOTT = as.numeric(POPTOTT)) %>% 
  mutate(GDPTOTUSC = as.numeric(GDPTOTUSC)) %>% 
  mutate(GDP_per_capita_PPP = GDPTOTPPPC / POPTOTT) %>%
  filter(Location != Country)

# General comparison charts -----

## Population
  
p01 <- generate_bar_plot(subset(oe_india, Year == 2019), 
         x_var = "Location",
         y_var = "POPTOTT",
         x_lab = NULL,
         y_lab = "Population (millions)",
         orientation = "vertical",
         sort_bars = "descending",
         title = "Population of selected Indian cities, 2019",
         subtitle = "(millions)",
         show_values = TRUE,
         value_format = scales::label_number(
                                 unit = "m", 
                                 scale = 1e-3,
                                 accuracy = 0.1),
         source_text = "Oxford City Database, 2022",
         source_size = 10,
         rotate_x_labels = TRUE) 

ggsave(filename = here::here("Output","India", "Total_Population_2019.png"),
       plot = p01, width = 12, height = 15, dpi = 600)

## GDP
p02 <- generate_bar_plot(subset(oe_india, Year == 2019), 
                         x_var = "Location",
                         y_var = "GDPTOTUSC",
                         x_lab = NULL,
                         y_lab = "GDP (millions)",
                         orientation = "vertical",
                         sort_bars = "descending",
                         title = "GDP of selected Indian cities, 2019",
                         subtitle = "Real, PPP adjusted (millions)",
                         show_values = TRUE,
                         value_format = scales::label_number(
                           unit = "m", 
                           scale = 1e-3,
                           accuracy = 0.1),
                         source_text = "Oxford City Database, 2022",
                         source_size = 10,
                         rotate_x_labels = TRUE) 

ggsave(filename = here::here("Output","India", "Total_GDP_2019.png"),
       plot = p02, width = 12, height = 15, dpi = 600)

## GDP per capita
p03 <- generate_bar_plot(subset(oe_india, Year == 2019), 
                         x_var = "Location",
                         y_var = "GDP_per_capita_PPP",
                         x_lab = NULL,
                         y_lab = "GDP per capita (thousands)",
                         orientation = "vertical",
                         sort_bars = "descending",
                         title = "GDP per capita of selected Indian cities, 2019",
                         subtitle = "Real, PPP adjusted (thousands)",
                         show_values = TRUE,
                         value_format = scales::label_number(
                           unit = "th", 
                           scale = 1,
                           accuracy = 0.1),
                         source_text = "Oxford City Database, 2022",
                         source_size = 10,
                         rotate_x_labels = TRUE) 

ggsave(filename = here::here("Output","India", "GDPpc_2019.png"),
       plot = p03, width = 12, height = 15, dpi = 600)


## GDP growth
growth_rates <- oe_india %>% 
  dplyr::filter(Year %in% c(2001, 2019)) %>%
  group_by(Location) %>%
  mutate(GDP_growth = if_else(Year == 2019,
                              (GDPTOTUSC[Year == 2019] - GDPTOTUSC[Year == 2001]) / GDPTOTUSC[Year == 2001],
                              NA_real_)) %>%  # Only created in Location[Year == 2019]
  dplyr::filter(Year == c(2001, 2019)) %>%
  dplyr::select(Location, Year, GDP_growth) #%>%
  # dplyr::select(Location, Year, GDPTOTUSC, GDP_growth) #%>%
  # 60 out of 72 cities have no data for 2000
  # filter(Year == 2019 & !is.na(GDPTOTUSC)) %>% 
  # filter(is.na(GDPTOTUSC)) %>%
  # View()

# Find in which Year GDPTOTUSC has the least NA, before 2019
  # oe_india %>%
  #   filter(Year < 2019) %>%
  #   group_by(Year) %>%
  #   summarize(
  #     na_count = sum(is.na(GDPTOTUSC)),
  #     total_rows = n(),
  #     percent_complete = (1 - na_count/total_rows) * 100
  #   ) %>%
  #   arrange(na_count) %>% 
  #   View() # 2001, changing this in growth_rates calculations

oe_india <- oe_india %>%
  left_join(growth_rates, by = c("Location", "Year"))
  
p04 <- generate_bar_plot(subset(oe_india, Year == 2019), 
                         x_var = "Location",
                         y_var = "GDP_growth",
                         x_lab = NULL,
                         y_lab = "GDP growth rate",
                         orientation = "vertical",
                         sort_bars = "descending",
                         title = "GDP growth rate by selected Indian cities, 2001-2019",
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

ggsave(filename = here::here("Output","India", "GDP_growth_2001-2019.png"),
       plot = p04, width = 12, height = 15, dpi = 600)

## Frontier distance
data_frontier <- data %>%
  mutate(GDPTOTUSC = as.numeric(GDPTOTUSC),
         POPTOTT = as.numeric(POPTOTT)) %>%
  filter(Year %in% c(2001, 2019)) %>%
  group_by(Location) %>%
  summarize(
    GDP2001 = GDPTOTUSC[Year == 2001],
    GDP2019 = GDPTOTUSC[Year == 2019],
    POPTOTT = POPTOTT[Year == 2019],
    Country = first(Country)
  ) %>%
  mutate(
    GDP_growth = if_else(!is.na(GDP2001) & !is.na(GDP2019),
                         (GDP2019 / GDP2001)^(1/18) - 1,
                         NA_real_)
  ) %>%
  filter(Location != Country) %>% 
  dplyr::filter(!is.na(GDP_growth))

  # %>%
  # mutate(
  #   growth_quantile = ntile(GDP_growth, 100),
  #   point_color = case_when(
  #     Country == "India" ~ "orange",
  #     growth_quantile >= 97 ~ "green",
  #     TRUE ~ "grey"
  #   )
  # ) %>%
  # ggplot(aes(x = log(POPTOTT), y = log(GDP_growth), color = point_color)) +
  # geom_point() +
  # geom_text(data = . %>% filter(Country == "India"), 
  #           aes(label = Location), 
  #           hjust = -0.1, 
  #           size = 3) +
  # geom_smooth(method = "lm", se = FALSE) +
  # scale_color_identity() +
  # labs(x = "Log Population (2019)", 
  #      y = "Log GDP Growth Rate (2001-2019)",
  #      title = "GDP Growth vs Population") +
  # theme_minimal()

# Step 2: Identify frontier cities
# Create population percentiles and find max GDP_growth for each percentile
# Ensure non-missing GDP_growth and create deciles
# Manually define decile breaks to ensure uniqueness
unique_breaks <- unique(quantile(data_frontier$POPTOTT, probs = seq(0, 1, 0.01), na.rm = TRUE))

# Ensure non-missing GDP_growth and create deciles
frontier_cities <- data_frontier %>%
  dplyr::filter(!is.na(GDP_growth)) %>%
  arrange(POPTOTT) %>%
  mutate(pop_percentile = cut(POPTOTT, 
                              breaks = c(unique_breaks[1], unique_breaks[-1] + 1e-7),
                              include.lowest = TRUE, labels = FALSE)) %>%
  group_by(pop_percentile) %>%
  slice_max(GDP_growth, n = 1) %>%
  ungroup()

View(frontier_cities)


# Step 3: Estimate frontier regression
frontier_model <- lm(log(GDP_growth) ~ log(POPTOTT), data = frontier_cities)

# Step 4: Calculate distances to frontier for all cities
results <- data_frontier %>%
  mutate(
    # Predicted frontier GDP_growth for each city's population
    predicted_frontier = exp(predict(frontier_model, 
                                     newdata = data.frame(POPTOTT = POPTOTT))),
    # Distance to frontier as percentage difference
    frontier_distance = ((GDP_growth - predicted_frontier) / predicted_frontier) * 100
  )

# Create visualization
require(ggrepel)

indian_frontier_cities <- frontier_cities %>% 
  filter(Country == "India") %>%
  # Add positioning logic
  mutate(
    above_line = log(GDP_growth) > predict(frontier_model, 
                                           newdata = data.frame(POPTOTT = POPTOTT))
  )

(frontier_plot <- ggplot() +
# Base cities (grey)
geom_point(data = results %>% 
            filter(Country != "India" & !(Location %in% frontier_cities$Location)),
          aes(x = log(POPTOTT), y = log(GDP_growth)), 
          color = "grey", 
          alpha = 0.3) +
# Indian cities (orange)
geom_point(data = results %>% 
            filter(Country == "India"),
          aes(x = log(POPTOTT), y = log(GDP_growth)), 
          color = "orange", 
          alpha = 0.5) +
# Frontier cities (green)
geom_point(data = frontier_cities,
          aes(x = log(POPTOTT), y = log(GDP_growth)),
          color = 'lightgreen', 
          size = 3, 
          alpha = 0.7) +
# Labels with smart placement
geom_text_repel(
 data = indian_frontier_cities,
 aes(x = log(POPTOTT), 
     y = log(GDP_growth), 
     label = Location),
 size = 2.5,
 force = 8,
 box.padding = 0.3,
 point.padding = 0.2,
 max.overlaps = Inf,
 direction = "both",
 segment.size = 0.3,
 segment.color = "grey50",
 segment.linetype = "dotted",
 min.segment.length = 0,
 nudge_y = ifelse(indian_frontier_cities$above_line, 0.2, -0.2),  # Push labels up or down based on position
 xlim = c(6, 10),
 ylim = c(-8, -1)
) +
# Frontier line
geom_smooth(data = frontier_cities,
           aes(x = log(POPTOTT), y = log(GDP_growth)),
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
 plot.margin = margin(1, 1, 1, 1, "cm")
) +
coord_cartesian(clip = "off") +
labs(x = "Log Population (2019)",
    y = "Log GDP Growth Rate (2001-2019)",
    title = "Economic Frontier Analysis",
    subtitle = "Green points and line represent frontier cities, orange points represent cities from India")
)

ggsave(filename = here::here("Output","India", "Frontier-Cities_2019_break.png"),
       plot = frontier_plot, width = 12, height = 10, dpi = 600)

# Charts, focus and comparators -----
# Mission cities
# # # # # # # # # # # # # # # # # 
mission_categories <- list(
  Group_1 = c("Bhubaneswar", 
                "Vijayawada",
                "Visakhapatnam", 
                "Guwahati"),
  
  Group_2 = c("Indore",
                "Monterrey",
                "Seattle-Tacoma-Bellevue, WA",
                "Houston-The Woodlands-Sugar Land, TX"),

  Group_3 = c("Bhopal",
                "Aurangabad",
                "Chandigarh",
                "Thiruvananthapuram")
)

oe_comparators <- data %>% 
  mutate(POPTOTT = as.numeric(POPTOTT)) %>% 
  mutate(GDPTOTUSC = as.numeric(GDPTOTUSC)) %>% 
  mutate(GDP_per_capita_PPP = GDPTOTPPPC / POPTOTT)

oe_comparators <-
  add_group_category(oe_comparators, 
                    categories = mission_categories,
                    var_col = "Location",
                    new_col = "mission_categories",
                    warn_unmapped = TRUE) %>% 
  dplyr::filter(mission_categories != " ")

## Population
  create_population_plot(subset(oe_comparators, Year == 2019),
                         location_var = "Location",
                         category_var = "mission_categories", 
                         year_var = "Year", 
                         variable_name = "POPTOTT",
                         value_format = scales::label_number(
                           scale = 1,
                           accuracy = NULL,
                           big.mark = ",",
                           decimal.mark = "."),
                         title = "Total Population by Selected cities and Comparators, 2019",
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
                         filename = "Total_Population_Mission-Cities_2019.png",
                         width = 10,
                         height = 8
                         )
## GDP
base_plot <- create_population_plot(subset(oe_comparators, Year == 2019),
                       location_var = "Location",
                       category_var = "mission_categories", 
                       year_var = "Year", 
                       variable_name = "GDPTOTUSC",
                       value_format = scales::label_number(
                         scale = 1,
                         accuracy = NULL,
                         big.mark = ",",
                         decimal.mark = "."),
                       title = "GDP by Selected cities and Comparators, 2019",
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
                       filename = "Total_GDP_Mission-Cities_2019.png",
                       width = 10,
                       height = 8
                      )

require(ggbreak)

# Function to create a broken axis plot
# Function requires ggbreak package
if (!requireNamespace("ggbreak", quietly = TRUE)) {
  stop("Please install the ggbreak package: install.packages('ggbreak')")
}

create_broken_axis_plot <- function(plot, breaks, upper_space = 0.3, y_expansion = 0.05) {
  # Input validation
  if (!is.numeric(breaks) || length(breaks) != 2) {
    stop("breaks must be a numeric vector of length 2")
  }
  if (breaks[1] >= breaks[2]) {
    stop("First break point must be less than second break point")
  }
  
  # Get the order and colors from the original plot
  location_order <- levels(plot$data$Location)
  if(is.null(location_order)) {
    location_order <- unique(plot$data$Location)
  }
  original_colors <- plot$scales$get_scales("fill")$palette(12)
  
  # Create the modified plot with break
  broken_plot <- plot +
    ggbreak::scale_y_break(
      breaks = breaks,
      space = upper_space,  # Control the relative space of the upper section
      expand = expansion(mult = c(0, y_expansion))  # Tighter control of y-axis expansion
    ) +
    scale_fill_manual(
      values = original_colors,
      limits = location_order
    ) +
    theme(
      plot.margin = margin(t = 20, r = 20, b = 20, l = 50),  # Adjusted margins
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.8, "cm"),
      legend.box = "horizontal",
      legend.spacing.x = unit(0.2, "cm")  # Space between legend items
    ) +
    guides(fill = guide_legend(
      title = "Location", 
      nrow = 2,  # Arrange in 2 rows
      byrow = TRUE,
      title.position = "top"
    ))
  
  return(broken_plot)
}

(final_plot <- create_broken_axis_plot(
  base_plot,
  breaks = c(25000, 350000),  # Define break points
  upper_space = 0.3 , # More compressed upper section
  y_expansion = 0.05
  )
)

ggsave(
  filename = here::here("Output","India","Total_GDP_Mission-Cities_2019_break.png"), 
  plot = final_plot, 
  width = 12,
  height = 8,
  dpi = 600
)

## GDP per capita
create_population_plot(subset(oe_comparators, Year == 2019),
                       location_var = "Location",
                       category_var = "mission_categories", 
                       year_var = "Year", 
                       variable_name = "GDP_per_capita_PPP",
                       value_format = scales::label_number(
                         scale = 1,
                         accuracy = 0.1,
                         decimal.mark = "."),
                       title = "GDP per capita by Selected cities and Comparators, 2019",
                       subtitle = NULL,
                       source_text = "Oxford City Database, 2022",  
                       source_size = 8,    
                       x_label = "Year",
                       y_label = "Real, PPP adjusted (thousands)",
                       category_order = "by_name",
                       within_group_order = "by_name",
                       palette = "Zissou1",
                       line_size = 1.2,
                       label_size = 3,
                       title_size = 16,  
                       save_plot = TRUE,
                       filename = "GDP_per_capita_Mission-Cities_2019.png",
                       width = 10,
                       height = 8
                      )

## GDP growth
growth_rates_comparators <- oe_comparators %>% 
  dplyr::filter(Year %in% c(2001, 2019)) %>%
  group_by(Location) %>%
  mutate(GDP_growth = if_else(Year == 2019,
                              (GDPTOTUSC[Year == 2019]/GDPTOTUSC[Year == 2001])^(1/18) - 1,  # CAGR formula
                              NA_real_)) %>%
  dplyr::filter(Year == c(2001, 2019)) %>%
  dplyr::select(Location, Year, GDP_growth)

# Find in which Year GDPTOTUSC has the least NA, before 2019
# oe_comparators %>% 
#   filter(Year < 2019) %>% 
#   filter(mission_categories != " ") %>% 
#   group_by(Year) %>% 
#   summarize(
#     na_count = sum(is.na(GDPTOTUSC)),
#     total_rows = n(),
#     percent_complete = (1 - na_count/total_rows) * 100
#   ) %>% 
#   arrange(na_count) %>% 
#   View() # 2001, only 1 missing Monterrey, changing this in growth_rates calculation


oe_comparators <- oe_comparators %>%
  left_join(growth_rates_comparators, by = c("Location", "Year"))

create_population_plot(subset(oe_comparators, Year == 2019),
                       location_var = "Location",
                       category_var = "mission_categories", 
                       year_var = "Year", 
                       variable_name = "GDP_growth",
                       value_format = scales::label_number(
                         unit = "%", 
                         scale = 100,
                         accuracy = 0.1,
                         decimal.mark = ".",
                         suffix = "%"),
                       title = "GDP growth rate by Selected cities and Comparators, 2001-2019",
                       subtitle = "Compound Average Growth Rate (CAGR)",
                       source_text = "Oxford City Database, 2022",  
                       source_size = 8,    
                       x_label = "Year",
                       y_label = "Percentage change between 2001-2019",
                       palette = "Zissou1",
                       line_size = 1.2,
                       label_size = 3,
                       title_size = 16,
                       category_order = "by_name",
                       within_group_order = "by_name",
                       save_plot = FALSE,
                       filename = "GDP_growth_Mission-Cities_2001-2019.png",
                       width = 10,
                       height = 8
)

# Leading cities and comparators plots
# # # # # # # # # # # # # # # # # # # # # # # 
cities_list <- list(
  Leading_cities = c("Delhi", "Chennai", "Bengaluru", "Surat", "Mumbai", "Hyderabad (India)"),
  Comparators = c("Guangzhou, Guangdong", "Bangkok", "Shanghai", "Jakarta", "Monterrey")
)

oe_comparators2 <- data %>% 
  mutate(POPTOTT = as.numeric(POPTOTT)) %>% 
  mutate(GDPTOTUSC = as.numeric(GDPTOTUSC)) %>% 
  mutate(GDP_per_capita_PPP = GDPTOTPPPC / POPTOTT)

oe_comparators2 <-
  add_group_category(oe_comparators2, 
                     categories = cities_list,
                     var_col = "Location",
                     new_col = "cities_list",
                     warn_unmapped = TRUE)%>% 
  dplyr::filter(cities_list != " ")


## Population
create_population_plot(subset(oe_comparators2, Year == 2019),
                       location_var = "Location",
                       category_var = "cities_list", 
                       year_var = "Year", 
                       variable_name = "POPTOTT",
                       value_format = scales::label_number(
                         scale = 1,
                         accuracy = NULL,
                         big.mark = ",",
                         decimal.mark = "."),
                       title = "Total Population by Leading and Comparators Cities, 2019",
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
                       filename = "Total_Population_Leading-Comparators_2019.png",
                       width = 10,
                       height = 8
)
## GDP
create_population_plot(subset(oe_comparators2, Year == 2019),
                       location_var = "Location",
                       category_var = "cities_list", 
                       year_var = "Year", 
                       variable_name = "GDPTOTUSC",
                       value_format = scales::label_number(
                         scale = 1,
                         accuracy = NULL,
                         big.mark = ",",
                         decimal.mark = "."),
                       title = "GDP by Leading and Comparators Cities, 2019",
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
                       filename = "Total_GDP_Leading-Comparators_2019.png",
                       width = 10,
                       height = 8
)

## GDP per capita
create_population_plot(subset(oe_comparators2, Year == 2019),
                       location_var = "Location",
                       category_var = "cities_list", 
                       year_var = "Year", 
                       variable_name = "GDP_per_capita_PPP",
                       value_format = scales::label_number(
                         scale = 1,
                         accuracy = 0.1,
                         big.mark = ",",
                         decimal.mark = "."),
                       title = "GDP per capita by Leading and Comparators Cities, 2019",
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
                       filename = "GDP_per_capita_Leading-Comparators_2019.png",
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
#   filter(cities_list != " ") %>%
#   group_by(Year) %>%
#   summarize(
#     na_count = sum(is.na(GDPTOTUSC)),
#     total_rows = n(),
#     percent_complete = (1 - na_count/total_rows) * 100
#   ) %>%
#   arrange(na_count) %>%
#   View() # 2001, only 1 missing, changing this in growth_rates calculation

# oe_comparators2 %>%
#   filter(cities_list != " ") %>%
#   summarise(total_rows = n(),
#             missing_gdp = sum(is.na(GDP_growth)),
#             pct_missing = round(sum(is.na(GDP_growth))/n()*100, 1)) %>%
#   glimpse()

oe_comparators2 <- oe_comparators2 %>%
  left_join(growth_rates_comparators2, by = c("Location", "Year"))

create_population_plot(subset(oe_comparators2, Year == 2019),
                       location_var = "Location",
                       category_var = "cities_list", 
                       year_var = "Year", 
                       variable_name = "GDP_growth",
                       value_format = scales::label_number(
                         unit = "%", 
                         scale = 100,
                         accuracy = 0.01,
                         decimal.mark = ".",
                         suffix = "%"),
                       title = "GDP growth rate by Leading and Comparators Cities, 2001-2019",
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
                       filename = "GDP_growth_Leading-Comparators_2001-2019.png",
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
         , Consumer_services_GVA_Pct = GVAGIR_UPPPC / GVATOTPPPC
         , Financial_business_services_GVA_Pct = GVAK_NPPPC / GVATOTPPPC
         , Industry_GVA_Pct = GVAB_FPPPC / GVATOTPPPC          
         , Public_services_GVA_Pct =  GVAO_QPPPC / GVATOTPPPC 
         , Transport_Information_Communic_Services_GVA_Pct =  GVAHJPPPC / GVATOTPPPC
  ) # Decimal format

# Find in which Year sector vars have the least NA, before 2019
# oe_comparators2 %>%
#   filter(Year < 2020) %>%
#   filter(cities_list != " ") %>%
#   group_by(Year) %>%
#   summarize(across(ends_with("GVA_Pct"), 
#                    ~sum(is.na(.)), 
#                    .names = "{.col}_NA"),
#             total_rows = n()) %>%
#   arrange(Year) %>% 
#   View()



columns_to_pivot <- c("Agriculture_GVA_Pct", "Consumer_services_GVA_Pct",
                      "Financial_business_services_GVA_Pct", "Industry_GVA_Pct",
                      "Public_services_GVA_Pct", "Transport_Information_Communic_Services_GVA_Pct")

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
  facet_wrap(~cities_list, scales = "free_x", ncol = 1) +
  labs(title = "GVA Contribution by Leading and Comparators Cities, 2019",
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
  filename = here::here("Output", "India", "GVA_Sector_Leading-Comparators_2019.png"), 
  plot = p05, 
  width = 15,
  height = 10,
  dpi = 600
)

# For each city, for 2001-2019

# Create a plot for each location
create_gva_visualizations <- function(data, year_range = c(2001, 2019), 
                                      output_dir = here::here("Output", "India")) {
  
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
  
  # Create directory if it doesn't exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
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
    
    # Create plot
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
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8, 
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
        size = 3,
        fontface = "bold"
      ) +
      geom_line(
        data = label_data %>% 
          filter(Year == start_year | Year == end_year),
        aes(x = Year, y = pos, group = Sector),
        linetype = "dotted", 
        color = "gray50"
      )
    
    # Save plot
    filename <- paste0(
      "GVA_Sector_Leading-Comparators_", 
      start_year, "-", end_year, "_",
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
                          , year_range = c(2001, 2019)
                          , output_dir = here::here("Output", "India")
                          )


