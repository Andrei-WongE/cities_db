# Request for Indian cities data

# Load packages and data, see Main file

source(here("Master_variables.R"))

if(!exists("data")) stop("Data not found")

source("Plot_functions_V1.R")

# Create output folder
dir.create("Output/India", showWarnings = FALSE)

# Filter data for India
oe_india <- data %>% filter(Country == "India")

oe_india %>% distinct(.$Location) %>% View() #72 (not considering national

# Plot graphs
# Cities: All cities in OE.

# Charts, all cities
# 
# 1 chart, with bar chart of population in decreasing order, for 2019.  (for first three try to make sure the labels are readable)
# 1 chart, with bar chart of GDP (real, PPP) in decreasing order, for 2019.
# 1 chart, with bar chart of GDP per capita in decreasing order, for 2019.
# 1 chart, with line chart of total GDP growth 2000-2019, highlight highest grower and worst performer, and focal cities
# Distance for frontiers, all cities in the dataset, color Indian cities, color for each size band of population with top 3% (use orange for Indian cities, and bright green for the frontier), the other think you can do here is estimate simple regressions of Y=Bp+r (where Y is GDP, and p is population) – you estimate B using regression  for only Indian cities, and B using regressions for only frontier cities, and I can use this to estimate the “distance to fronteer”.)
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
         orientation = "horizontal",
         sort_bars = "ascending",
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
                         orientation = "horizontal",
                         sort_bars = "ascending",
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
                         orientation = "horizontal",
                         sort_bars = "ascending",
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
                         orientation = "horizontal",
                         sort_bars = "ascending",
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
                       title = "Total Population by Category and City, 2019",
                       subtitle = "Total population in thousands",
                       x_label = "Year",
                       y_label = "Total Population (thousands)",
                       category_order = "desc",
                       within_group_order = "desc",
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
create_population_plot(subset(oe_comparators, Year == 2019),
                       location_var = "Location",
                       category_var = "mission_categories", 
                       year_var = "Year", 
                       variable_name = "GDPTOTUSC",
                       title = "GDP of selected Indian cities, 2019",
                       x_label = "Year",
                       y_label = "Real, PPP adjusted (millions)",
                       category_order = "desc",
                       within_group_order = "desc",
                       palette = "Zissou1",
                       line_size = 1.2,
                       label_size = 3,
                       title_size = 16,  
                       save_plot = FALSE,
                       filename = "Total_GDP_Leading-Comparators_2019.png",
                       width = 10,
                       height = 8
                      )

## GDP per capita
create_population_plot(subset(oe_india, Year == 2019),
                       location_var = "Location",
                       category_var = "mission_categories", 
                       year_var = "Year", 
                       variable_name = "GDP_per_capita_PPP",
                       title = "GDP per capita of selected Indian cities, 2019",
                       x_label = "Year",
                       y_label = "Real, PPP adjusted (millions)",
                       category_order = "desc",
                       within_group_order = "desc",
                       palette = "Zissou1",
                       line_size = 1.2,
                       label_size = 3,
                       title_size = 16,  
                       save_plot = FALSE,
                       filename = "GDP_per_capita_Leading-Comparators_2019.png",
                       width = 10,
                       height = 8
                      )

## GDP growth
growth_rates_comparators <- oe_comparators %>% 
  dplyr::filter(Year %in% c(2001, 2019)) %>%
  group_by(Location) %>%
  mutate(GDP_growth = if_else(Year == 2019,
                              (GDPTOTUSC[Year == 2019] - GDPTOTUSC[Year == 2001]) / GDPTOTUSC[Year == 2001],
                              NA_real_)) %>%  # Only created in Location[Year == 2019]
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
#   View() # 2001, only 1 missing, changing this in growth_rates calculation


oe_comparators <- oe_comparators %>%
  left_join(growth_rates_comparators, by = c("Location", "Year"))

create_population_plot(subset(oe_india, Year == 2019),
                       location_var = "Location",
                       category_var = "mission_categories", 
                       year_var = "Year", 
                       variable_name = "GDP_growth",
                       title = "GDP growth rate by selected Indian cities, 2001-2019",
                       x_label = "Year",
                       y_label = "Percentage change between 2001-2019",
                       category_order = "desc",
                       within_group_order = "desc",
                       palette = "Zissou1",
                       line_size = 1.2,
                       label_size = 3,
                       title_size = 16,  
                       save_plot = FALSE,
                       filename = "GDP_growth_Leading-Comparators_2001-2019.png",
                       width = 10,
                       height = 8
)

# Leading cities and comparators plots
# # # # # # # # # # # # # # # # # # # # # # # 
cities_list <- list(
  Leading_cities = c("Delhi", "Chennai", "Bengaluru", "Ahmedabad", "Mumbai"),
  Comparators = c("Guangzhou, Guangdong", "Bangkok", "Ahmedabad", "Shanghai", "Hyderabad (India)", "Monterrey")
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
                     warn_unmapped = TRUE)



