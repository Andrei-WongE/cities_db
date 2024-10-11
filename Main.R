## ---------------------------
##
## Script name: Global cities benchmarking databases
##
## Project: Geo WB
##
## Purpose of script: Merge db and extract benchmarking indicators
##
## Author: Andrei Wong Espejo
##
## Date Created: 2024-09-28
##
## Email: awonge01@student.bbk.ac.uk
##
## ---------------------------
##
## Notes: 
##   
##
## ---------------------------

## Load required packages ----

library("pacman")
library("here")
library("groundhog")

set.groundhog.folder(here("groundhog_library"))
groundhog.day = "2024-04-25" #"2020-05-12"
#Dowloaded fromn https://github.com/CredibilityLab/groundhog

pkgs = c("dplyr", "tidyverse", "janitor", "sf"
         , "ggplot2","xfun", "remotes", "sp", "spdep", "maptools"
         , "foreach", "doParallel", "parallel", "progress"
         , "doSNOW", "purrr", "patchwork"
         , "haven", "openxlsx", "MASS", "reticulate"
         , "future", "furrr", "data.table","leaflet"
         , "jtools", "tidyr", "ggspatial", "raster"
         , "prettymapr", "viridis", "labelled"
         , "writexl", "WDI", "wesanderson", "ggrepel"
)

groundhog.library(pkgs, groundhog.day, ignore.deps = c("stringi", "fs"))

## Program Set-up ------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
sf_use_s2(TRUE) # Use s2 spherical geometry for geographical coordinate operations 

## Runs the following --------
# 1. Load data
# 2. Include labels
# 3. Wrangles data for the required years (2011-2019)
# 4. Creates graphs and variables for the benchmarking indicators
# 5. Creates datasets in long and wide format

## Upload data -------
data <- read_dta(here("data","OE_GC_2021.dta"))

# Include labels
labels <- read_csv(here("Data","labels.csv"), col_names = TRUE) %>% 
  .[-1, ]

labels_vector <- setNames(labels$Label, labels$Variable)

for (var in names(labels_vector)) {
  if (!is.null(data[[var]])) {
    var_label(data[[var]]) <- labels_vector[var]
  }
}

# Delete unnecessary rows
data <- data[-c(1:7), ]

# Clean variables
data <- data %>%
  mutate(Location = trimws(sub(" - Total$", "", Location))) # National level includes this word

# Create variables, national if 3 letter country code Locationcode
data$National_level <- ifelse(nchar(data$Locationcode) == 3, 1, 0)

#Problems with missing data and In group 1117: `Country = "Colombia"` and `Year = "2000"

# Filter the data for the required years)
data_filtered <- data %>%
  filter(Year >= 2011 & Year <= 2019)

# # Extract PPP data from the World Bank API
# ppp_data <- WDI(indicator = "PA.NUS.PPP", start = 2011, end = 2019, extra = TRUE) %>% 
#   mutate(year = as.character(year))
# 
# # Merge the PPP data with the cities data
# data_merged <- data_filtered %>%
#   left_join(ppp_data, by = c("Location" = "capital", "Year" = "year"))

# Get all variable labels
var_label(data) %>% View()

# Filer for location and create groups
numeric_columns <- c("PEDYHHPUSN", "FCTOTPPPC", "FCTOTUSN", "FCTOTUSC", "EMPA", "EMPGIR_U", "EMPK_N", "EMPB_F",
                     "EMPO_Q", "EMPTOTT", "EMPHJ", "GDPTOTUSC", "GDPTOTPPPN", "GVAAUSN", "GVAGIR_UUSN", "GVAK_NUSN",
                     "GVAB_FUSN", "GVAO_QUSN", "GVATOTUSN", "GVAHJUSN")

countries <- c("Sri Lanka", "India", "Bangladesh", "Pakistan", "Vietnam", "Indonesia",
               "Singapore", "Thailand", "Malaysia", "China")

location <- c("Colombo", "Mumbai", "Kochi", "Chittagong", "Ho Chi Minh City", "Surabaya",
               "Singapore", "Bangkok", "Kuala Lumpur", "Hangzhou", "Qinhuangdao")


data_merged <- data_filtered %>% 
  filter(Country %in% countries) %>%
  filter(Location != "") %>% 
  # mutate(across(everything(), ~na_if(., "NA"))) %>% 
  mutate(across(all_of(numeric_columns), as.numeric)) %>% 
  mutate(category_var = case_when(
    str_detect(Location, "Colombo") ~ 1,
    str_detect(Location, "Mumbai|Kochi|Chittagong|Ho Chi Minh City|Surabaya") ~ 2,
    str_detect(Location, "Singapore|Bangkok|Kuala Lumpur|Hangzhou|Qinhuangdao") ~ 3
  )) %>% 
  filter(
    sapply(location, function(x) str_detect(Location, fixed(x))) %>% rowSums() > 0 | 
      Location == "" |
      (Location == Country & Country %in% countries)
  )

# Table of category_var
table(data_merged$category_var)

# Convert category_var to integer
data_merged$category_int <- as.integer(data_merged$category_var)

# Remove duplicates
data_merged <- distinct(data_merged)

# Label category variable
data_merged$category_var <- factor(data_merged$category_var, 
                            levels = 1:3, 
                            labels = c("City", "DIRECT COMPARATORS", "ASPIRATIONAL COMPARATORS"))

# Remove specific locations
# data <- data %>% filter(!(Location %in% c("Indonesia", "Cambodia", "Sri Lanka", "Mauritius")))

# Create category group
data_merged <- data_merged %>% group_by(category_var) %>% mutate(category_group = cur_group_id())

# Convert Year to numeric
data_merged$Year <- as.numeric(as.character(data_merged$Year))

#Load functions
functions <- c("Structure_plot_function.R", "plot_function.R")
invisible(lapply(here("Functions", functions)
                 , function(f) suppressMessages(source(f, echo = FALSE))))

### Total Population-----
require(scales)

data_merged <- data_merged %>%
  mutate(POPTOTT = as.numeric(POPTOTT)) %>%
  mutate(Total_population = POPTOTT)  
  
create_population_plot(data_merged,
                       variable_name = "Total_population",
                       title = "Total Population by Category and City (2011-2019)",
                       x_label = "Year",
                       y_label = "Total Population (thousands)",
                       save_plot = TRUE,
                       filename = "TOT_POP.png"
)

# # Order of locations based on their last data point
# location_order <- data_merged %>%
#   group_by(Location) %>%
#   summarize(last_value = last(Total_population)) %>%
#   arrange(desc(last_value)) %>%
#   pull(Location)
# 
# data_merged$Location <- factor(data_merged$Location, levels = location_order)
# 
# # Last values based on combination of Location and category_var
# last_values <- data_merged %>%
#   group_by(Location, category_var) %>%
#   slice_max(Year) %>%
#   ungroup() %>%
#   arrange(desc(Total_population))
# 
# interaction_levels <- interaction(last_values$Location, last_values$category_var, drop = TRUE)
# 
# p7 <- ggplot(data_merged, aes(x = Year,
#                               y = Total_population,
#                               color = interaction(Location, category_var),
#                               group = interaction(Location, category_var))) +
#   geom_line(size = 1.5) +
#   geom_point() +
#   scale_color_manual(values = wes_palette("Zissou1", n = length(interaction_levels), type = "continuous"),
#                      breaks = interaction_levels,
#                      labels = levels(interaction_levels)) +
#   labs(title = "Total Population by Category and City (2011-2019)",
#        x = "Year",
#        y = "Total Population (thousands)") +
#   theme_minimal() 
# 
# # Labels for ggrepel
# label_data <- data_merged %>%
#   group_by(Location, category_var) %>%
#   slice_max(Year) %>%
#   ungroup()
# 
# p7 <- p7 +
#   geom_text_repel(data = label_data,
#                   aes(label = paste0(Location, "-", category_var, "\n", 
#                                      comma(round(Total_population), accuracy = 1))),
#                   nudge_x = 1,  # Adjust this value to move labels horizontally
#                   direction = "y",
#                   hjust = 0,
#                   segment.size = 0.2,
#                   segment.color = "grey50",
#                   box.padding = 0.5,
#                   point.padding = 0.5,
#                   force = 2,
#                   size = 3) +  # Adjust text size if needed
#   theme(legend.position = "none") +  # Remove legend as labels are now on the plot
#   scale_x_continuous(limits = c(min(data_merged$Year), max(data_merged$Year) + 1))  # Extend x-axis for labels
# 
# ggsave(filename = here("Figures", "TOT_POP.png"), plot = p7, width = 10, height = 8)

### Total Employment-----
create_population_plot(data_merged,
                       variable_name = "EMPTOTT",
                       title = "Total Employment by Category and City (2011-2019)",
                       x_label = "Year",
                       y_label = "Total Employment (thousands)",
                       save_plot = TRUE,
                       filename = "TOT_EMP.png"
                       )

# 
# p1 <- ggplot(data_merged, aes(x = Year
#                               , y = EMPTOTT
#                               , color = factor(Location, levels = location_order))) +
#   geom_line(linewidth = 1.2) +  # Adjust the size to change line thickness
#   labs(title = "Total Employment",
#        x = "Year",
#        y = "Total Employment",
#        color = "Location") +
#   theme_minimal() +
#   scale_x_continuous(limits = c(2011, 2019), breaks = seq(2011, 2019, by = 1)) +
#   scale_y_continuous(n.breaks = 4, labels = scales::comma) +
#   theme(legend.position = "right") +
#   scale_color_manual(values = wes_palette("Zissou1", n = length(unique(data_merged$Location))
#                                           , type = "continuous"))
# 
# # Add labels at the end of the lines
# p1 <- p1 + geom_text_repel(data = data_merged %>% group_by(Location, category_var) %>% filter(Year == max(Year)),
#                            aes(label = Location),
#                            nudge_x = 0.5,  # Adjust this value to move the labels further to the right
#                            direction = "y",
#                            hjust = 0,
#                            segment.color = "grey50",
#                            size = 3,  # Adjust the size of the text
#                            box.padding = 0.3,  # Adjust padding around the text
#                            point.padding = 0.5,  # Adjust padding around the points
#                            max.overlaps = Inf)  # Allow for more overlaps
# 
# ggsave(filename = here::here("Figures", "TOT_EMP.png"), plot = p1, width = 8, height = 6)
### Structure of Employment-----
data_merged <- data_merged %>%
  mutate(across(c(EMPO_Q
                  , EMPB_F
                  , EMPK_N
                  , EMPGIR_U
                  , EMPA
                  , EMPHJ), as.numeric)) %>%
  mutate(
    Public_Services_Emp_Pct = EMPO_Q / EMPTOTT,
    Industry_Emp_Pct = EMPB_F / EMPTOTT,
    Financial_Busines_Serices_Emp_Pct = EMPK_N / EMPTOTT,
    Consumer_services_Emp_Pct = EMPGIR_U / EMPTOTT,
    Agriculture_Emp_Pct = EMPA / EMPTOTT,
    Transport_Information_Communic_Services_Emp_Pct = EMPHJ / EMPTOTT
  )

columns_to_pivot_Emp_Pct <- c("Public_Services_Emp_Pct",
                      "Industry_Emp_Pct",
                      "Financial_Busines_Serices_Emp_Pct",
                      "Consumer_services_Emp_Pct",
                      "Agriculture_Emp_Pct",
                      "Transport_Information_Communic_Services_Emp_Pct")

pie_data3 <- data_merged %>%
  pivot_longer(cols = all_of(columns_to_pivot_Emp_Pct)
               , names_to = "Employment_sector"
               , values_to = "Percentage")

p10 <- ggplot(pie_data3, aes(x = Location, y = Percentage, fill = Employment_sector)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c(wes_palette("Zissou1", n = length(unique(pie_data3$Employment_sector)), type = "continuous"), "#D3D3D3")) + # Adding an extra color
  labs(title = "Enployment Sector Percentage by Selected Categories (2011-2019)",
       x = "Location",
       y = "Percentage") +
  theme_minimal() +
  facet_wrap(~ Year, scales = "free", labeller = label_both) +
  theme(strip.text = element_text(size = 8)) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.1)),
            position = position_fill(vjust = 0.5), size = 2)

ggsave(filename = here::here("Figures", "EMP_SECTOR.png"), plot = p10, width = 8, height = 6)


# Code for Location specific plots
# Get unique locations
locations <- unique(pie_data3$Location)

# Create a plot for each location
for (loc in locations) {
  # Filter data for the current location
  loc_data <- pie_data3 %>% filter(Location == loc)
  
  # Create the plot
  p11 <- ggplot(loc_data, aes(x = Year, y = Percentage, fill = Employment_sector)) +
    geom_area(position = "fill") +
    scale_fill_manual(values = c(wes_palette("Zissou1", n = length(unique(pie_data3$Employment_sector)), type = "continuous"), "#D3D3D3")) +
    labs(title = paste("Employment by Sector (percentage) by Category in", loc, "(2019)"),
         x = "Year",
         y = "Percentage",
         fill = "Employment Sector") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(breaks = unique(loc_data$Year))
  
  # Save the plot with location-specific filename
  ggsave(
    filename = here::here("Figures", paste0("EMP_SECTOR_"
                                            , gsub(" ", "_", loc),
                                            "_FINAL" , ".png")), 
    plot = p11, 
    width = 10, 
    height = 8
  )
  
  print(p11)
}

## Only for one year, stacket bar
pie_data3 <- data_merged %>%
  pivot_longer(cols = all_of(columns_to_pivot_Emp_Pct),
               names_to = "Employment_sector",
               values_to = "Percentage") %>%
  filter(Year == 2019)

# Get unique locations
locations <- unique(pie_data3$Location)

# Create a plot for each location
for (loc in locations) {
  # Filter data for the current location and year 2019
  loc_data <- pie_data3 %>% 
    filter(Location == loc, Year == 2019) %>%
    group_by(category_var) %>%
    mutate(category_total = sum(Percentage)) %>%
    ungroup() %>%
    arrange(desc(category_total), desc(Percentage))
  
  # Create a factor for Employment_sector that preserves the order
  loc_data$Employment_sector <- factor(loc_data$Employment_sector, levels = unique(loc_data$Employment_sector))
  
  # Create the plot
  p11 <- ggplot(loc_data, aes(x = category_var, y = Percentage, fill = Employment_sector)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c(wes_palette("Zissou1", n = length(unique(loc_data$Employment_sector)), type = "continuous"), "#D3D3D3")) +
    labs(title = paste("Employment by Sector in", loc, "(2019)"),
         subtitle = "Employment Sector",
         x = "Category",
         y = "Percentage",
         fill = "Employment Sector") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    ) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
  
  # Add percentage labels
  p11 <- p11 + geom_text(
    aes(label = scales::percent(Percentage, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    size = 2.5,
    check_overlap = TRUE
  )
  
  # Save the plot with location-specific filename
  ggsave(
    filename = here::here("Figures", paste0("EMP_SECTOR"
                                            , gsub(" ", "_", loc)
                                            , "_2019_FINAL.png")), 
    plot = p11, 
    width = 14,
    height = 10
  )
  
  print(p11)
}

# Only Colombo, stacket chart
Colombo_data <- data_merged %>%
  filter(Location == "Colombo")

pie_data7 <- Colombo_data %>%
  pivot_longer(cols = all_of(columns_to_pivot_Emp_Pct)
               , names_to = "Employment_category"
               , values_to = "Percentage") %>%
  mutate(Employment_category = factor(Employment_category, levels = columns_to_pivot_Emp_Pct))

# Function to generate colors
generate_colors <- function(n_categories, palette_name = "Zissou1") {
  base_colors <- wes_palette(palette_name, n = n_categories, type = "continuous")
  if (length(base_colors) < n_categories) {
    color_function <- colorRampPalette(base_colors)
    base_colors <- color_function(n_categories)
  }
  return(base_colors)
}

# Updated plotting function
create_structure_plot <- function(data, output_file) {
  data$Year <- as.numeric(as.character(data$Year))
  
  data <- data %>%
    group_by(Location, Year) %>%
    arrange(desc(Employment_category)) %>%  # Reverse the order for stacking
    mutate(CumulativePercentage = cumsum(Percentage),
           YPosition = CumulativePercentage - Percentage/2) %>%
    ungroup()
  
  n_categories <- length(unique(data$Employment_category))
  colors <- generate_colors(n_categories)
  
  years <- unique(data$Year)
  start_year <- min(years)
  end_year <- max(years)
  
  label_data <- data %>%
    filter(Year %in% c(start_year, end_year)) %>%
    mutate(Label = scales::percent(Percentage, accuracy = 0.1, scale = 100),
           x_position = if_else(Year == start_year, Year - 0.5, Year + 0.5))
  
  p <- ggplot(data, aes(x = Year, y = Percentage, fill = Employment_category)) +
    geom_area(position = "stack") +
    scale_fill_manual(values = colors) +
    facet_wrap(~ Location, ncol = 1, scales = "free_y") +
    labs(title = "Employment by Sector (percentage) in Colombo (2011-2019)",
         x = "Year",
         y = "Percentage") +
    theme_minimal() +
    scale_x_continuous(breaks = years) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16, face = "bold"))
  
  p <- p + geom_text_repel(
    data = label_data,
    aes(x = x_position, y = YPosition, label = Label, color = Employment_category),
    direction = "y",
    segment.color = "grey50",
    show.legend = FALSE,
    size = 3,
    force = 1,
    max.overlaps = Inf
  )
  
  p <- p + scale_color_manual(values = colors)
  
  ggsave(filename = output_file, width = 12, height = 8)
  return(p)
}

# Create and save the plot
p18 <- create_structure_plot(pie_data7, here::here("Figures", "EMP_POP_COLOMBO_FINAL.png"))

# Only Colombo, raw total
data_merged <- data_merged %>%
  mutate(
    Public_Services_Emp = EMPO_Q,
    Industry_Emp = EMPB_F,
    Financial_Busines_Serices_Emp = EMPK_N,
    Consumer_services_Emp = EMPGIR_U,
    Agriculture_Emp = EMPA,
    Transport_Information_Communic_Services_Emp = EMPHJ
  )

Colombo_data <- data_merged %>%
  filter(Location == "Colombo")

columns_to_pivot_Emp <- c("Public_Services_Emp", "Industry_Emp"
                          , "Financial_Busines_Serices_Emp", "Consumer_services_Emp"
                          , "Agriculture_Emp", "Transport_Information_Communic_Services_Emp")

pie_data9 <- Colombo_data %>%
  dplyr::select(Year, all_of(columns_to_pivot_Emp)) %>%
  pivot_longer(cols = all_of(columns_to_pivot_Emp),
               names_to = "Employment_category",
               values_to = "Number_of_Employees") %>% 
  mutate(Employment_category = factor(Employment_category, levels = columns_to_pivot_Emp))

wes_palette <- wesanderson::wes_palette("Zissou1", n = length(unique(pie_data9$Employment_category)), type = "continuous")

# Calculate positions for labels
pie_data9 <- pie_data9 %>%
  group_by(Year) %>%
  arrange(desc(Employment_category)) %>%
  mutate(cumulative_sum = cumsum(Number_of_Employees),
         label_y = cumulative_sum - 0.5 * Number_of_Employees) %>%
  ungroup()

# Get start and end years
start_year <- min(pie_data9$Year)
end_year <- max(pie_data9$Year)

# Prepare label data (now only with numeric values)
label_data <- pie_data9 %>%
  filter(Year %in% c(start_year, end_year)) %>%
  mutate(label_x = ifelse(Year == start_year, Year - 0.5, Year + 0.5),
         label_text = comma(Number_of_Employees))

# Create the area chart with labels and legend
p20 <- ggplot(pie_data9, aes(x = Year, y = Number_of_Employees, fill = Employment_category)) +
  geom_area(position = "stack") +
  geom_text_repel(
    data = label_data,
    aes(x = label_x, y = label_y, label = label_text),
    direction = "y",
    hjust = ifelse(label_data$Year == start_year, 1, 0),
    vjust = 0.5,
    size = 3,
    segment.color = "grey50",
    segment.size = 0.2,
    box.padding = unit(0.35, "lines"),
    force = 1
  ) +
  scale_fill_manual(values = wes_palette) +
  labs(title = "Employment Composition in Colombo, 2011-2019",
       x = "Year",
       y = "Number of Employees (thousands)",
       fill = "Employment Category") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16)) +
  scale_x_continuous(breaks = unique(pie_data9$Year), 
                     expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(labels = comma) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(filename = here::here("Figures", "EMP_POP_COLOMBO_ABSOLUTE_FINAL.png"), plot = p20, width = 8, height = 6)

### Total GDP-----
create_population_plot(data_merged,
                       variable_name = "GDPTOTUSC",
                       title = "Total GDP by Category and City",
                       x_label = "Year",
                       y_label = "Total GDP (millions)",
                       save_plot = TRUE,
                       filename = "TOT_GDP.png"
)

# (ggplot(data, aes(x = Year, y = GDPTOTUSC, color = Location)) +
#    geom_point(size = 3) +
#    labs(title = "Total GDP",
#         x = "Year",
#         y = "Total GDP") +
#    theme_minimal() +
#    scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 5)) +
#    scale_y_continuous(n.breaks = 4))

### Real GDP per capita adjusted for PPP-----
data_merged <- data_merged %>%
  mutate(GDPTOTPPPC = as.numeric(GDPTOTPPPC),
         POPTOTT = as.numeric(POPTOTT),) %>%
  mutate(GDP_per_capita_PPP = GDPTOTPPPC / POPTOTT)

create_population_plot(data_merged,
                       variable_name = "GDP_per_capita_PPP",
                       title = "GDP per capita (PPP adjusted) by Category and City",
                       x_label = "Year",
                       y_label = "GDP per capita PPP (thousands)",
                       save_plot = TRUE,
                       filename = "R_GDP_PPP.png"
)

# ggplot(data_merged, aes(x = Location, y = GDP_per_capita_PPP, fill = category_var)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(data_merged$category_var)), type = "continuous")) +
#   labs(title = "GDP per Capita PPP by Category and City (2011-2019)",
#        x = "City",
#        y = "GDP per Capita PPP") +
#   theme_minimal() +
#   facet_wrap(~ Year)

# p1 <- ggplot(data_merged, aes(x = Year
#                               , y = EMPTOTT
#                               , color = category_var
#                               , group = interaction(Location, category_var))) +
#   geom_line(linewidth = 1.1) +
#   geom_point() +
#   scale_color_manual(values = wes_palette("Zissou1", n = length(unique(data_merged$category_var)), type = "continuous")) +
#   labs(title = "Total Employment by Category and City (2011-2019)",
#        x = "Year",
#        y = "Total Employment",
#        color = "Category") +
#   theme_minimal()
#
# # Add labels at the end of the line
# (p1 + geom_text_repel(data = data_merged %>% group_by(Location, category_var) %>% filter(Year == max(Year)),
#                            aes(label = Location),
#                            nudge_x = 1,  # Adjust this value to move the labels further to the right
#                            direction = "y",
#                            hjust = 0,
#                            segment.color = "grey50"))
#
# ggsave(filename = here::here("Figures", "TOT_EMP.png"), plot = p1, width = 8, height = 6)

# Code for Location specific plots
# Get unique locations
locations <- unique(data_merged$Location)

# Create a plot for each location
for (loc in locations) {
  # Filter data for the current location
  loc_data <- data_merged %>% 
    filter(Location == loc) %>%
    mutate(label = ifelse(Year == min(Year) | Year == max(Year), 
                          scales::comma(GDP_per_capita_PPP), NA))
  
  # Create the plot
  p16 <- ggplot(loc_data, aes(x = Year, y = GDP_per_capita_PPP)) +
    geom_line(color = wes_palette("Zissou1")[1], size = 1) +
    geom_point(color = wes_palette("Zissou1")[5], size = 3) +
    geom_text_repel(
      aes(label = label),
      nudge_x = 0.5,
      nudge_y = 0.5,
      size = 3,
      fontface = "bold",
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.5, "lines")
    ) +
    labs(title = paste("GDP per capita (Real, PPP) in", loc, "(2011-2019)"),
         x = "Year",
         y = "GDP per capita PPP (Real USD) (thousands)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = 2011:2019) +
    scale_y_continuous(labels = scales::comma)
  
  # Save the plot with location-specific filename
  ggsave(
    filename = here::here("Figures", paste0("GDP_PPP_", gsub(" ", "_", loc), ".png")), 
    plot = p16, 
    width = 10, 
    height = 6
  )
  
  # Display the plot
  print(p16)
}

# Bar chart 2019
data_2019 <- data_merged %>%
  filter(Year == 2019) %>%
  arrange(category_var, desc(GDP_per_capita_PPP)) %>%
  mutate(Location = factor(Location, levels = unique(Location)))  # Preserve order in plot

# Create color palette (extend if needed for more categories)
num_categories <- length(unique(data_2019$category_var))
color_palette <- wes_palette("Zissou1", num_categories, type = "continuous")

# Create the grouped bar chart
p17 <- ggplot(data_2019, aes(x = Location, y = GDP_per_capita_PPP, fill = category_var)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(GDP_per_capita_PPP)), 
            hjust = -0.1, 
            size = 3, 
            fontface = "bold") +
  coord_flip() +  # Flip coordinates for horizontal bars
  labs(title = "GDP per capita PPP by Location and Category (thousands), 2019",
       x = "Location",
       y = "GDP per capita PPP (Real USD)",
       fill = "Category:") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_blank(),  # Remove y-axis label as it's redundant
    panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::comma, 
                     expand = expansion(mult = c(0, 0.15))) +  # Expand y-axis to fit labels
  scale_fill_manual(values = color_palette)

# Save the plot
ggsave(
  filename = here::here("Figures", "GDP_PPP_Bar_Chart_2019.png"), 
  plot = p17, 
  width = 14,  # Increased width to accommodate categories
  height = 10
)


### Total Employment as a Pct of the working-age population-----
data_merged <- data_merged %>%
  mutate(POPTOTT = as.numeric(POPTOTT),
         EMPTOTT = as.numeric(EMPTOTT),
         POP0_14T = as.numeric(POP0_14T),
         POP65_T = as.numeric(POP65_T),) %>%
  mutate(Total_Employment_Pct = (EMPTOTT / (POPTOTT-POP0_14T-POP65_T) * 100))

create_population_plot(data_merged,
                       variable_name = "Total_Employment_Pct",
                       title = "Employment as % of working age population (15-65) by Category and City (2011-2019)",
                       x_label = "Year",
                       y_label = "Employment (percentage)",
                       save_plot = TRUE,
                       filename = "EMP_WA_POP_FINAL.png"
)


# ggplot(data_merged, aes(x = Location, y = Total_Employment_Pct, fill = category_var)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(data_merged$category_var)), type = "continuous")) +
#   labs(title = "Total Employment Percentage of Working-Age pop by Category and City",
#        x = "City",
#        y = "Total Employment Percentage") +
#   theme_minimal()

# p2 <- ggplot(data_merged, aes(x = Year
#                              , y = Total_Employment_Pct
#                              , color = interaction(Location, category_var)
#                              , group = interaction(Location, category_var))) +
#   geom_line(size = 1.5) +  # Increase the line width
#   geom_point() +
#   scale_color_manual(values = wes_palette("Zissou1", n = length(unique(interaction(data_merged$Location, data_merged$category_var))), type = "continuous")) +
#   labs(title = "Total Employment Percentage of Working-Age Population by Category and City (2011-2019)",
#        x = "Year",
#        y = "Total Employment Percentage") +
#   theme_minimal()
# 
# # Add labels at the end of line
# p2 + geom_text_repel(data = data_merged %>% group_by(Location, category_var) %>% filter(Year == max(Year)),
#                     aes(label = Location),
#                     nudge_x = 1,  # Adjust this value to move the labels further to the right
#                     direction = "y",
#                     hjust = 0,
#                     segment.color = "grey50")
# 
# ggsave(filename = here::here("Figures", "EMP_WA_POP.png"), plot = p2, width = 8, height = 6)

### Average household personal disposable income, real, PPP$-----
label_tothhincome <- attr(data_merged$PEDYHHPPPPC, "label")
print(label_tothhincome)

data_merged <- data_merged %>%
  mutate(PEDYHHPPPPC = as.numeric(PEDYHHPPPPC)) %>% 
  mutate(Avg_Household_Income_PPP = PEDYHHPPPPC)

create_population_plot(data_merged,
                       variable_name = "Avg_Household_Income_PPP",
                       title = "Average Household Income PPP by Category and City (2011-2019)",
                       x_label = "Year",
                       y_label = "Average Household Income PPP (Real USD)",
                       save_plot = TRUE,
                       filename = "AV_HH_DINC.png"
)

# ggplot(data_merged, aes(x = Location, y = Avg_Household_Income_PPP, fill = category_var)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(data_merged$category_var)), type = "continuous")) +
#   labs(title = "Average Household Income PPP by Category and City",
#        x = "City",
#        y = "Average Household Income PPP") +
#   theme_minimal()

# p3 <- ggplot(data_merged, aes(x = Year, y = Avg_Household_Income_PPP, color = category_var, group = interaction(Location, category_var))) +
#   geom_line(size = 1.5) +  # Increase the line width
#   geom_point() +
#   scale_color_manual(values = wes_palette("Zissou1", n = length(unique(data_merged$category_var)), type = "continuous")) +
#   labs(title = "Average Household Income PPP by Category and City (2011-2019)",
#        x = "Year",
#        y = "Average Household Income PPP") +
#   theme_minimal()
# 
# # Add labels at the end of line
# p3 + geom_text_repel(data = data_merged %>% group_by(Location, category_var) %>% filter(Year == max(Year)),
#                     aes(label = Location),
#                     nudge_x = 1,  # Adjust this value to move the labels further to the right
#                     direction = "y",
#                     hjust = 0,
#                     segment.color = "grey50")
# 
# ggsave(filename = here::here("Figures", "AV_HH_DINC.png"), plot = p3, width = 8, height = 6)

### Labor Force Participation Rate FALTA!!!!!-----
# data_merged <- data_merged %>%
#   mutate(Labor_Force_Participation_Rate = Labor_Force / Working_Age_Population * 100)

### Calculate Employment in High-Tech Industries as a Pct of total employment-----
data_filtered <-   data_merged %>% 
  dplyr::select(starts_with("EMP")) %>% 
  colnames()
# [1] "EMPA"         "EMPGIR_U"     "EMPK_N"      
# [5] "EMPB_F"       "EMPO_Q"       "EMPTOTT"      "EMPHJ

map(c("EMPA", "EMPGIR_U", "EMPK_N", "EMPB_F", "EMPO_Q", "EMPTOTT", "EMPHJ"),
    ~ attr(data_merged[[.x]], "label"))

data_merged <- data_merged %>%
  mutate(High_Skills_Employment_Pct = (EMPB_F + EMPK_N + EMPO_Q + EMPGIR_U) / EMPTOTT * 100) # Industry + finance + public serv + consumer serv

create_population_plot(data_merged,
                       variable_name = "High_Skills_Employment_Pct",
                       title = "Proportion of Industry, Finance & Business, Public ad Consumer Services Sector Enployment by Category and City (2011-2019)",
                       x_label = "Year",
                       y_label = "High Skill Employment (percentaje)",
                       save_plot = TRUE,
                       filename = "HIGH_SKILLS_EMP_FINAL.png"
)

### Private employment, 2019----

pie_data13 <- data_merged %>%
  filter(Year == 2019) %>%
  group_by(Location) %>%
  summarise(Private_Emp = sum(Private_Emp, na.rm = TRUE),
            category_var = first(category_var)) %>%  # Assume category_var is consistent per Location
  ungroup() %>%
  mutate(Total_Private_Employment = sum(Private_Emp, na.rm = TRUE),
         Percentage = Private_Emp / Total_Private_Employment * 100) %>%
  arrange(desc(Percentage))

# Create a single vertical stacked bar plot
p24 <- ggplot(pie_data13, aes(x = "", y = Percentage, fill = Location)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = wes_palette("Zissou1", n = nrow(pie_data13)
                                         , type = "continuous")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 100),
                     breaks = seq(0, 100, 10)) +
  labs(title = "Proportion of Industry, Finance & Business, Public ad Consumer Services Sector, 2019",
       x = NULL,
       y = "Percentage of share of High Skill Employment",
       fill = "City") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  # Modify only the legend to group by category_var
  guides(fill = guide_legend(byrow = TRUE,
                             title = "City",
                             order = 1,
                             override.aes = list(size = 3))) +
  # Add a second legend for categories
  scale_color_manual(values = setNames(rep("black", nrow(pie_data13))
                                       , pie_data13$Location),
                     breaks = pie_data13$Location,
                     labels = pie_data13$category_var,
                     name = "Category") +
  guides(color = guide_legend(override.aes = list(fill = NA, color = NA),
                              order = 2))

# Add percentage labels using ggrepel
p24 <- p24 +
  geom_text_repel(
    aes(label = paste0(Location, "\n", round(Percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3,
    direction = "y",
    force = 1,
    segment.color = NA
  )

# Save the plot
ggsave(
  filename = here::here("Figures", "HIGH_SKILLS_EMP_2019_FINAL.png"),
  plot = p24,
  width = 12,  # Increased width to accommodate the expanded legend
  height = 12,
  dpi = 300
)


### Consumer spending, PPP, real-----
map(c("FC03PPPC", "FC08PPPC", "FC01PPPC", "FC06PPPC", "FC04PPPC", "FC124PPPC"),
    ~ attr(data_merged[[.x]], "label"))

data_merged <- data_merged %>%
  mutate(across(c(FC03PPPC
                  , FC08PPPC
                  , FC01PPPC
                  , FC06PPPC
                  , FC04PPPC
                  , FC124PPPC
                  , FCTOTPPPC), as.numeric)) %>%
  mutate(
    Clothing_footwear = FC03PPPC / FCTOTPPPC,
    Comm_goods_services = FC08PPPC / FCTOTPPPC,
    Food_non_alc_services = FC01PPPC / FCTOTPPPC,
    Health_goods_services = FC06PPPC / FCTOTPPPC,
    Housing_utilities = FC04PPPC / FCTOTPPPC,
    Social_protection = FC124PPPC / FCTOTPPPC
  )

columns_to_pivot <- c("Clothing_footwear", "Comm_goods_services"
                      , "Food_non_alc_services", "Health_goods_services"
                      , "Housing_utilities", "Social_protection")

pie_data2 <- data_merged %>%
  pivot_longer(cols = all_of(columns_to_pivot)
               , names_to = "Spending_category"
               , values_to = "Percentage")

p6 <- ggplot(pie_data2, aes(x = Location, y = Percentage, fill = Spending_category)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c(wes_palette("Zissou1", n = length(unique(pie_data2$Spending_category)), type = "continuous"), "#D3D3D3")) + # Adding an extra color
  labs(title = "Consumer Spending Percentage by Selected Categories (2011-2019)",
       x = "Location",
       y = "Percentage") +
  theme_minimal() +
  facet_wrap(~ Year, scales = "free", labeller = label_both) +
  theme(strip.text = element_text(size = 8)) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.1)),
            position = position_fill(vjust = 0.5), size = 2)

ggsave(filename = here::here("Figures", "CONS_SPENDING.png"), plot = p6, width = 8, height = 6)

# Code for Location specific plots
# Get unique locations
locations <- unique(pie_data2$Location)

# Create a plot for each location
for (loc in locations) {
  # Filter data for the current location
  loc_data <- pie_data2 %>% filter(Location == loc)
  
  # Create the plot
  p8 <- ggplot(loc_data, aes(x = Year, y = Percentage, fill = Spending_category)) +
    geom_area(position = "fill") +
    scale_fill_manual(values = c(wes_palette("Zissou1", n = length(unique(pie_data2$Spending_category)), type = "continuous"), "#D3D3D3")) +
    labs(title = paste("Consumer Spending Percentage by Category in", loc, "(2011-2019)"),
         x = "Year",
         y = "Percentage",
         fill = "Spending Category") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(breaks = unique(loc_data$Year))
  
  # Save the plot with location-specific filename
  ggsave(
    filename = here::here("Figures", paste0("CONS_SPENDING_", gsub(" ", "_", loc), ".png")), 
    plot = p8, 
    width = 10, 
    height = 8
  )
  
  # Display the plot
  print(p8)
}


### Sectorial contribution to GVA, real, PPP-----
data_filtered  <- data_merged %>% 
  dplyr::select(starts_with("GVA")) %>% 
  dplyr::select(contains("PPC")) %>% colnames()
# [1] "GVAAPPPC"     "GVAGIR_UPPPC"
# [3] "GVAK_NPPPC"   "GVAB_FPPPC"  
# [5] "GVAO_QPPPC"   "GVATOTPPPC"  
# [7] "GVAHJPPPC"  

map(c("GVAAPPPC", "GVAGIR_UPPPC", "GVAK_NPPPC", "GVAB_FPPPC", "GVAO_QPPPC", "GVATOTPPPC", "GVAHJPPPC"), 
    ~ attr(data_merged[[.x]], "label"))

data_merged <- data_merged %>%
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

columns_to_pivot <- c("Agriculture_GVA_Pct", "Consumer_services_GVA_Pct",
                      "Financial_business_services_GVA_Pct", "Industry_GVA_Pct",
                      "Public_services_GVA_Pct")

pie_data <- data_merged %>%
  pivot_longer(cols = all_of(columns_to_pivot), names_to = "Sector", values_to = "Percentage")

# GVA pie chart
# ggplot(pie_data, aes(x = "", y = Percentage, fill = Sector)) +
#   geom_bar(stat = "identity", width = 1) +
#   coord_polar("y") +
#   scale_fill_manual(values = c(wes_palette("Zissou1"), "#D3D3D3")) + # Adding an extra color
#   theme_void() +
#   labs(title = "GVA Percentage by Sector")

p5 <-  ggplot(pie_data, aes(x = Location, y = Percentage, fill = Sector)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c(wes_palette("Zissou1", n = length(unique(pie_data$Sector)), type = "continuous"), "#D3D3D3")) + # Adding an extra color
  labs(title = "GVA Contribution by Sector (2011-2019)",
       x = "Location",
       y = "Percentage") +
  theme_minimal() +
  facet_wrap(~ Year, scales = "free", labeller = label_both) +
  theme(strip.text = element_text(size = 8)) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.1)),
            position = position_fill(vjust = 0.5), size = 2)

ggsave(filename = here::here("Figures", "GVA_SECTOR.png"), plot = p5, width = 8, height = 6)

# Code for Location specific plots
# Code for Location specific plots
# Get unique locations
locations <- unique(pie_data$Location)

# Create a plot for each location
for (loc in locations) {
  # Filter data for the current location
  loc_data <- pie_data %>% filter(Location == loc)
  
  # Get start and end years
  start_year <- min(loc_data$Year)
  end_year <- max(loc_data$Year)
  
  # Prepare label data
  label_data <- loc_data %>%
    group_by(Year) %>%
    mutate(
      pos = cumsum(Percentage) - 0.5 * Percentage,
      perc = scales::percent(Percentage, accuracy = 0.1)
    ) %>%
    ungroup()
  
  # Create the plot
  p9 <- ggplot(loc_data, aes(x = Year, y = Percentage, fill = Sector)) +
    geom_area(position = "fill") +
    scale_fill_manual(values = c(wes_palette("Zissou1", n = length(unique(pie_data$Sector)), type = "continuous"), "#D3D3D3")) +
    labs(title = paste("GVA Contribution by Sector in", loc, "(2011-2019)"),
         x = "Year",
         y = "Percentage",
         fill = "Sector") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(breaks = unique(loc_data$Year)) +
    geom_text(data = label_data %>% filter(Year == start_year | Year == end_year),
              aes(x = Year, y = pos, label = perc, group = Sector,
                  hjust = ifelse(Year == start_year, 1, 0)),
              size = 3) +
    geom_line(data = label_data %>% filter(Year == start_year | Year == end_year),
              aes(x = Year, y = pos, group = Sector),
              linetype = "dotted", color = "gray50")
  
  # Save the plot with location-specific filename
  ggsave(
    filename = here::here("Figures", paste0("GVA_SECTOR_", gsub(" ", "_", loc), ".png")), 
    plot = p9, 
    width = 12,
    height = 8
  )
  
}

# Only 2019
# Filter data for 2019
pie_data <- data_merged %>%
  pivot_longer(cols = all_of(columns_to_pivot), names_to = "Sector", values_to = "Percentage") %>% 
  filter(Year == 2019)

# Ensure percentages sum to 100 for each location
pie_data <- pie_data %>%
  group_by(Location) %>%
  mutate(Total = sum(Percentage)) %>%
  mutate(Percentage = Percentage / Total * 100) %>%
  ungroup()

# Create a single plot with all locations
p9 <- ggplot(pie_data, aes(x = Location, y = Percentage, fill = Sector)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(wes_palette("Zissou1", n = length(unique(pie_data$Sector)), type = "continuous"), "#D3D3D3")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), 
                     limits = c(0, 100),
                     breaks = seq(0, 100, 20)) +
  labs(title = "GVA Contribution by Sector Across Locations (2019)",
       x = NULL,
       y = "Percentage",
       fill = "Sector") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    legend.text = element_text(size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Add percentage labels
p9 <- p9 + geom_text(aes(label = ifelse(Percentage >= 5, paste0(round(Percentage, 1), "%"), "")), 
                     position = position_stack(vjust = 0.5), 
                     size = 3, color = "white")

# Save the plot
ggsave(
  filename = here::here("Figures", "GVA_SECTOR_ALL_LOCATIONS_2019_FINAL.png"), 
  plot = p9, 
  width = 15,
  height = 10,
  dpi = 300
)


### Productivity per worker for each of the sectors (in real usd) ----
# Define productivity as GVA per worker

data_merged <- data_merged %>%
  mutate(Agriculture_Productivity = GVAAPPPC / EMPA,
         Consumer_services_Productivity = GVAGIR_UPPPC / EMPGIR_U,
         Financial_business_services_Productivity = GVAK_NPPPC / EMPK_N,
         Industry_Productivity = GVAK_NPPPC / EMPB_F,          
         Public_services_Productivity = GVAB_FPPPC / EMPO_Q,
         Transport_Information_Communic_Services_Productivity = GVAHJPPPC / EMPHJ)

columns_to_pivot <- c("Agriculture_Productivity", "Consumer_services_Productivity",
                      "Financial_business_services_Productivity", "Industry_Productivity",
                      "Public_services_Productivity", "Transport_Information_Communic_Services_Productivity")

pie_data5 <- data_merged %>%
  pivot_longer(cols = all_of(columns_to_pivot), names_to = "Sector_productivity", values_to = "Productivity") %>% 
  filter(Year == 2019)

p18 <- ggplot(pie_data5, aes(x = Sector_productivity, y = Productivity, fill = Sector_productivity)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(pie_data5$Sector_productivity)), type = "continuous")) +
  labs(title = "Sector Productivity by City, 2019",
       x = "Sector",
       y = "Productivity") +
  theme_minimal() +
  facet_wrap(~ Location, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 8),
        legend.position = "none") +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label = scales::comma(Productivity, accuracy = 0.01)),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 2.5)

ggsave(filename = here::here("Figures", "SECTOR_PRODUCTIVITY_2019_FINAL.png"), plot = p18, width = 12, height = 8)

## By sector all years
pie_data6 <- data_merged %>%
  pivot_longer(cols = all_of(columns_to_pivot)
               , names_to = "Sector"
               , values_to = "Productivity") %>% 
  mutate(Sector = gsub("_Productivity", "", Sector))  # Clean up sector names

create_sector_plot <- function(data, sector) {
  # Filter data for the specific sector
  sector_data <- data %>% filter(Sector == sector)
  
  # Identify start and end years
  start_year <- min(sector_data$Year)
  end_year <- max(sector_data$Year)
  
  # Prepare label data
  label_data <- sector_data %>%
    group_by(Location) %>%
    summarize(
      Start_Year = first(Year),
      End_Year = last(Year),
      Start_Productivity = first(Productivity),
      End_Productivity = last(Productivity)
    )
  
  # Get color palette
  color_palette <- wes_palette("Zissou1", n = length(unique(sector_data$Location)), type = "continuous")
  
  p <- ggplot(sector_data, aes(x = Year, y = Productivity, color = Location, group = Location)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = color_palette) +
    geom_text_repel(
      data = label_data,
      aes(x = Start_Year, y = Start_Productivity, label = paste(Location, round(Start_Productivity, 2))),
      nudge_x = -0.5,
      direction = "y",
      hjust = 1,
      segment.color = "grey50"
    ) +
    geom_text_repel(
      data = label_data,
      aes(x = End_Year, y = End_Productivity, label = paste(Location, round(End_Productivity, 2))),
      nudge_x = 0.5,
      direction = "y",
      hjust = 0,
      segment.color = "grey50"
    ) +
    labs(title = paste(sector, "Productivity by City, 2011-2019"),
         x = "Year",
         y = "Productivity (GVA/Employment)") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"  # Remove legend as we now have labels
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = start_year:end_year)
  
  # Save the plot
  filename <- paste0(gsub(" ", "_", sector), "_FINAL.png")
  ggsave(filename = here::here("Figures", filename), 
         plot = p, 
         width = 15, 
         height = 10)
}

# Create and save a plot for each sector
unique_sectors <- unique(pie_data6$Sector)
lapply(unique_sectors, function(sector) create_sector_plot(pie_data6, sector))

### GVA per capita and GDP_per_capita----
City_GDP_share 
City_Employment_share 


create_population_plot(data_merged,
                       variable_name = "GVA_per_capita",
                       title = "GVA per capita (PPP) by Category and City (2011-2019)",
                       x_label = "Year",
                       y_label = "USD PPP (thousands)",
                       save_plot = TRUE,
                       filename = "GVA_PC_FINAL.png"
)

create_population_plot(data_merged,
                       variable_name = "City_GDP_share",
                       title = "City GDP share of National GDP by Category and City (2011-2019)",
                       x_label = "Year",
                       y_label = "USD PPP (thousands)",
                       save_plot = TRUE,
                       filename = "GDP_SHARE_FINAL.png"
)

### Stacked GDP for all cities, 2019

# create_stacked_bar_plot (data = data_merged
#                          , year = 2019
#                          , variable = "GDPTOTUSC"
#                          # , category_var
#                          , title = "GDP Distribution Across Cities (Real USD), 2019"
#                          , y_label = "Percentage of share of Total GDP"
#                          , output_filename = "CITY_GDP_2019_FINAL.png"
#                          )

# # Filter data for 2019 and calculate percentages
pie_data10 <- data_merged %>%
  filter(Year == 2019) %>%
  group_by(Location) %>%
  summarise(GDPTOTUSC = sum(GDPTOTUSC, na.rm = TRUE),
            category_var = first(category_var)) %>%  # Assume category_var is consistent per Location
  ungroup() %>%
  mutate(Total_GDP = sum(GDPTOTUSC, na.rm = TRUE),
         Percentage = GDPTOTUSC / Total_GDP * 100) %>%
  arrange(desc(Percentage))

# Create a single vertical stacked bar plot
p21 <- ggplot(pie_data10, aes(x = "", y = Percentage, fill = Location)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = wes_palette("Zissou1", n = nrow(pie_data10), type = "continuous")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 100),
                     breaks = seq(0, 100, 10)) +
  labs(title = "GDP Distribution Across Cities (Real USD), 2019",
       x = NULL,
       y = "Percentage of share of Total GDP",
       fill = "City") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  # Modify only the legend to group by category_var
  guides(fill = guide_legend(byrow = TRUE,
                             title = "City",
                             order = 1,
                             override.aes = list(size = 3))) +
  # Add a second legend for categories
  scale_color_manual(values = setNames(rep("black", nrow(pie_data10)), pie_data10$Location),
                     breaks = pie_data10$Location,
                     labels = pie_data10$category_var,
                     name = "Category") +
  guides(color = guide_legend(override.aes = list(fill = NA, color = NA),
                              order = 2))

# Add percentage labels using ggrepel
p21 <- p21 +
  geom_text_repel(
    aes(label = paste0(Location, "\n", round(Percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3,
    direction = "y",
    force = 1,
    segment.color = NA
  )

# Save the plot
ggsave(
  filename = here::here("Figures", "CITY_GDP_2019_FINAL.png"),
  plot = p21,
  width = 12,  # Increased width to accommodate the expanded legend
  height = 12,
  dpi = 300
)

### Stacked Total Employment for all cities, 2019----
# require(knitr))
# emptott_table <- pie_data11 %>%
#   dplyr::select(Location, EMPTOTT) %>%
#   arrange(desc(EMPTOTT))
# 
# # Format the EMPTOTT column
# emptott_table$EMPTOTT <- format(emptott_table$EMPTOTT, big.mark = ",")
# 
# # Add row numbers
# emptott_table <- emptott_table %>%
#   mutate(Rank = row_number()) %>%
# dplyr::select(Rank, everything())
# 
# | Rank|Location           |EMPTOTT |
#   |----:|:------------------|:-------|
#   |    1|Bangkok            |9,449.9 |
#   |    2|Mumbai             |7,776.8 |
#   |    3|Hangzhou, Zhejiang |5,761.5 |
#   |    4|Ho Chi Minh City   |5,072.3 |
#   |    5|Kuala Lumpur       |3,865.2 |
#   |    6|Singapore          |3,752.8 |
#   |    7|Surabaya           |3,481.8 |
#   |    8|Chittagong         |1,398.7 |
#   |    9|Kochi              |1,013.3 |
#   |   10|Colombo            |729.4   |
#   |   11|Qinhuangdao, Hebei |528.5   |

# kable(emptott_table, caption = "Total Employment by Location")

pie_data11 <- data_merged %>%
  filter(Year == 2019) %>%
  group_by(Location) %>%
  summarise(EMPTOTT = sum(EMPTOTT, na.rm = TRUE),
            category_var = first(category_var)) %>%  # Assume category_var is consistent per Location
  ungroup() %>%
  mutate(Total_Employment = sum(EMPTOTT, na.rm = TRUE),
         Percentage = EMPTOTT / Total_Employment * 100) %>%
  arrange(desc(Percentage))

# Create a single vertical stacked bar plot
p22 <- ggplot(pie_data11, aes(x = "", y = Percentage, fill = Location)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = wes_palette("Zissou1", n = nrow(pie_data11)
                                         , type = "continuous")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 100),
                     breaks = seq(0, 100, 10)) +
  labs(title = "Total Employment Distribution Across Cities (thousands), 2019",
       x = NULL,
       y = "Percentage of share of Total Employment",
       fill = "City") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  # Modify only the legend to group by category_var
  guides(fill = guide_legend(byrow = TRUE,
                             title = "City",
                             order = 1,
                             override.aes = list(size = 3))) +
  # Add a second legend for categories
  scale_color_manual(values = setNames(rep("black", nrow(pie_data11)), pie_data11$Location),
                     breaks = pie_data11$Location,
                     labels = pie_data11$category_var,
                     name = "Category") +
  guides(color = guide_legend(override.aes = list(fill = NA, color = NA),
                              order = 2))

# Add percentage labels using ggrepel
p22 <- p22 +
  geom_text_repel(
    aes(label = paste0(Location, "\n", round(Percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3,
    direction = "y",
    force = 1,
    segment.color = NA
  )

# Save the plot
ggsave(
  filename = here::here("Figures", "CITY_TOTAL_ENPLOYMENT_2019_FINAL.png"),
  plot = p22,
  width = 12,  # Increased width to accommodate the expanded legend
  height = 12,
  dpi = 300
)

### Private employment, 2019----
data_merged <- data_merged %>%
  mutate(Private_Emp = EMPTOTT - EMPO_Q) 

data_merged <- data_merged %>%
  group_by(Country, Year) %>%
  mutate(
    National_Private_Emp = ifelse(National_level == 1, Private_Emp, NA_real_)
  ) %>%
  mutate(
    National_Private_Emp = ifelse(is.na(National_Private_Emp)
                                  , first(National_Private_Emp[National_level == 1])
                          , National_Private_Emp)
  ) %>%
  ungroup() %>% 
  mutate(
    City_Private_Employment_share = ifelse(National_level == 0 & !is.na(National_Private_Emp), 
                            pmin(Private_Emp / National_Private_Emp, 1), 
                            NA_real_)
  )

# Prepare data for plotting
pie_data22 <- data_merged %>%
  filter(Year == 2019, National_level != 1) %>%
  dplyr::select(Location, City_Private_Employment_share) %>%
  rename(Percentage = City_Private_Employment_share)

# Create the plot
p32 <- ggplot(pie_data22, aes(x = reorder(Location, -Percentage)
                              , y = Percentage, fill = Location)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(pie_data22$Location))
                                         , type = "continuous")) +
  labs(title = "City Private Employment share of National Private Employment by City, 2019",
       x = "City",
       y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 8),
        legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.01)),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 2.5)

# Save the plot
ggsave(
  filename = here::here("Figures", "Share_Private_Employment_2019_FINAL.png"),
  plot = p32,
  width = 12,  # Increased width to accommodate the expanded legend
  height = 12,
  dpi = 300
)

### City proportion of National GDP and Employment, 2019----

# Data preparation
data_merged <- data_merged %>%
  group_by(Country, Year) %>%
  mutate(
    National_GDP = ifelse(National_level == 1, GDPTOTUSC, NA_real_),
    National_EMP = ifelse(National_level == 1, EMPTOTT, NA_real_)
  ) %>%
  mutate(
    National_GDP = ifelse(is.na(National_GDP), first(National_GDP[National_level == 1]), National_GDP),
    National_EMP = ifelse(is.na(National_EMP), first(National_EMP[National_level == 1]), National_EMP)
  ) %>%
  ungroup() %>% 
  mutate(
    City_GDP_share = ifelse(National_level == 0 & !is.na(National_GDP), 
                            pmin(GDPTOTUSC / National_GDP, 1), 
                            NA_real_),
    City_EMP_share = ifelse(National_level == 0 & !is.na(National_EMP), 
                            pmin(EMPTOTT / National_EMP, 1), 
                            NA_real_)
  )

# Prepare data for plotting
pie_data20 <- data_merged %>%
  filter(Year == 2019, National_level != 1) %>%
  dplyr::select(Location, City_GDP_share) %>%
  rename(Percentage = City_GDP_share)

# Create the plot
p30 <- ggplot(pie_data20, aes(x = reorder(Location, -Percentage)
                              , y = Percentage, fill = Location)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(pie_data20$Location))
                                         , type = "continuous")) +
  labs(title = "City GDP share of National GDP by City, 2019",
       x = "City",
       y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 8),
        legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.01)),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 2.5)

# Save the plot
ggsave(filename = here::here("Figures", "Share_GDP_2019_FINAL.png"),
       plot = p30, width = 12, height = 8)

pie_data21 <- data_merged %>%
  filter(Year == 2019, National_level != 1) %>%
  dplyr::select(Location, City_EMP_share) %>%
  rename(Percentage = City_EMP_share)

# Create the plot
p31 <- ggplot(pie_data21, aes(x = reorder(Location, -Percentage)
                              , y = Percentage, fill = Location)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(pie_data21$Location))
                                         , type = "continuous")) +
  labs(title = "City Employment share of National GDP by City, 2019",
       x = "City",
       y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 8),
        legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.01)),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 2.5)

# Save the plot
ggsave(filename = here::here("Figures", "Share_Employment_2019_FINAL.png"),
       plot = p31, width = 12, height = 8)

### National employment by sector, 2019----
# Only Colombo, raw total
data_merged <- data_merged %>%
  mutate(
    Public_Services_Emp = EMPO_Q,
    Industry_Emp = EMPB_F,
    Financial_Busines_Serices_Emp = EMPK_N,
    Consumer_services_Emp = EMPGIR_U,
    Agriculture_Emp = EMPA,
    Transport_Information_Communic_Services_Emp = EMPHJ
  )

data_merged <- data_merged %>%
  group_by(Country, Year) %>%
  mutate(
    across(c(Public_Services_Emp, Industry_Emp, Financial_Busines_Serices_Emp, 
             Consumer_services_Emp),
           list(National = ~ifelse(National_level == 1, ., NA_real_)),
           .names = "National_{.col}")
  ) %>%
  mutate(
    across(starts_with("National_"),
           ~ifelse(is.na(.), first(.[National_level == 1]), .))
  ) %>%
  ungroup() %>%
  mutate(
    across(c(Public_Services_Emp, Industry_Emp, Financial_Busines_Serices_Emp, 
             Consumer_services_Emp),
           list(City_share = ~ifelse(National_level == 0 & !is.na(get(paste0("National_", cur_column()))), 
                                     pmin(. / get(paste0("National_", cur_column())), 1), 
                                     NA_real_)),
           .names = "City_{.col}_share")
  )

pie_data23 <- data_merged %>%
  filter(Year == 2019, National_level != 1) %>%
  dplyr::select(
    Location,
    City_Public_Services_Emp_share,
    City_Industry_Emp_share,
    City_Financial_Busines_Serices_Emp_share,
    City_Consumer_services_Emp_share
  ) %>%
  pivot_longer(
    cols = starts_with("City_"),
    names_to = "Employment_Category",
    values_to = "Percentage"
  ) %>%
  mutate(
    Employment_Category = case_when(
      Employment_Category == "City_Public_Services_Emp_share" ~ "Public Services",
      Employment_Category == "City_Industry_Emp_share" ~ "Industry",
      Employment_Category == "City_Financial_Busines_Serices_Emp_share" ~ "Financial & Business Services",
      Employment_Category == "City_Consumer_services_Emp_share" ~ "Consumer Services"
    )
  )

# Create individual plots for each Employment_Category
employment_categories <- unique(pie_data23$Employment_Category)

for (category in employment_categories) {
  p_category <- pie_data23 %>%
    filter(Employment_Category == category) %>%
    ggplot(aes(x = reorder(Location, -Percentage), y = Percentage, fill = Location)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(pie_data23$Location)), type = "continuous")) +
    labs(title = paste("City Employment Share of", category, " of National", category ,", 2019"),
         x = "City", y = "Percentage") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 8),
          legend.position = "none") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
    geom_text(aes(label = scales::percent(Percentage, accuracy = 0.01)),
              position = position_dodge(width = 0.9), 
              vjust = -0.5, 
              size = 2.5)
  
  ggsave(filename = here::here("Figures", paste0("Share_", gsub(" ", "_", category)
                                                 , "_Employment_2019_FINAL.png")),
         plot = p_category, width = 12, height = 8)
}

# Output database -----
require(lubridate)

today_date <- format(Sys.Date(), "%m-%d-%Y")
file_name <- paste0("Figures_data_", today_date, ".csv")

# Select the specified columns
# selected_columns <- c("Locationcode", "Location", "Country", "Year", "category_var", "category_group",
#                         "Total_population",
#                         "GDP_per_capita_PPP",
#                         "Total_Employment_Pct",
#                         "Avg_Household_Income_PPP",
#                         "High_Skills_Employment_Pct",
#                         "Clothing_footwear",
#                         "Comm_goods_services",
#                         "Food_non_alc_services",
#                         "Health_goods_services",
#                         "Housing_utilities",
#                         "Social_protection",
#                         "Agriculture_GVA_Pct",
#                         "Consumer_services_GVA_Pct",
#                         "Financial_business_services_GVA_Pct",
#                         "Industry_GVA_Pct",
#                         "Public_services_GVA_Pct",
#                         "Transport_Information_Communic_Services_GVA_Pct",
#                         "Agriculture_Productivity",
#                         "Consumer_services_Productivity",
#                         "Financial_business_services_Productivity",
#                         "Industry_Productivity",
#                         "Public_services_Productivity",
#                         "Transport_Information_Communic_Services_Productivity",
#                         "GVA_per_capita",
#                         "Private_Emp",
#                         "Public_Services_Emp_Pct",
#                         "Industry_Emp_Pct",
#                         "Financial_Busines_Serices_Emp_Pct",
#                         "Consumer_services_Emp_Pct",
#                         "Agriculture_Emp_Pct",
#                         "Transport_Information_Communic_Services_Emp_Pct"
#                       )

data_merged <- data_merged %>%
  mutate(Total_Employment = EMPTOTT,
         Total_GVA = GVATOTPPPC,
         Total_GDP = GDPTOTUSC)

selected_columns <- c("Location", "Country", "Year", "category_var",
                      "Total_Employment",
                      "Public_Services_Emp_Pct",
                      "Industry_Emp_Pct",
                      "Financial_Busines_Serices_Emp_Pct",
                      "Consumer_services_Emp_Pct",
                      "Agriculture_Emp_Pct",
                      "Transport_Information_Communic_Services_Emp_Pct",
                      "Total_GVA",
                      "Agriculture_GVA_Pct",
                      "Consumer_services_GVA_Pct",
                      "Financial_business_services_GVA_Pct",
                      "Public_services_GVA_Pct",
                      "Transport_Information_Communic_Services_GVA_Pct",
                      "Agriculture_Productivity",
                      "Consumer_services_Productivity",
                      "Financial_business_services_Productivity",
                      "Industry_Productivity",
                      "Public_services_Productivity",
                      "Transport_Information_Communic_Services_Productivity",
                      "Total_GDP",
                      "GDP_per_capita_PPP",
                      "GVA_per_capita",
                      "POPTOTT",
                      "EMPTOTT",
                      "EMPO_Q",
                      "EMPB_F",
                      "EMPK_N",
                      "EMPGIR_U",
                      "EMPA",
                      "EMPHJ",
                      "GVATOTPPPC",
                      "GVAAPPPC",
                      "GVAGIR_UPPPC",
                      "GVAK_NPPPC",
                      "GVAB_FPPPC",
                      "GVAO_QPPPC",
                      "GVAHJPPPC",
                      "GDPTOTUSC",
                      "GDPTOTPPPC")

## Export database, panel data
write.csv(data_merged[selected_columns], here("Output",file_name), row.names = FALSE)

## Export database, time series format

reshaped_data <- data_merged[selected_columns] %>%
  pivot_longer(
    cols = -c(Location, Country, category_var, Year),
    names_to = "variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = Year,
    values_from = value
  )

## Export database, time series
file_name <- paste0("Figures_data_ts_", today_date, ".csv")
write.csv(reshaped_data, here("Output",file_name), row.names = FALSE)


