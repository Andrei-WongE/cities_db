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

data <- data[-c(1:7), ]

# Load shapefile
shape_file <- st_read(here("data", "OE_FUA_SHAPEFILE", "OE_FUA_SHAPEFILE.shp")) %>% 
  filter(OE_COUNTRY=="LKA")

View(shape_file)

require(ggspatial)

ggplot(data = shape_file) +
  annotation_map_tile(type = "osm", zoom = 10) +  # Add OpenStreetMap tiles
  geom_sf(fill = NA, color = "blue", size = 3.5) +  # Make the shape file transparent  theme_minimal()
  labs(title = "eFUA Colombo",
       subtitle = "Colombo, Sri Lanka",
       caption = "Source: Oxford Economics 2022") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering)  +
  annotation_scale(location = "bl", width_hint = 0.5)

ggsave(here("Figures","colombo_FUA_OE.png"), width = 6, height = 4, dpi = 300)


# Load raster files
fua <- raster(here("Data", "Raster","ghs_smod_fua.tif"))
oe <- raster(here("Data", "Raster", "ghs_smod_oe.tif"))
ucdb <- raster(here("Data", "Raster","ghs_smod_ucdb.tif"))

crs(fua)  # +proj=moll
# values <- getValues(fua)

minValue(fua)
maxValue(fua)
cellStats(fua, stat = 'mean')
#plot(fua, main = "GHS SMOD FUA")

# Reproject raster to WGS 84
# raster_files <- list(fua, oe, ucdb)
# 
# reprojected_rasters <- lapply(raster_files, function(raster) {
#   projectRaster(raster, crs = st_crs(4326)$proj4string)
# })
# names(reprojected_rasters) <- c("fua", "oe", "ucdb")

fua <- projectRaster(ucdb, crs = st_crs(4326)$proj4string)
# Warning message:
#   # [project] 294 failed transformations 
crs(ucdb)  # +proj=wgs84

# saveRDS(ucdb, here("Data", "Raster", "ucdb.rds"))


# Get bounding box, crop and mask raster
colombo_bbox <- st_bbox(c(xmin = 79.84, ymin = 6.80, xmax = 79.92, ymax = 6.98), crs = st_crs(4326))

colombo_extent <- as(extent(colombo_bbox), "SpatialPolygons")
crs(colombo_extent) <- crs(fua)  # Ensure the CRS matches

fua_colombo <- crop(fua, colombo_extent) %>% mask(colombo_extent)
oe_colombo <- crop(oe, colombo_extent) %>% mask(colombo_extent)
ucdb_colombo <- crop(ucdb, colombo_extent) %>% mask(colombo_extent)

# Function to convert raster to dataframe
raster_to_df <- function(raster, name) {
  df <- as.data.frame(raster, xy = TRUE)
  names(df)[3] <- "value"
  df$dataset <- name
  return(df)
}

# Convert rasters to dataframes
fua_df <- raster_to_df(fua_colombo, "FUA")
oe_df <- raster_to_df(oe_colombo, "OE")
ucdb_df <- raster_to_df(ucdb_colombo, "UCDB")

# Combine dataframes
all_data <- bind_rows(fua_df, oe_df, ucdb_df)

# Create the plot
(ggplot() +
  annotation_map_tile(type = "osm", zoom = 12) +  # Add OpenStreetMap tiles
  geom_raster(data = all_data, aes(x = x, y = y, fill = value), alpha = 0.7) +
  facet_wrap(~ dataset, ncol = 3) +
  scale_fill_viridis_c(option = "plasma") +
  coord_sf(crs = st_crs(4326)) +
  theme_minimal() +
  labs(title = "Colombo Urban Area Analysis",
       fill = "Value") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(axis.title = element_blank(),
        legend.position = "bottom"))

# Save the plot
ggsave(here("Figures", "colombo_urban_analysis_with_osm.png"), width = 15, height = 5, dpi = 300)

# Filter locations and years
# country <- data %>%
#            filter("Country" %in% "Sri Lanka")

colnames(data)
require(labelled)

locations <- c("Colombo", "Sri Lanka", "Phnom Penh", "Cambodia", "Ho Chi Minh City", "Vietnam - Total",
               "Surabaya", "Indonesia", "Chittagong", "Port Louis", "Mauritius", "Auckland", "New Zealand - Total")

cities <- c("Colombo", "Phnom Penh", "Ho Chi Minh City", "Surabaya", "Chittagong", "Port Louis")
mena <- c("Cairo", "Riyadh", "Dubai", "Tehran", "Istanbul", "Baghdad", "Casablanca", "Doha")


data_mena <- data %>% 
             filter(Location %in% mena | Location == "")


# Select columns
# columns_to_keep <- c("Year", "Location", "PEDYHHPUSN", "FCTOTPPPC", "FCTOTUSN", "FCTOTUSC", "EMPA", "EMPGIR_U",
#                      "EMPK_N", "EMPB_F", "EMPO_Q", "EMPTOTT", "EMPHJ", "GDPTOTUSC", "GDPTOTPPPN", "GVAAUSN",
#                      "GVAGIR_UUSN", "GVAK_NUSN", "GVAB_FUSN", "GVAO_QUSN", "GVATOTUSN", "GVAHJUSN", "POPTOTT")
# data <- data %>% dplyr::select(all_of(columns_to_keep))

# 
# data <- data %>% 
#   filter(Location != "") %>% 
#   mutate(across(everything(), ~na_if(., "NA"))) %>% 
#   mutate(across(all_of(numeric_columns), as.numeric)) %>% 
#   mutate(category_var = case_when(
#     Location == "Colombo" ~ 1,
#     Location %in% c("Phnom Penh", "Ho Chi Minh City", "Surabaya", "Chittagong") ~ 2,
#     Location %in% c("Port Louis", "Auckland", "Lisbon", "Singapore") ~ 3
#   ))

## Visualisation -------
# Create bar plot
# (ggplot(data, aes(x = reorder(Location, PEDYHHPUSN), y = PEDYHHPUSN, fill = category_var)) +
#   geom_bar(stat = "identity") +
#   facet_grid(~ category_group, scales = "free_x", space = "free_x") +
#   scale_fill_manual(values = c("green", "blue", "red")) +
#   labs(title = "Average Household Personal Disposable Income",
#        y = "$US, 2015, Nominal",
#        x = "Location") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   geom_text(aes(label = round(PEDYHHPUSN, 0)), vjust = -0.5))

#### Request ----
require(readr)

show_in_excel <- function(.data){

tmp <- paste0(tempfile(), ".csv")

write_xlsx(.data, tmp)

#fs:: file_show(path = tmp)
browseURL(tmp) 
}


data %>% filter(Country=="Sri Lanka") %>% View(.)

colnames(data) <- labels_vector[colnames(data)]

data_colombo <- data %>% filter(Location=="Colombo") %>% 
  dplyr::select(where(~ !all(is.na(.)))) 

show_in_excel(data_colombo)

labels_vector <- setNames(labels$Label, labels$Variable)
data_colombo_labeled <- setNames(data_colombo, labels_vector[names(data_colombo)])

write_xlsx(data_colombo_labeled, here("Data", "data_colombo.xlsx"), col_names = TRUE)



data %>% filter(Location=="Colombo") %>% 
  dplyr::select(where(~ !all(is.na(.)))) %>%
  colnames(.)


qs_rankings <- read_csv(here("Data", "qs_rankings_CCKSB.csv"))
colnames(qs_rankings)[colnames(qs_rankings) == "...1"] <- "id"

data_colombo2 <- qs_rankings %>% filter(location=="Sri Lanka") %>% 
  dplyr::select(where(~ !all(is.na(.)))) 

show_in_excel(data_colombo2)


### wb_doingbusiness----
wb_doingbusiness <- read_csv(here("Data", "WB_doingbusiness_CCKSB.csv")) %>% 
  rename(Location = `Location 1\r\n(city, port)`)

data_colombo3 <- wb_doingbusiness %>% filter(Economy=="Sri Lanka") %>% 
  dplyr::select(where(~ !all(is.na(.)))) 

show_in_excel(data_colombo3)


cities <- c("Colombo", "Mumbai", "Chennai", "Kochi", "Chittagong", "Karachi", "Ho Chi Minh City", "Surabaya",
               "Singapore", "Bangkok", "Kuala Lumpur", "Hangzhou", "Quingdao", "Tianjin", "Xiamen")

data_wbdb <- wb_doingbusiness %>% filter(Location %in% cities) 



### UCDB----
ucdb_full_vars <- read_csv(here("Data","UCDB_CCKSB_full_vars.csv")) %>% 
  rename(Location = loc_latin)

data_colombo4 <- ucdb_full_vars %>% filter(CTR_MN_NM=="Sri Lanka") %>% 
  dplyr::select(where(~ !all(is.na(.)))) %>% 
  filter(loc_latin != "Sammanturai") # Row contains all missing data


show_in_excel(data_colombo4)

data_ucdb <- ucdb_full_vars %>% filter(Location %in% cities) 

# Statistics -----
# labels_vector <- setNames(labels$Label, labels$Variable)
# data_merged <- setNames(data_merged, labels_vector[names(data_merged)])

# Filter the data for the required years (2011-2019)
data_filtered <- data %>%
  filter(Year >= 2011 & Year <= 2019)

# Extract PPP data from the World Bank API
ppp_data <- WDI(indicator = "PA.NUS.PPP", start = 2011, end = 2019, extra = TRUE) %>% 
  mutate(year = as.character(year))

# Merge the PPP data with the cities data
data_merged <- data_filtered %>%
  left_join(ppp_data, by = c("Location" = "capital", "Year" = "year"))

# Get all variable labels
var_label(data_merged) %>% View()

# Filer for location and create groups
numeric_columns <- c("PEDYHHPUSN", "FCTOTPPPC", "FCTOTUSN", "FCTOTUSC", "EMPA", "EMPGIR_U", "EMPK_N", "EMPB_F",
                     "EMPO_Q", "EMPTOTT", "EMPHJ", "GDPTOTUSC", "GDPTOTPPPN", "GVAAUSN", "GVAGIR_UUSN", "GVAK_NUSN",
                     "GVAB_FUSN", "GVAO_QUSN", "GVATOTUSN", "GVAHJUSN")

# locations <- c("Colombo", "Mumbai", "Chennai", "Kochi", "Chittagong", "Karachi", "Ho Chi Minh City", "Surabaya",
#                "Singapore", "Bangkok", "Kuala Lumpur", "Hangzhou", "Quingdao", "Tianjin", "Xiamen")

locations <- c("Colombo", "Mumbai", "Kochi", "Chittagong", "Ho Chi Minh City", "Surabaya",
               "Singapore", "Bangkok", "Kuala Lumpur", "Hangzhou", "Quingdao")


data_merged <- data_merged %>% 
  filter(Location != "") %>% 
  # mutate(across(everything(), ~na_if(., "NA"))) %>% 
  mutate(across(all_of(numeric_columns), as.numeric)) %>% 
  mutate(category_var = case_when(
    Location %in% c("Colombo") ~ 1,
    Location %in% c("Mumbai", "Kochi", "Chittagong", "Ho Chi Minh City", "Surabaya") ~ 2,
    Location %in% c("Singapore", "Bangkok", "Kuala Lumpur", "Hangzhou", "Quingdao") ~ 3
  )) %>% 
  filter(Location %in% locations| Location == "") %>% 
  filter(Locationcode != "SGP") #2 sets of Singapore data!!! Kept SGP2

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
                      "Fiancial_Busines_Serices_Emp_Pct",
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
    filename = here::here("Figures", paste0("EMP_SECTOR_", gsub(" ", "_", loc),"2019_FINAL" , ".png")), 
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
    filename = here::here("Figures", paste0("EMP_SECTOR_", gsub(" ", "_", loc), "_2019_FINAL.png")), 
    plot = p11, 
    width = 14,
    height = 10
  )
  
  print(p11)
}

# Only Colombo, stacket chart
# Colombo_data <- data_merged %>%
#   filter(Location == "Colombo")
# 
# pie_data7 <- Colombo_data %>%
#   pivot_longer(cols = all_of(columns_to_pivot_Emp_Pct)
#                , names_to = "Employment_category"
#                , values_to = "Percentage") %>% 
#   mutate(Employment_category = factor(Employment_category, levels = columns_to_pivot_Emp_Pct))
# 
# # Function to generate colors
# generate_colors <- function(n_categories, palette_name = "Zissou1") {
#   base_colors <- wes_palette(palette_name, n = n_categories, type = "continuous")
#   if (length(base_colors) < n_categories) {
#     color_function <- colorRampPalette(base_colors)
#     base_colors <- color_function(n_categories)
#   }
#   return(base_colors)
# }

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
p18 <- create_structure_plot(pie_data7, here::here("Figures", "EMP_WA_POP_COLOMBO_FINAL.png"))

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

ggsave(filename = here::here("Figures", "EMP_WA_POP_COLOMBO_ABSOLUTE_FINAL.png"), plot = p20, width = 8, height = 6)

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
  mutate(High_Skills_Employment_Pct = (EMPB_F + EMPK_N) / EMPTOTT * 100) # Industry + finance

create_population_plot(data_merged,
                       variable_name = "High_Skills_Employment_Pct",
                       title = "Proportion of Industry and Finance Sector Enployment by Category and City (2011-2019)",
                       x_label = "Year",
                       y_label = "High Skill Employment (percentaje)",
                       save_plot = TRUE,
                       filename = "HIGH_SKILLS_EMP.png"
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
  filename = here::here("Figures", "GVA_SECTOR_ALL_LOCATIONS_2019_FIBNA.png"), 
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




# Output database -----
require(lubridate)

today_date <- format(Sys.Date(), "%m-%d-%Y")
file_name <- paste0("Figures_data_", today_date, ".csv")

# Select the specified columns
selected_columns <- c("Locationcode", "Location", "Country", "Year", "category_var", "category_group", 
                      "Total_population", "Public_Services_Emp_Pct", "Industry_Emp_Pct", "Fiancial_Busines_Serices_Emp_Pct",
                      "Consumer_services_Emp_Pct", "Agriculture_Emp_Pct", "Transport_Information_Communic_Services_Emp_Pct",
                      "GDP_per_capita_PPP", "Agriculture_GVA_Pct", "Consumer_services_GVA_Pct", "Financial_business_services_GVA_Pct",
                      "Industry_GVA_Pct", "Public_services_GVA_Pct", "Transport_Information_Communic_Services_GVA_Pct", 
                      "Agriculture_Productivity", "Consumer_services_Productivity", "Financial_business_services_Productivity", 
                      "Industry_Productivity", "Public_services_Productivity", "Transport_Information_Communic_Services_Productivity")

write.csv(data_merged[selected_columns], file_name, row.names = FALSE)

# Write the CSV file
write.csv(data_merged, file_name, row.names = FALSE)

