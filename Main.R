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

## Program Set-up ------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation

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
         , "prettymapr", "viridis"
)

groundhog.library(pkgs, groundhog.day, ignore.deps = c("stringi", "fs"))

## Runs the following --------


## Upload data -------

data <- read_dta(here("data","OE_GC_2021.dta"))

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

fua <- projectRaster(fua, crs = st_crs(4326)$proj4string)
crs(fua)  # +proj=wgs84

saveRDS(fua, here("Data", "Raster", "fua.rds"))


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
  
locations <- c("Colombo", "Sri Lanka", "Phnom Penh", "Cambodia", "Ho Chi Minh City", "Vietnam - Total",
               "Surabaya", "Indonesia", "Chittagong", "Port Louis", "Mauritius", "Auckland", "New Zealand - Total")
cities <- c("Colombo", "Phnom Penh", "Ho Chi Minh City", "Surabaya", "Chittagong", "Port Louis")


data <- data %>% filter(Location %in% locations) %>% 
                 filter(Location %in% cities | Location == "") %>% 
                 filter(Year == 2019 | Year == "")

# Select columns
columns_to_keep <- c("Year", "Location", "PEDYHHPUSN", "FCTOTPPPC", "FCTOTUSN", "FCTOTUSC", "EMPA", "EMPGIR_U",
                     "EMPK_N", "EMPB_F", "EMPO_Q", "EMPTOTT", "EMPHJ", "GDPTOTUSC", "GDPTOTPPPN", "GVAAUSN",
                     "GVAGIR_UUSN", "GVAK_NUSN", "GVAB_FUSN", "GVAO_QUSN", "GVATOTUSN", "GVAHJUSN", "POPTOTT")
data <- data %>% dplyr::select(all_of(columns_to_keep))

# Cleaning
numeric_columns <- c("PEDYHHPUSN", "FCTOTPPPC", "FCTOTUSN", "FCTOTUSC", "EMPA", "EMPGIR_U", "EMPK_N", "EMPB_F",
                     "EMPO_Q", "EMPTOTT", "EMPHJ", "GDPTOTUSC", "GDPTOTPPPN", "GVAAUSN", "GVAGIR_UUSN", "GVAK_NUSN",
                     "GVAB_FUSN", "GVAO_QUSN", "GVATOTUSN", "GVAHJUSN")

data <- data %>% 
  filter(Location != "") %>% 
  mutate(across(everything(), ~na_if(., "NA"))) %>% 
  mutate(across(all_of(numeric_columns), as.numeric)) %>% 
  mutate(category_var = case_when(
    Location == "Colombo" ~ 1,
    Location %in% c("Phnom Penh", "Ho Chi Minh City", "Surabaya", "Chittagong") ~ 2,
    Location %in% c("Port Louis", "Auckland", "Lisbon", "Singapore") ~ 3
  ))

# Remove duplicates
data <- distinct(data)

# Label category variable
data$category_var <- factor(data$category_var, 
                            levels = 1:3, 
                            labels = c("City", "DIRECT COMPARATORS", "ASPIRATIONAL COMPARATORS"))

# Remove specific locations
data <- data %>% filter(!(Location %in% c("Indonesia", "Cambodia", "Sri Lanka", "Mauritius")))

# Create category group
data <- data %>% group_by(category_var) %>% mutate(category_group = cur_group_id())


## Visualisation -------
# Create bar plot
(ggplot(data, aes(x = reorder(Location, PEDYHHPUSN), y = PEDYHHPUSN, fill = category_var)) +
  geom_bar(stat = "identity") +
  facet_grid(~ category_group, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = c("green", "blue", "red")) +
  labs(title = "Average Household Personal Disposable Income",
       y = "$US, 2015, Nominal",
       x = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(PEDYHHPUSN, 0)), vjust = -0.5))

# Table of category_var
table(data$category_var)

# Convert category_var to integer
data$category_int <- as.integer(data$category_var)

# Time series graphs
data$id_numeric <- as.numeric(factor(data$Location))
data$Year <- as.numeric(data$Year)

# Set up panel data
data_panel <- data %>% arrange(id_numeric, Year) %>% 
                 filter(!is.na(Year) & !is.na(EMPTOTT))

### Total Employment-----

# ggplot(data, aes(x = Year, y = EMPTOTT, group = Location, color = Location)) +
#   geom_line() +
#   labs(title = "Total Employment",
#        x = "Year",
#        y = "Total Employment") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 5)) +
#   scale_y_continuous(n.breaks = 4)

# Order of locations based on their last data point
location_order <- data %>%
  group_by(Location) %>%
  summarise(TotalEmp = sum(EMPTOTT, na.rm = TRUE)) %>%
  arrange(desc(TotalEmp)) %>%
  pull(Location)

(ggplot(data, aes(x = Year, y = EMPTOTT, color = factor(Location, levels = location_order))) +
  geom_point(size = 3) +
  labs(title = "Total Employment",
       x = "Year",
       y = "Total Employment",
       color = "Location") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 5)) +
  scale_y_continuous(n.breaks = 4, labels = scales::comma) +
  theme(legend.position = "right"))

### Average total employment-----
avg_data <- data %>% 
  group_by(Year) %>% 
  summarise(EMPTOTT = mean(EMPTOTT, na.rm = TRUE))

# Line plot for Average Total Employment
ggplot(avg_data, aes(x = Year, y = EMPTOTT)) +
  geom_line() +
  labs(title = "Average Total Employment Over Time",
       x = "Year",
       y = "Average Total Employment") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2040, by = 5)) +
  scale_y_continuous(breaks = seq(0, 8000, by = 2000))

### Total GDP-----
(ggplot(data, aes(x = Year, y = GDPTOTUSC, color = Location)) +
  geom_point(size = 3) +
  labs(title = "Total GDP",
       x = "Year",
       y = "Total GDP") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 5)) +
  scale_y_continuous(n.breaks = 4))