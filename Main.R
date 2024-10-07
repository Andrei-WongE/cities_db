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


data <- data %>% filter(Location %in% locations) %>% 
                 filter(Location %in% cities | Location == "") 
#%>% 
                 # filter(Year == 2019 | Year == "")

# Select columns
# columns_to_keep <- c("Year", "Location", "PEDYHHPUSN", "FCTOTPPPC", "FCTOTUSN", "FCTOTUSC", "EMPA", "EMPGIR_U",
#                      "EMPK_N", "EMPB_F", "EMPO_Q", "EMPTOTT", "EMPHJ", "GDPTOTUSC", "GDPTOTPPPN", "GVAAUSN",
#                      "GVAGIR_UUSN", "GVAK_NUSN", "GVAB_FUSN", "GVAO_QUSN", "GVATOTUSN", "GVAHJUSN", "POPTOTT")
# data <- data %>% dplyr::select(all_of(columns_to_keep))

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



wb_doingbusiness <- read_csv(here("Data","WB_doingbusiness_CCKSB.csv"))

data_colombo3 <- wb_doingbusiness %>% filter(Economy=="Sri Lanka") %>% 
  dplyr::select(where(~ !all(is.na(.)))) 

show_in_excel(data_colombo3)


ucdb_full_vars <- read_csv(here("Data","UCDB_CCKSB_full_vars.csv"))
data_colombo4 <- ucdb_full_vars %>% filter(CTR_MN_NM=="Sri Lanka") %>% 
  dplyr::select(where(~ !all(is.na(.)))) %>% 
  filter(loc_latin != "Sammanturai") # Row contains all missing data


show_in_excel(data_colombo4)


# Statistics -----
# labels_vector <- setNames(labels$Label, labels$Variable)
# data_merged <- setNames(data_merged, labels_vector[names(data_merged)])

# Filter the data for the required years (2011-2022)
data_filtered <- data %>%
  filter(Year >= 2011 & Year <= 2022) %>% 
  mutate(Year = as.character(Year))

# Extract PPP data from the World Bank API
ppp_data <- WDI(indicator = "PA.NUS.PPP", start = 2011, end = 2022, extra = TRUE) %>% 
  mutate(year = as.character(year))

# Merge the PPP data with the cities data
data_merged <- data_filtered %>%
  left_join(ppp_data, by = c("Location" = "capital", "Year" = "year"))

# Get all variable labels
var_label(data_merged) %>% View()

# Table of category_var
table(data_merged$category_var)

# Convert category_var to integer
data_merged$category_int <- as.integer(data_merged$category_var)

# Convert Year to numeric
data_merged$Year <- as.numeric(as.character(data_merged$Year))

### Total Population-----
require(scales)

data_merged <- data_merged %>%
  mutate(POPTOTT = as.numeric(POPTOTT)) %>%
  mutate(Total_population = POPTOTT)  
  
# Order of locations based on their last data point
location_order <- data_merged %>%
  group_by(Location) %>%
  summarize(last_value = last(Total_population)) %>%
  arrange(desc(last_value)) %>%
  pull(Location)

data_merged$Location <- factor(data_merged$Location, levels = location_order)

# Last values based on combination of Location and category_var
last_values <- data_merged %>%
  group_by(Location, category_var) %>%
  slice_max(Year) %>%
  ungroup() %>%
  arrange(desc(Total_population))

interaction_levels <- interaction(last_values$Location, last_values$category_var, drop = TRUE)

p7 <- ggplot(data_merged, aes(x = Year,
                              y = Total_population,
                              color = interaction(Location, category_var),
                              group = interaction(Location, category_var))) +
  geom_line(size = 1.5) +
  geom_point() +
  scale_color_manual(values = wes_palette("Zissou1", n = length(interaction_levels), type = "continuous"),
                     breaks = interaction_levels,
                     labels = levels(interaction_levels)) +
  labs(title = "Total Population by Category and City (2011-2022)",
       x = "Year",
       y = "Total Population (thousands)") +
  theme_minimal() 

# Labels for ggrepel
label_data <- data_merged %>%
  group_by(Location, category_var) %>%
  slice_max(Year) %>%
  ungroup()

p7 <- p7 +
  geom_text_repel(data = label_data,
                  aes(label = paste0(Location, "-", category_var, "\n", 
                                     comma(round(Total_population), accuracy = 1))),
                  nudge_x = 1,  # Adjust this value to move labels horizontally
                  direction = "y",
                  hjust = 0,
                  segment.size = 0.2,
                  segment.color = "grey50",
                  box.padding = 0.5,
                  point.padding = 0.5,
                  force = 2,
                  size = 3) +  # Adjust text size if needed
  theme(legend.position = "none") +  # Remove legend as labels are now on the plot
  scale_x_continuous(limits = c(min(data_merged$Year), max(data_merged$Year) + 1))  # Extend x-axis for labels

ggsave(filename = here("Figures", "TOT_POP.png"), plot = p7, width = 10, height = 8)

### Total Employment-----
create_population_plot(data_merged,
                       variable_name = "EMPTOTT",
                       title = "Total Employment by Category and City",
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
#   scale_x_continuous(limits = c(2011, 2022), breaks = seq(2011, 2022, by = 1)) +
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
                       title = "GDP per capita PPP by Category and City",
                       x_label = "Year",
                       y_label = "GDP per capita PPP (thousands)",
                       save_plot = TRUE,
                       filename = "R_GDP_PPP.png"
)


# ggplot(data_merged, aes(x = Location, y = GDP_per_capita_PPP, fill = category_var)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(data_merged$category_var)), type = "continuous")) +
#   labs(title = "GDP per Capita PPP by Category and City (2011-2022)",
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
#   labs(title = "Total Employment by Category and City (2011-2022)",
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

### Total Employment as a Pct of the working-age population-----
data_merged <- data_merged %>%
  mutate(POPTOTT = as.numeric(POPTOTT),
         EMPTOTT = as.numeric(EMPTOTT),
         POP0_14T = as.numeric(POP0_14T),
         POP65_T = as.numeric(POP65_T),) %>%
  mutate(Total_Employment_Pct = (EMPTOTT / (POPTOTT-POP0_14T-POP65_T) * 100))

# ggplot(data_merged, aes(x = Location, y = Total_Employment_Pct, fill = category_var)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(data_merged$category_var)), type = "continuous")) +
#   labs(title = "Total Employment Percentage of Working-Age pop by Category and City",
#        x = "City",
#        y = "Total Employment Percentage") +
#   theme_minimal()

p2 <- ggplot(data_merged, aes(x = Year
                             , y = Total_Employment_Pct
                             , color = interaction(Location, category_var)
                             , group = interaction(Location, category_var))) +
  geom_line(size = 1.5) +  # Increase the line width
  geom_point() +
  scale_color_manual(values = wes_palette("Zissou1", n = length(unique(interaction(data_merged$Location, data_merged$category_var))), type = "continuous")) +
  labs(title = "Total Employment Percentage of Working-Age Population by Category and City (2011-2022)",
       x = "Year",
       y = "Total Employment Percentage") +
  theme_minimal()

# Add labels at the end of line
p2 + geom_text_repel(data = data_merged %>% group_by(Location, category_var) %>% filter(Year == max(Year)),
                    aes(label = Location),
                    nudge_x = 1,  # Adjust this value to move the labels further to the right
                    direction = "y",
                    hjust = 0,
                    segment.color = "grey50")

ggsave(filename = here::here("Figures", "EMP_WA_POP.png"), plot = p2, width = 8, height = 6)

### Average household personal disposable income, real, PPP$-----
label_tothhincome <- attr(data_merged$PEDYHHPPPPC, "label")
print(label_tothhincome)

data_merged <- data_merged %>%
  mutate(PEDYHHPPPPC = as.numeric(PEDYHHPPPPC)) %>% 
  mutate(Avg_Household_Income_PPP = PEDYHHPPPPC)

# ggplot(data_merged, aes(x = Location, y = Avg_Household_Income_PPP, fill = category_var)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(data_merged$category_var)), type = "continuous")) +
#   labs(title = "Average Household Income PPP by Category and City",
#        x = "City",
#        y = "Average Household Income PPP") +
#   theme_minimal()

p3 <- ggplot(data_merged, aes(x = Year, y = Avg_Household_Income_PPP, color = category_var, group = interaction(Location, category_var))) +
  geom_line(size = 1.5) +  # Increase the line width
  geom_point() +
  scale_color_manual(values = wes_palette("Zissou1", n = length(unique(data_merged$category_var)), type = "continuous")) +
  labs(title = "Average Household Income PPP by Category and City (2011-2022)",
       x = "Year",
       y = "Average Household Income PPP") +
  theme_minimal()

# Add labels at the end of line
p3 + geom_text_repel(data = data_merged %>% group_by(Location, category_var) %>% filter(Year == max(Year)),
                    aes(label = Location),
                    nudge_x = 1,  # Adjust this value to move the labels further to the right
                    direction = "y",
                    hjust = 0,
                    segment.color = "grey50")

ggsave(filename = here::here("Figures", "AV_HH_DINC.png"), plot = p3, width = 8, height = 6)

### Labor Force Participation Rate FALTA!!!!!-----
data_merged <- data_merged %>%
  mutate(Labor_Force_Participation_Rate = Labor_Force / Working_Age_Population * 100)

# Calculate Employment in High-Tech Industries as a Pct of total employment FALTA!!!!!
data_filtered <-   data_merged %>% 
  dplyr::select(starts_with("EMP")) %>% 
  colnames()
# [1] "EMPA"         "EMPGIR_U"     "EMPK_N"      
# [5] "EMPB_F"       "EMPO_Q"       "EMPTOTT"      "EMPHJ

map(c("EMPA", "EMPGIR_U", "EMPK_N", "EMPB_F", "EMPO_Q", "EMPTOTT", "EMPHJ"),
    ~ attr(data_merged[[.x]], "label"))

data_merged <- data_merged %>%
  mutate(High_Tech_Employment_Pct = High_Tech_Employment / Total_Employment * 100)

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
  labs(title = "Consumer Spending Percentage by Selected Categories (2011-2022)",
       x = "Location",
       y = "Percentage") +
  theme_minimal() +
  facet_wrap(~ Year, scales = "free", labeller = label_both) +
  theme(strip.text = element_text(size = 8)) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.1)),
            position = position_fill(vjust = 0.5), size = 2)

ggsave(filename = here::here("Figures", "CONS_SPENDING.png"), plot = p6, width = 8, height = 6)

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
         GVAK_NPPPC = as.numeric(GVAK_NPPPC),
         GVAB_FPPPC = as.numeric(GVAB_FPPPC),        
         ) %>%
  mutate(Agriculture_GVA_Pct = GVAAPPPC / GVATOTPPPC
         , Consumer_services_GVA_Pct = GVAGIR_UPPPC / GVATOTPPPC
         , Financial_business_services_GVA_Pct = GVAK_NPPPC / GVATOTPPPC
         , Industry_GVA_Pct = GVAK_NPPPC / GVATOTPPPC          
         , Public_services_GVA_Pct = GVAB_FPPPC / GVATOTPPPC 
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
  labs(title = "GVA Percentage by Sector (2011-2022)",
       x = "Location",
       y = "Percentage") +
  theme_minimal() +
  facet_wrap(~ Year, scales = "free", labeller = label_both) +
  theme(strip.text = element_text(size = 8)) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.1)),
            position = position_fill(vjust = 0.5), size = 2)

ggsave(filename = here::here("Figures", "GVA_SECTOR.png"), plot = p5, width = 8, height = 6)




