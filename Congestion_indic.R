


# Congestion indicators -----
require(naniar)
require(fuzzyjoin)

source("Utils.R")

# Run other scripts for main databases
if(all(sapply(c("oe_mena", "ucdb_mena_cities", "UCDB_all"), exists)))
 {
  print("Objects exists")

   } else {
  
print("Some of all objects does not exist")
 }

# OE indicators----
oe_mena_cong <- oe_mena %>% 
                dplyr::select(,"Country"
                              ,"Year"
                              ,"Location"
                              ,"FC041PPPC"  #'Consumer spending, real, PPP$ - Housing rent'
                              ,"FC042PPPC" #'Consumer spending, real, PPP$ - Imputed housing rent'
                              ,"FC072PPPC" #'Consumer spending, real, PPP$ - Personal transport running costs'
                              ,"FC07PPPC"  #'Consumer spending, real, PPP$ - Transport services and vehicle purchases - Total" 
                     )

glimpse(oe_mena_cong)

result <- analyze_missing_values(oe_mena_cong, years = c(2001, 2019), multi_year = TRUE, show_pct = TRUE)
(result$missing_summary)
(result$plot)


# UCDB indicators----

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

subset_layers_by_year_location <- function(layers_data, years, countries, cores = parallel::detectCores() - 3) {
  require(parallel)
  require(doParallel) 
  require(foreach)
  require(dplyr)
  require(sf)
  
  cl <- makeCluster(cores)
  on.exit(stopCluster(cl))
  registerDoParallel(cl)
  
  foreach(i = seq_along(layers_data), 
          .packages = c("dplyr", "sf"),
          .final = function(x) {
            setNames(x, names(layers_data))
          }) %dopar% {
            df <- layers_data[[i]]
            layer_name <- names(layers_data)[i]
            
            gc_gad <- names(df)[grep("^GC_CNT_GAD_", names(df))]
            gc_mai <- names(df)[grep("^GC_UCN_MAI_", names(df))]
            
            year_pattern <- paste0("_", years, "$", collapse = "|")
            year_cols <- names(df)[grep(year_pattern, names(df))]
            
            if(length(gc_gad) == 1 && length(gc_mai) == 1) {
              df %>%
                rename(
                  location = all_of(gc_gad),
                  city = all_of(gc_mai)
                ) %>%
                mutate(layer = layer_name) %>%
                select(layer, location, city, all_of(year_cols), geom) %>%
                filter(location %in% countries)
            }
          }
  #Little effing function
}

UCDB_all_mena <- subset_layers_by_year_location(UCDB_all,
                                       years = c(2000, 2005, 2010, 2015, 2020),
                                       countries = mena_countries
                                      ) 

  
                        dplyr::select(,"Location"
                                      ,"Year"
                                      ,"EM_CO2_TRA_XXXX"  # Total CO2 emissions in transport sector
                                      ,"EM_PM2_TRA_XXXX"   # Total PM2.5 emissions in transport sector 
                                      ,"EM_GHG_TRA_XXXX"   # Total GHG emissions in transport sector
                                      ,"EM_NOX_TRA_XXXX"   # Total NOx emissions in transport sector
                                      ,"EM_TRA_TOT_XXXX"   # Total emissions in transport sector
                                      ,"EM_TRA_PER_XXXX"   # Share of transport emissions over total emissions
                                      ,"ACCES_CT_MN"     # Accessibility to the country's main cities
                                      ,"IN_ROA_LEN_XXXX"   # Total road length inside the urban center
                                      ,"IN_CIS_TRA_XXXX"   # Critical Infrastructures Spatial Index for the transportation sector
                                      ,"IN_TRA_TOT_XXXX"   # Total infrastructure in the transportation sector
                                      )                                  
                                     
search_term <- c("Location"
                ,"Year"
                ,"EM_CO2_TRA_XXXX"  # Total CO2 emissions in transport sector
                ,"EM_PM2_TRA_XXXX"   # Total PM2.5 emissions in transport sector 
                ,"EM_GHG_TRA_XXXX"   # Total GHG emissions in transport sector
                ,"EM_NOX_TRA_XXXX"   # Total NOx emissions in transport sector
                ,"EM_TRA_TOT_XXXX"   # Total emissions in transport sector
                ,"EM_TRA_PER_XXXX"   # Share of transport emissions over total emissions
                ,"ACCES_CT_MN"     # Accessibility to the country's main cities
                ,"IN_ROA_LEN_XXXX"   # Total road length inside the urban center
                ,"IN_CIS_TRA_XXXX"   # Critical Infrastructures Spatial Index for the transportation sector
                ,"IN_TRA_TOT_XXXX"   # Total infrastructure in the transportation sector
)        

# NUMBEO indicators ----
data_rankings_traffic_past <- readRDS(here("Data","NUMBEO","data_rankings_traffic_past.rds"))
names(data_rankings_traffic_past)
unique(sort(as.factor(data_rankings_traffic_past$Year)))

data_rankings_traffic_past <- data_rankings_traffic_past %>% 
  mutate(Year = case_when( Year == "2012-Q1" ~ "2012"
                           , Year == "2013-Q1" ~ "2013"
                           , TRUE ~ Year )
         ) %>% 
  dplyr::filter(!str_detect(Year, "-mid")) %>% 
  group_by(city_name, Year) 

oe_data <- data
names(oe_data)

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
      
oe_data %>% dplyr::filter(Country %in% mena_countries)
    
mena_list <- oe_data %>% filter(Country!=Location) %>%
  distinct(.$Location)

oe_data <- oe_data %>% group_by(Location, Year) 

# search_variable(data_rankings_traffic
#                 , mena_list
#                 , partial_match = TRUE
#                 , case_sensitive = FALSE
#                 )

(matching_cities <- stringdist_join(
  data_rankings_traffic_past, oe_data,
  by = c("city_name" = "Location"),
  mode = "full",
  method = "lv" # Levenshtein, Measures the minimum number of single-character edits required.
  #, distance_col = "distance"
)
)
matching_cities %>% filter(Country!=Location) %>% 
                    filter(Year.x == Year.y) %>% 
                    distinct(.$Location) %>% 
                    View()

matching_cities <- matching_cities %>% filter(Year.x == Year.y) %>% 
  rename(Year = Year.x) %>% dplyr::select(-Year.y)
# For 2019, onlt 30pct of OE cities have a match in NUMBEO

# Open OE geometry
oe_geo <- st_read(here("Data", "OE_FUA_SHAPEFILE", "OE_FUA_SHAPEFILE.shp"))

valid_geometries <- st_is_valid(oe_geo)
summary(valid_geometries)
# Mode   FALSE    TRUE 
# logical       1     899 

oe_geo <- st_make_valid(oe_geo)

oe_geo <- oe_geo %>% 
  mutate(area = st_area(.)/1e6) # Adjust to km2

matching_cities <- matching_cities %>% 
  left_join(oe_geo, by = c("Location" = "OE_FUANAME")) %>% 
  dplyr::select(-geometry)

matching_cities <- matching_cities %>%
  rename(Area = area) 
# %>%
#   dplyr::select(-area.y)

matching_cities <- matching_cities %>% 
  mutate(density = if_else(is.na(Area), NA_real_, as.numeric(POPTOTT) * 1e6/ as.numeric(Area)))

matching_cities <- matching_cities %>% 
mutate(Region = case_when(
  Country %in% mena_countries ~ "MENA",
  TRUE ~ "Not_MENA"
)) %>% 
mutate(log_density = log(density))

summary(matching_cities %>% filter(Country != Location) %>% pull(density))

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.06    0.20    0.36    0.93    1.94    4.54      82 

index <- c("time_exp_index", "traffic_index", "time_index", "inefficiency_index", "co2_emission_index")

plot_index <- function(index) {
  plot <- matching_cities %>% 
    filter(Country != Location) %>% 
    filter(Year == 2019) %>%
    ggplot(aes(x = log_density, y = .data[[index]], color = Region)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +  # Optional: Add a linear regression line
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Log Density", y = index, title = paste(index, "vs Log Density (2019)")) +
    scale_color_manual(values = c("MENA" = "red", "Other" = "blue"))
  
  print(plot)
  
  ggsave(filename = here("Figures", paste0(index, "_vs_Log_Density_2019.png")), plot = plot)
}

walk(index, plot_index)

# Contamination
---------------
data_rankings_pollution_past <- readRDS(here("Data","NUMBEO","data_rankings_pollution_past.rds"))
names(data_rankings_pollution_past)
unique(sort(as.factor(data_rankings_pollution_past$Year)))

data_rankings_pollution_past <- data_rankings_pollution_past %>% 
  mutate(Year = case_when( Year == "2012-Q1" ~ "2012"
                           , Year == "2013-Q1" ~ "2013"
                           , TRUE ~ Year )
  ) %>% 
  dplyr::filter(!str_detect(Year, "-mid")) %>% 
  group_by(city_name, Year) 

(matching_cities_pollution <- stringdist_join(
  data_rankings_pollution_past, oe_data,
  by = c("city_name" = "Location"),
  mode = "full",
  method = "lv" # Levenshtein, Measures the minimum number of single-character edits required.
  #, distance_col = "distance"
)
)
matching_cities_pollution %>% filter(Country!=Location) %>% 
  filter(Year.x == Year.y) %>% 
  distinct(.$Location) %>% 
  View()

matching_cities_pollution <- matching_cities_pollution %>% filter(Year.x == Year.y) %>% 
  rename(Year = Year.x) %>% dplyr::select(-Year.y)
# For 2019, onlt 30pct of OE cities have a match in NUMBEO

matching_cities_pollution <- matching_cities_pollution %>% 
  left_join(oe_geo, by = c("Location" = "OE_FUANAME")) %>% 
  dplyr::select(-geometry)

matching_cities_pollution <- matching_cities_pollution %>%
  rename(Area = area) 
# %>%
#   dplyr::select(-area.y)

matching_cities_pollution <- matching_cities_pollution %>% 
  mutate(density = if_else(is.na(Area), NA_real_, as.numeric(POPTOTT) * 1e6/ as.numeric(Area)))

matching_cities_pollution <- matching_cities_pollution %>% 
  mutate(Region = case_when(
    Country %in% mena_countries ~ "MENA",
    TRUE ~ "Not_MENA"
  )) %>% 
  mutate(log_density = log(density))

summary(matching_cities_pollution %>% filter(Country != Location) %>% pull(density))

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.06    0.20    0.36    0.93    1.94    4.54      82 

index <- c("pollution_index", "exp_pollution_index")

plot_index <- function(index) {
  plot <- matching_cities_pollution %>% 
    filter(Country != Location) %>% 
    filter(Year == 2019) %>%
    ggplot(aes(x = log_density, y = .data[[index]], color = Region)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +  # Optional: Add a linear regression line
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Log Density", y = index, title = paste(index, "vs Log Density (2019)")) +
    scale_color_manual(values = c("MENA" = "red", "Other" = "blue"))
  
  print(plot)
  
  ggsave(filename = here("Figures", paste0(index, "_vs_Log_Density_2019.png")), plot = plot)
}

walk(index, plot_index)

