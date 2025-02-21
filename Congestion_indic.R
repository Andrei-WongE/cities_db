


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
source("Master_variables.R")
source("Utils.R")
UCDB_all <- read_gpkg_layers(here("Data", "GHS24", "GHS_UCDB_GLOBE_R2024A.gpkg")
                             , selected_layers = NULL
                             , quiet = FALSE)

UCDB_FUA <- read_gpkg_layers(here("Data", "GHS_FUA_19", "GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg")
                             , selected_layers = NULL
                             , quiet = FALSE)

UCDB_all_2019 <- read_gpkg_layers(here("Data", "GHS19", "GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")
                             , selected_layers = NULL
                             , quiet = FALSE)
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

# 
# subset_layers_by_year_location <- function(layers_data, years, countries, cores = parallel::detectCores() - 3) {
#   require(parallel)
#   require(doParallel) 
#   require(foreach)
#   require(dplyr)
#   require(sf)
#   
#   cl <- makeCluster(cores)
#   on.exit(stopCluster(cl))
#   registerDoParallel(cl)
#   
#   foreach(i = seq_along(layers_data), 
#           .packages = c("dplyr", "sf"),
#           .final = function(x) {
#             setNames(x, names(layers_data))
#           }) %dopar% {
#             df <- layers_data[[i]]
#             layer_name <- names(layers_data)[i]
#             
#             gc_gad <- names(df)[grep("^GC_CNT_GAD_", names(df))]
#             gc_mai <- names(df)[grep("^GC_UCN_MAI_", names(df))]
#             
#             year_pattern <- paste0("_", years, "$", collapse = "|")
#             year_cols <- names(df)[grep(year_pattern, names(df))]
#             
#             if(length(gc_gad) == 1 && length(gc_mai) == 1) {
#               df %>%
#                 rename(
#                   location = all_of(gc_gad),
#                   city = all_of(gc_mai)
#                 ) %>%
#                 mutate(layer = layer_name) %>%
#                 select(layer, location, city, all_of(year_cols), geom)
#               # %>%
#               #   filter(location %in% countries)
#             }
#           }
#   #Little effing function
# }
# 
# UCDB_all <- subset_layers_by_year_location(UCDB_all,
#                                        years = c(2000, 2005, 2010, 2015, 2020),
#                                        # countries = mena_countries
#                                       )
# # UCDB_all <- UCDB_all %>% 
#             mutate(Region = case_when(
#               Country %in% mena_countries ~ "MENA",
#               TRUE ~ "Not_MENA"
#             ))

# Urban centre 

id <- UCDB_all$GHS_UCDB_THEME_GHSL_GLOBE_R2024A$ID_UC_G0
density <- UCDB_all$GHS_UCDB_THEME_GHSL_GLOBE_R2024A$GH_XST_D30_2020
plausibility <- UCDB_all$GHS_UCDB_THEME_GENERAL_CHARACTERISTICS_GLOBE_R2024A
geom <- UCDB_all$GHS_UCDB_THEME_GHSL_GLOBE_R2024A$geom
density_ucdb <- data.frame(id = as.numeric(id), Density = as.numeric(density)
                           , Plausibility  = as.factor(plausibility)
                           , geom = geom)

id <- UCDB_all$GHS_UCDB_THEME_EMISSIONS_GLOBE_R2024A$ID_UC_G0
pm25 <- UCDB_all$GHS_UCDB_THEME_EMISSIONS_GLOBE_R2024A$EM_PM2_TOT_2020
pm25_con <- UCDB_all$GHS_UCDB_THEME_EMISSIONS_GLOBE_R2024A$EM_PM2_CON_2020
geom <- UCDB_all$GHS_UCDB_THEME_EMISSIONS_GLOBE_R2024A$geom
pm25_ucdb <- data.frame(id = as.numeric(id)
                        , PM2.5 = as.numeric(pm25)
                        , PM2.5_concentration = as.numeric(pm25_con)
                        , geom = geom
                        )

geo_ucdb <- UCDB_all$GHS_UCDB_THEME_GENERAL_CHARACTERISTICS_GLOBE_R2024A %>% 
            rename(
              id = ID_UC_G0,
              Location = GC_UCN_MAI_2025,
              Country = GC_CNT_GAD_2025,
              Regions = GC_DEV_USR_2025
            ) 

id <- UCDB_all$GHS_UCDB_THEME_SOCIOECONOMIC_GLOBE_R2024A$ID_UC_G0
gdp <- UCDB_all$GHS_UCDB_THEME_SOCIOECONOMIC_GLOBE_R2024A$SC_SEC_GDP_2020
geom <- UCDB_all$GHS_UCDB_THEME_SOCIOECONOMIC_GLOBE_R2024A$geom
econ_ucdb <- data.frame(id = as.numeric(id), GDP = as.numeric(gdp), geom = geom)

pollution_ucdb <- density_ucdb %>%
                  full_join(pm25_ucdb, by = c("id")) %>%
                  full_join(geo_ucdb, by = c("id")) %>% 
                  full_join(econ_ucdb, by = c("id")) %>%
                  dplyr::filter(Plausibility == "High") %>%
                  mutate(log_density = log(Density)) %>% 
                  mutate(log_concentration = log(PM2.5_concentration)) %>%
                  mutate(log_gdp = log(GDP)) %>%
                  mutate(Region = case_when(
                    Country %in% mena_countries ~ "MENA",
                    TRUE ~ "Not_MENA"
                  ))

summary(pollution_ucdb$Density)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    3633    5326    6040    7714   61003 

summary_details <- function(data, variable_name, region_column, region_value) {
  variable <- data[[variable_name]]
  region_count <- sum(data[[region_column]] == region_value, na.rm = TRUE)
  n <- length(variable)
  missing <- sum(is.na(variable))
  non_missing <- n - missing
  basic_summary <- summary(variable)
  
  list(
    N = n,
    Missing = missing,
    Non_Missing = non_missing,
    Region_Count = region_count,
    Summary = basic_summary
  )
}
summary_details(pollution_ucdb
                # , "log_density"
                , "PM2.5_concentration"
                , "Region"
                , "MENA"
                )


# Plots

index <- c("PM2.5", "PM2.5_concentration")

plot_index <- function(index) {
  plot <- pollution_ucdb %>% 
    filter(Country != Location) %>% 
    filter(.data[[index]] > 0) %>%  
    ggplot(aes(x = log_gdp, y = .data[[index]], color = Region)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +  # Optional: Add a linear regression line
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Log GDP", y = gsub("_", "", index)
         # , title = paste(index, "vs Log Density (2020)")
    ) +
    scale_color_manual(values = c("MENA" = "red", "Other" = "blue"))
  
  print(plot)
  
  ggsave(filename = here("Figures", paste0(index, "_vs_Log_GDP_2020.png"))
         , plot = plot, width = 12, height = 10, dpi = 800)
}

walk(index, plot_index)

# Boxplot by region

# Create a combined dataset with both Regions2 and MENA
plot_data <- pollution_ucdb %>%
  group_by(Regions) %>%
  mutate(median_PM2.5_concentration = median(PM2.5_concentration)) %>%
  ungroup() %>%
  mutate(Regions2 = factor(Regions)) %>%
  # mutate(Regions2 = reorder(Regions2, -median_PM2.5_concentration)) %>%
  # group_by(Regions2) %>%
  # mutate(n = n()) %>%
  ungroup()

# Add MENA as a separate category
mena_data <- pollution_ucdb %>% 
  filter(Region == "MENA") %>%
  mutate(Regions2 = "MENA",
         median_PM2.5_concentration = median(PM2.5_concentration)) %>% 
  # mutate(Regions2 = reorder(Regions2, -median_PM2.5_concentration)) %>% 
  group_by(Regions2) %>%
  mutate(n = n()) %>%
  ungroup()

# Combine the datasets
plot_data <- bind_rows(plot_data, mena_data) %>% 
  mutate(Regions2 = reorder(Regions2, median_PM2.5_concentration)) %>% 
  mutate(Regions2 = reorder(Regions2, -median_PM2.5_concentration)) %>% 
  group_by(Regions2) %>%
  mutate(n = n()) %>%
  ungroup()


plot2 <- ggplot(plot_data,
                aes(x = PM2.5_concentration, y = Regions2, fill = Regions2 == "MENA")) +
  geom_boxplot(outlier.size = 2, outlier.alpha = 0.6) +
  labs(
    x = expression(PM[2.5]~concentration),
    y = NULL,
    title = expression(Concentration~of~PM[2.5]~by~region)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(limits = c(0, 150)) +
  scale_fill_manual(values = c("TRUE" = "red"))  +
  labs(y = NULL) +
  annotate("text", x = Inf, y = plot_data$Regions2
           , label = paste0(" (n = ", plot_data$n, ")"), 
           hjust = 1, vjust = 0.5, size = 4)

ggsave(filename = here("Figures", "PM2.5_concentration_vs_Log_Density_2020.png")
       , plot = plot2, width = 12, height = 10, dpi = 800)
plot2


# FUA, merge using UC_ID1 (Urban Centre IDs contained in each eFUA, as GHS-UCDB “ID_HDC_G0”-----

## Split UC_IDs string and extract ID_HDC_G0 values
extract_UC_IDs <- function(ids_string) {
  if(is.na(ids_string)) return(NA)
  return(strsplit(ids_string, ",|;| ")[[1]])
}

## New dataset, slow but simple
create_pollution_data <- function(UCDB_all_2019, UCDB_FUA) {
  
  require(progress)
  
  # Create a lookup table from UCDB_all_2019
  pollution_lookup <- UCDB_all_2019 %>%
    dplyr::select(ID_HDC_G0, E_CPM2_T14, AREA, CTR_MN_NM, GRGN_L1)
  
  # Setup progress bar
  pb <- progress_bar$new(
    format = "  Processing [:bar] :percent eta: :eta",
    total = nrow(UCDB_FUA),
    clear = FALSE,
    width = 60
  )
  
  # Process each FUA record with progress updates
  result <- data.frame()
  
  for (i in 1:nrow(UCDB_FUA)) {
    pb$tick()
    
    fua_row <- UCDB_FUA[i,]
    
    # Case when only one UC_ID
    if (fua_row$UC_num == 1) {
      matches <- pollution_lookup %>% 
        filter(ID_HDC_G0 == fua_row$UC_IDs)
    
      if (nrow(matches) > 0) {
        fua_row$E_CPM2_T14_agg <- matches$E_CPM2_T14
      } else {
        fua_row$E_CPM2_T14_agg <- "Error, multiple matches"
      }
    
   # Case when only more than one UC_ID
    } else if (fua_row$UC_num > 1) {
      ids <- extract_UC_IDs(fua_row$UC_IDs)
      
      if (!all(is.na(ids))) {
        matches <- pollution_lookup %>% 
          filter(ID_HDC_G0 %in% ids)
        
        if (nrow(matches) > 0) {
          fua_row$E_CPM2_T14_agg <- sum(matches$E_CPM2_T14 * matches$AREA, na.rm = TRUE) / 
            sum(matches$AREA, na.rm = TRUE)
        } else {
      #  Just in case of missing data    
          fua_row$E_CPM2_T14_agg <- NA
        }
      #  Just in case there are no IDs    
      } else {
        fua_row$E_CPM2_T14_agg <- "No match"
      }
    }
    
    result <- rbind(result, fua_row)
  }
  
  return(result)
}

# QA2_1V: quality code (0 – false positive, 1 – true positive, >1 uncertain) 10303 cases vs 13135 cases
UCDB_all_2019 <- UCDB_all_2019 %>% 
                 dplyr::filter(QA2_1V == 1) %>% 
                 st_drop_geometry()

pollution_ucdb2 <- create_pollution_data(UCDB_all_2019, UCDB_FUA)

mena_countries2 <- c("Algeria", "Bahrain", "Djibouti", "Egypt", "Iran", "Iraq", "Israel", "Jordan", "Kuwait", 
                     "Lebanon", "Libya", "Mauritania", "Morocco", "Oman", "Palestine", "Qatar", 
                     "Saudi Arabia", "Somalia", "Sudan", "Syria", "Tunisia", "United Arab Emirates", 
                     "Yemen")

# Join with UCDB_all_2019 to get country and region names
UCDB_all_2019_sub <- UCDB_all_2019 %>% 
  dplyr::select(CTR_MN_NM, GRGN_L1) %>% 
  group_by(CTR_MN_NM) %>%
  distinct()

pollution_ucdb2_merged <- pollution_ucdb2 %>% 
  left_join(UCDB_all_2019_sub
          , by = c("Cntry_name" = "CTR_MN_NM")
          , relationship = "many-to-many"
           ) %>% 
  mutate(Country = Cntry_name,
         Region = GRGN_L1,
         PM2.5_concentration = E_CPM2_T14_agg
         ) %>%
  mutate(Region = case_when(
    Country %in% mena_countries2 ~ "MENA",
    TRUE ~ Region
  ))

# Issues with country mapping
country_region_mapping <- c(
  "BosniaandHerzegovina" = "Europe",
  "BurkinaFaso" = "Africa",
  "CapeVerde" = "Africa",
  "CentralAfricanRepublic" = "Africa",
  "CostaRica" = "Latin America and the Caribbean",
  "Curacao" = "Latin America and the Caribbean",
  "CzechRepublic" = "Europe",
  "CotedIvoire" = "Africa",
  "DemocraticRepublicoftheCongo" = "Africa",
  "DominicanRepublic" = "Latin America and the Caribbean",
  "ElSalvador" = "Latin America and the Caribbean",
  "EquatorialGuinea" = "Africa",
  "FrenchGuiana" = "Latin America and the Caribbean",
  "Guadeloupe" = "Latin America and the Caribbean",
  "GuineaBissau" = "Africa",
  "HongKong" = "Asia",
  "Macao" = "Asia",
  "Martinique" = "Latin America and the Caribbean",
  "Mayotte" = "Africa",
  "NewCaledonia" = "Oceania",
  "NewZealand" = "Oceania",
  "NorthKorea" = "Asia",
  "NorthernCyprus" = "Europe",
  "PapuaNewGuinea" = "Oceania",
  "PuertoRico" = "Latin America and the Caribbean",
  "RepublicoftheCongo" = "Africa",
  "RepublicofCongo" = "Africa",
  "Reunion" = "Africa",
  "SaudiArabia" = "MENA",
  "SierraLeone" = "Africa",
  "SolomonIslands" = "Oceania",
  "SouthAfrica" = "Africa",
  "SouthKorea" = "Asia",
  "SouthSudan" = "Africa",
  "SriLanka" = "Asia",
  "SaoTomeandPrincipe" = "Africa",
  "TimorLeste" = "Asia",
  "TrinidadandTobago" = "Latin America and the Caribbean",
  "UnitedArabEmirates" = "MENA",
  "UnitedKingdom" = "Europe",
  "UnitedStates" = "Northern America",
  "WesternSahara" = "Africa"
)

# Assigning regions based on the country
pollution_ucdb2_merged <- pollution_ucdb2_merged %>%
  mutate(Region = case_when(
    Country %in% names(country_region_mapping) ~ country_region_mapping[Country],
    TRUE ~ Region
  ))


summary_details(pollution_ucdb2_merged
                , "PM2.5_concentration"
                , "Region"
                , "MENA"
)


# Plot 
plot_data <- pollution_ucdb2_merged %>% 
  group_by(Region) %>%
  mutate(median_PM2.5_concentration = median(PM2.5_concentration)) %>%
  ungroup() %>%
  mutate(Region = factor(Region)) %>%
  ungroup()

plot_data <- plot_data %>% 
  mutate(Region = reorder(Region, -median_PM2.5_concentration)) %>% 
  group_by(Region) %>%
  mutate(n = n()) %>%
  ungroup() %>% 
  filter(!is.na(median_PM2.5_concentration))

plot3 <- ggplot(plot_data,
                aes(x = PM2.5_concentration, y = Region, fill = Region == "MENA")) +
  geom_boxplot(outlier.size = 2, outlier.alpha = 0.6) +
  labs(
    x = expression(PM[2.5]~concentration),
    y = NULL,
    title = expression(Concentration~of~PM[2.5]~by~region)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(limits = c(0, 150)) +
  scale_fill_manual(values = c("TRUE" = "red"))  +
  labs(y = NULL) +
  annotate("text", x = Inf, y = plot_data$Region
           , label = paste0(" (n = ", plot_data$n, ")"), 
           hjust = 1, vjust = 0.5, size = 4)

ggsave(filename = here("Figures", "PM2.5_concentration_FUA.png")
       , plot = plot3, width = 12, height = 10, dpi = 800)

plot3
          dplyr::select(,"Country"
                              ,"Year"
                              ,"Location"
                              ,"GC_CNT_GAD_XXXX"  # Country's main cities
                              ,"GC_UCN_MAI_XXXX"   # Country's main cities
                              ,"IN_ROA_LEN_XXXX"   # Total road length inside the urban center
                              ,"IN_CIS_TRA_XXXX"   # Critical Infrastructures Spatial Index for the transportation sector
                              ,"IN_TRA_TOT_XXXX"   # Total infrastructure in the transportation sector
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
  data_rankings_traffic_past, oe_data %>% filter(Country != Location),
  by = c("country" = "Country", "city_name" = "Location"),
  mode = "full",
  method = "jw" # 
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
  mutate(area = st_area(.)) %>% 
  mutate(area_km2 = as.numeric(st_area(.) / 1e6)) # Adjust to km²

matching_cities <- matching_cities %>% 
  left_join(oe_geo, by = c("Location" = "OE_FUANAME")) %>% 
  dplyr::select(-geometry)

matching_cities <- matching_cities %>%
  rename(Area = area_km2) 
# %>%
#   dplyr::select(-area.y)

matching_cities <- matching_cities %>% 
  mutate(density = if_else(is.na(Area), NA_real_, as.numeric(POPTOTT) * 1e3/ as.numeric(Area)))

matching_cities <- matching_cities %>% 
mutate(Region = case_when(
  Country %in% mena_countries ~ "MENA",
  TRUE ~ "Not_MENA"
)) %>% 
mutate(log_density = log(density))

summary(matching_cities %>% filter(Country != Location) %>% pull(density))

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      57     546    1280    1722    2468    7465     220

summary_details <- function(data, variable_name, region_column, region_value) {
  variable <- data[[variable_name]]
  region_count <- sum(data[[region_column]] == region_value, na.rm = TRUE)
  n <- length(variable)
  missing <- sum(is.na(variable))
  non_missing <- n - missing
  basic_summary <- summary(variable)
  
  list(
    N = n,
    Missing = missing,
    Non_Missing = non_missing,
    Region_Count = region_count,
    Summary = basic_summary
  )
}
summary_details(matching_cities %>% filter(Country != Location) %>% filter(Year == 2019) 
                , "log_density", "Region", "MENA")

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
    labs(x = "Log Density", y = index
         # , title = paste(index, "vs Log Density (2019)")
         ) +
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
  data_rankings_pollution_past, oe_data %>% filter(Country != Location),
  by = c("country" = "Country", "city_name" = "Location"),
  mode = "full",
  method = "jw" # 
  #, distance_col = "distance"
)
)
matching_cities_pollution %>% filter(Country!=Location) %>% 
  # filter(Year.x == Year.y) %>% 
  distinct(.$Location) %>% 
  View()

matching_cities_pollution <- matching_cities_pollution %>% filter(Year.x == Year.y) %>% 
  rename(Year = Year.x) %>% dplyr::select(-Year.y)
# For 2019, onlt 30pct of OE cities have a match in NUMBEO

matching_cities_pollution <- matching_cities_pollution %>% 
  left_join(oe_geo, by = c("Location" = "OE_FUANAME")) %>% 
  dplyr::select(-geometry)

matching_cities_pollution <- matching_cities_pollution %>%
  rename(Area = area_km2) 
# %>%
#   dplyr::select(-area.y)

matching_cities_pollution <- matching_cities_pollution %>% 
  mutate(density = if_else(is.na(Area), NA_real_, as.numeric(POPTOTT) * 1e3/ as.numeric(Area)))

matching_cities_pollution <- matching_cities_pollution %>% 
  mutate(Region = case_when(
    Country %in% mena_countries ~ "MENA",
    TRUE ~ "Not_MENA"
  )) %>% 
  mutate(log_density = log(density))

summary(matching_cities_pollution %>% filter(Country != Location) %>% pull(density))

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      17     744    1459    2845    2590  122506     363

summary_details(matching_cities_pollution %>% filter(Country != Location) %>% filter(Year == 2019) 
                , "log_density", "Region", "MENA")

     
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
    labs(x = "Log Density", y = index
         # , title = paste(index, "vs Log Density (2019)")
         ) +
    scale_color_manual(values = c("MENA" = "red", "Other" = "blue"))
  
  print(plot)
  
  ggsave(filename = here("Figures", paste0(index, "_vs_Log_Density_2019.png")), plot = plot)
}

walk(index, plot_index)

# Quality of life
-----------------
cost_living <- readRDS(here("Data", "NUMBEO","data_rankings_past.rds"))
names(cost_living)

# (matching_cities_cost_living <- stringdist_join(
#   cost_living, oe_data %>% filter(Country != Location),
#   by = c("country" = "Country", "city_name" = "Location"),
#   mode = "full",
#   method = "jw" # 
#   #, distance_col = "distance"
# )
# )

(matching_cities_cost_living <- left_join(
  cost_living, oe_data %>% filter(Country != Location),
  by = c("country" = "Country", "city_name" = "Location")
)
)

matching_cities_cost_living <- matching_cities_cost_living %>% filter(Year.x == Year.y) %>% 
  rename(Year = Year.x) %>% dplyr::select(-Year.y)
# For 2019, onlt 30pct of OE cities have a match in NUMBEO

matching_cities_cost_living <- matching_cities_cost_living %>% 
  left_join(oe_geo, by = c("Location" = "OE_FUANAME")) %>% 
  dplyr::select(-geometry)

matching_cities_cost_living <- matching_cities_cost_living %>%
  rename(Area = area_km2) 
# %>%
#   dplyr::select(-area.y)

matching_cities_cost_living <- matching_cities_cost_living %>% 
  mutate(density = if_else(is.na(Area), NA_real_, as.numeric(POPTOTT) * 1e3/ as.numeric(Area)))

matching_cities_cost_living <- matching_cities_cost_living %>% 
  mutate(Region = case_when(
    Country %in% mena_countries ~ "MENA",
    TRUE ~ "Not_MENA"
  )) %>% 
  mutate(log_density = log(density))

summary(matching_cities_cost_living %>% filter(Country != Location) %>% pull(density))

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      17     744    1459    2845    2590  122506     363

summary_details(matching_cities_cost_living %>% filter(Country != Location) %>% filter(Year == 2019) 
                , "log_density", "Region", "MENA")


index <- c("pollution_index", "exp_pollution_index")

plot_index <- function(index) {
  plot <- matching_cities_cost_living %>% 
    filter(Country != Location) %>% 
    filter(Year == 2019) %>%
    ggplot(aes(x = log_density, y = .data[[index]], color = Region)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +  # Optional: Add a linear regression line
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Log Density", y = index
         # , title = paste(index, "vs Log Density (2019)")
    ) +
    scale_color_manual(values = c("MENA" = "red", "Other" = "blue"))
  
  print(plot)
  
  ggsave(filename = here("Figures", paste0(index, "_vs_Log_Density_2019.png")), plot = plot)
}
