


# Congestion indicators -----
require(naniar)

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

subset_layers_by_year_location <- function(layers_data, years, countries, cores = parallel::detectCores() - 1) {
  require(parallel)
  require(doParallel) 
  require(foreach)
  require(dplyr)
  require(sf)
  
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  layers_subset <- foreach(i = seq_along(layers_data), 
                           .packages = c("dplyr", "sf"),
                           .final = function(x) {
                             names(x) <- names(layers_data)
                             return(x)
                           }) %dopar% {
                             df <- layers_data[[i]]
                             layer_name <- names(layers_data)[i]
                             
                             gc_gad <- names(df)[grep("^GC_CNT_GAD_", names(df))]
                             gc_mai <- names(df)[grep("^GC_UCN_MAI_", names(df))]
                             
                             year_pattern <- paste0("_", years, "$", collapse = "|")
                             year_cols <- names(df)[grep(year_pattern, names(df))]
                             
                             if(length(gc_gad) == 1 && length(gc_mai) == 1) {
                               df <- df %>%
                                 rename(
                                   location = all_of(gc_gad),
                                   city = all_of(gc_mai)
                                 ) %>%
                                 mutate(layer = layer_name) %>%
                                 select(layer, location, city, all_of(year_cols), geom) %>%
                                 filter(location %in% countries)
                             }
                             return(df)
                           }
  
  stopCluster(cl)
  return(layers_subset)
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
