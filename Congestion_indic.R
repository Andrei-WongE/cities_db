


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

subset_layers_by_year_location <- function(layers_data, years, countries, cores = parallel::detectCores() - 1) {
  require(parallel)
  require(doParallel)
  require(foreach)
  require(dplyr)
  require(sf)
  
  # Ensure years is numeric
  years <- as.numeric(years)
  
  # Setup parallel backend
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Process layers in parallel
  layers_subset <- foreach(x = layers_data, 
                           .packages = c("dplyr", "sf"),
                           .export = c("years", "countries")) %dopar% {
                             
                             # Create dynamic renaming pairs for each year
                             rename_pairs <- lapply(years, function(year) {
                               c(
                                 setNames(paste0("GC_CNT_GAD_", year), paste0("location_", year)),
                                 setNames(paste0("GC_UCN_MAI_", year), paste0("city_", year))
                               )
                             })
                             
                             # Combine all rename pairs into one list
                             rename_list <- do.call(c, rename_pairs)
                             
                             # Rename the columns
                             x <- x %>%
                               rename(!!!rename_list)
                             
                             # Get columns for specified years
                             year_cols <- grep(paste0("_(", paste(years, collapse = "|"), ")$"), 
                                               names(x), value = TRUE, perl = TRUE)
                             
                             # Filter and select
                             if (length(year_cols) > 0) {
                               x <- x %>%
                                 filter(if_any(starts_with("location_"), ~ . %in% countries)) %>%
                                 select(starts_with("location_"), starts_with("city_"), all_of(year_cols), geometry)
                             }
                             
                             return(x)
                           }
  
  # Stop cluster
  stopCluster(cl)
  return(layers_subset)
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
search_variable(ucdb_mena_cities, search_term = "EM_", partial_match = TRUE, case_sensitive = FALSE)
                                       