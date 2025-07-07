
source("Master_variables.R")
source("Utils.R")
require(scales)

# USCDB-----
UCDB_multi <- st_read(here("Data", "GHS24", "GHS_UCDB_MTUC_GLOBE_R2024A.gpkg")
                             # , layer = "GHSL_UCDB_MTUC_2020_GLOBE_R2024"
                      )
UCDB_all <- read_gpkg_layers(here("Data", "GHS24", "GHS_UCDB_GLOBE_R2024A.gpkg")
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

country_groups <- list(
  GCC = c(
    "Bahrain",
    "Kuwait",
    "Oman",
    "Qatar",
    "Saudi Arabia",
    "UAE"
  ),
  
  Maghreb = c(
    "Algeria",
    "Libya",
    "Mauritania",
    "Morocco",
    "Tunisia"
  ),
  
  Mashreq = c("Bahrain",
              "Egypt",
              "Iraq",
              "Jordan",
              "Kuwait",
              # "Lebanon",
              "Oman",
              "Palestine",
              "Qatar",
              "Saudi Arabia",
              "Sudan",
              "Syria",
              "UAE",
              "Yemen")
)

id <- UCDB_all$GHS_UCDB_THEME_GHSL_GLOBE_R2024A$ID_UC_G0
wb_income_group <- UCDB_all$GHS_UCDB_THEME_GHSL_GLOBE_R2024A$GC_DEV_WIG_2025
geo_region <- UCDB_all$GHS_UCDB_THEME_GHSL_GLOBE_R2024A$GC_DEV_USR_2025
country <- UCDB_all$GHS_UCDB_THEME_GHSL_GLOBE_R2024A$GC_CNT_GAD_2025
comparators_ucdb <- data.frame(id = as.numeric(id)
                               , WB_income_group = as.character(wb_income_group)
                               , Region = as.character(geo_region)
                               , Country = as.character(country)
                               ) %>%
                    dplyr::select(-id) %>%
                    distinct()

comparators_ucdb$Country <- gsub("United Arab Emirates", "UAE", comparators_ucdb$Country)
comparators_ucdb$Country <- gsub("Democratic Republic of the Congo", "Democratic Republic of Congo", comparators_ucdb$Country)
comparators_ucdb$Country <- gsub("Republic of the Congo", "Congo", comparators_ucdb$Country)
comparators_ucdb$Country <- gsub("Cabo Verde", "Cape Verde", comparators_ucdb$Country)
comparators_ucdb$Country <- gsub("Czechia", "Czech Republic", comparators_ucdb$Country)
comparators_ucdb$Country <- gsub("Laos", "Lao PDR", comparators_ucdb$Country)
comparators_ucdb$Country <- gsub("México", "Mexico", comparators_ucdb$Country)
comparators_ucdb$Country <- gsub("Swaziland", "Eswatini", comparators_ucdb$Country)


id <- UCDB_multi$ID_MTUC_G0
uc_extent_change <- UCDB_multi$MT_XST_D30_2020_2015
built_pc_change <- UCDB_multi$MT_BPC_DIF_2020_2015
built_rel_change <- UCDB_multi$MT_BUS_DIF_2020_2015
city_uc <-UCDB_multi$GC_UCN_MAI_2025
wb_income_group <-UCDB_multi$GC_DEV_WIG_2025
geo_region <- UCDB_multi$GC_DEV_USR_2025
country <- UCDB_multi$GC_CNT_GAD_2025
# geom <- UCDB_multi$geom
built_ucdb <- data.frame( id = as.numeric(id)
                        , UC_extent_change = as.numeric(uc_extent_change)
                        , Built_up_pc_change = as.numeric(built_pc_change)
                        , Built_rel_change = as.numeric(built_rel_change)
                        , Urban_centre = as.character(city_uc)
                        , WB_income_group = as.character(wb_income_group)
                        , Region = as.character(geo_region)
                        , Country = as.character(country)
                        )  %>% 
              mutate(Comparators = case_when(
                country %in% country_groups$GCC ~ "GCC",
                country %in% country_groups$Maghreb ~ "Maghreb",
                country %in% country_groups$Mashreq ~ "Mashreq",
                country %in% mena_countries ~ "MENA",
                TRUE ~ "Other"
              )) %>%
      dplyr::select(-id) %>%
      filter(!is.na(Urban_centre)) %>% 
      filter(!is.na(Built_rel_change)) %>% 
      distinct()

# built_ucdb %>% filter(is.na(Urban_centre)) %>% View() #NA UC!

# OE-----
oe_data <- data
names(oe_data)

oe_data <- oe_data %>% filter(Country!=Location) 

oe_data_shi <- oe_data %>%
  # Filter the data for the required years (2011-2019)
  filter(Year >= 2001 & Year <= 2021) %>%
  # Create comparator groups
  # mutate(Region2 = ifelse(Country %in% mena_countries, "MENA", "Other")) %>%
  # mutate(Comparators = case_when(
  #   Country %in% country_groups$GCC ~ "GCC",
  #   Country %in% country_groups$Maghreb ~ "Maghreb",
  #   Country %in% country_groups$Mashreq ~ "Mashreq",
  #   Country %in% mena_countries ~ "MENA",
  #   TRUE ~ "Other"
  # )) %>%
  # left_join(comparators_ucdb, by = c("Country")) %>%
  # Employment -----------------------------------------------------------------
  mutate(across(c("EMPO_Q"
                  , "EMPB_F"
                  , "EMPK_N"
                  , "EMPGIR_U"
                  , "EMPA"
                  , "EMPHJ"
                  , "GDPTOTPPPC"
                  , "POPTOTT"), as.numeric)) %>%
  mutate(GDP_per_capita_PPP = GDPTOTPPPC / POPTOTT) %>% 
  mutate(
    Public_Services_Emp_Pct = EMPO_Q / EMPTOTT,
    Industry_Emp_Pct = EMPB_F / EMPTOTT,
    Financial_Busines_Services_Emp_Pct = EMPK_N / EMPTOTT,
    Consumer_Services_Emp_Pct = EMPGIR_U / EMPTOTT,
    Agriculture_Emp_Pct = EMPA / EMPTOTT,
    Transport_Information_Communic_Services_Emp_Pct = EMPHJ / EMPTOTT
  ) %>% 
  # GVA ------------------------------------------------------------------------
  mutate(across(c("GVATOTPPPC"
                  ,"GVAGIR_UPPPC"
                  , "GVAAPPPC"
                  , "GVAK_NPPPC"
                  , "GVAB_FPPPC"
                  , "GVAO_QPPPC"
                  , "GVAHJPPPC"),as.numeric)) %>%
  mutate(Agriculture_GVA_Pct = GVAAPPPC / GVATOTPPPC
           , Consumer_Services_GVA_Pct = GVAGIR_UPPPC / GVATOTPPPC
           , Financial_Business_Services_GVA_Pct = GVAK_NPPPC / GVATOTPPPC
           , Industry_GVA_Pct = GVAB_FPPPC / GVATOTPPPC          
           , Public_Services_GVA_Pct =  GVAO_QPPPC / GVATOTPPPC 
           , Transport_Information_Communic_Services_GVA_Pct =  GVAHJPPPC / GVATOTPPPC
    ) %>% 
 # Variable selection ----------------------------------------------------------
  dplyr::select(
    Location, Country, Year, GDP_per_capita_PPP
    # , Region2
    # , Comparators
    # , WB_income_group
    # , Region
    ,Transport_Information_Communic_Services_Emp_Pct, Agriculture_Emp_Pct, Consumer_Services_Emp_Pct
    ,Financial_Busines_Services_Emp_Pct, Industry_Emp_Pct, Public_Services_Emp_Pct
    ,Agriculture_GVA_Pct, Consumer_services_GVA_Pct, Financial_business_services_GVA_Pct
    ,Industry_GVA_Pct, Public_Services_GVA_Pct, Transport_Information_Communic_Services_GVA_Pct
  ) %>% 
  dplyr::filter(!is.na(GDP_per_capita_PPP)) %>% 
  dplyr::filter(if_all(-Location, ~!is.na(.)))


# oe_data_shi %>% filter(is.na(WB_income_group)) %>% View() #Must be empty

oe_data_shi %>% summarise(across(everything(), ~sum(is.na(.)))) 
oe_data_shi %>% filter(if_any(-Location, is.na)) %>%
                distinct(Location)

# oe_data_shi$GDP_per_capita_PPP <- comma(oe_data_shi$GDP_per_capita_PPP * 1000, accuracy = 0.1) creates problems when plotting in shiny

# Export data to shiny project

save(oe_data_shi, file = here("Output", "shiny_data.RData")) #Last update 2025-07-07 
save(built_ucdb, file = here("Output", "built_ucdb.RData"))



