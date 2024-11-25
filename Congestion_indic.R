


# Congestion indicators -----
require(naniar)

# Run other scripts for main databases
if(all(sapply(c("oe_mena", "ucdb_mena_cities", "ghsl_data_mena"), exists)))
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

missing_patterns <- oe_mena_cong %>%
  group_by(Year, Location) %>%
  miss_var_summary() %>%
  arrange(desc(n_miss))

gg_miss_var(ghsl_data, facet = Year) +
  theme_minimal() +
  labs(title = "Missing Values by Variable and Year")