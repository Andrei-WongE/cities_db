# #### Request ----
# require(readr)
# 
# show_in_excel <- function(.data){
# 
# tmp <- paste0(tempfile(), ".csv")
# 
# write_xlsx(.data, tmp)
# 
# #fs:: file_show(path = tmp)
# browseURL(tmp) 
# }
# 
# 
# data %>% filter(Country=="Sri Lanka") %>% View(.)
# 
# colnames(data) <- labels_vector[colnames(data)]
# 
# data_colombo <- data %>% filter(Location=="Colombo") %>% 
#   dplyr::select(where(~ !all(is.na(.)))) 
# 
# show_in_excel(data_colombo)
# 
# labels_vector <- setNames(labels$Label, labels$Variable)
# data_colombo_labeled <- setNames(data_colombo, labels_vector[names(data_colombo)])
# 
# write_xlsx(data_colombo_labeled, here("Data", "data_colombo.xlsx"), col_names = TRUE)
# 
# 
# 
# data %>% filter(Location=="Colombo") %>% 
#   dplyr::select(where(~ !all(is.na(.)))) %>%
#   colnames(.)
# 
# 
# qs_rankings <- read_csv(here("Data", "qs_rankings_CCKSB.csv"))
# colnames(qs_rankings)[colnames(qs_rankings) == "...1"] <- "id"
# 
# data_colombo2 <- qs_rankings %>% filter(location=="Sri Lanka") %>% 
#   dplyr::select(where(~ !all(is.na(.)))) 
# 
# show_in_excel(data_colombo2)
# 
# 
# ### wb_doingbusiness----
# wb_doingbusiness <- read_csv(here("Data", "WB_doingbusiness_CCKSB.csv")) %>% 
#   rename(Location = `Location 1\r\n(city, port)`)
# 
# data_colombo3 <- wb_doingbusiness %>% filter(Economy=="Sri Lanka") %>% 
#   dplyr::select(where(~ !all(is.na(.)))) 
# 
# show_in_excel(data_colombo3)
# 
# 
# cities <- c("Colombo", "Mumbai", "Chennai", "Kochi", "Chittagong", "Karachi", "Ho Chi Minh City", "Surabaya",
#                "Singapore", "Bangkok", "Kuala Lumpur", "Hangzhou", "Quingdao", "Tianjin", "Xiamen")
# 
# data_wbdb <- wb_doingbusiness %>% filter(Location %in% cities) 
# 
# 
# 
# ### UCDB----
# ucdb_full_vars <- read_csv(here("Data","UCDB_CCKSB_full_vars.csv")) %>% 
#   rename(Location = loc_latin)
# 
# data_colombo4 <- ucdb_full_vars %>% filter(CTR_MN_NM=="Sri Lanka") %>% 
#   dplyr::select(where(~ !all(is.na(.)))) %>% 
#   filter(loc_latin != "Sammanturai") # Row contains all missing data
# 
# 
# show_in_excel(data_colombo4)
# 
# data_ucdb <- ucdb_full_vars %>% filter(Location %in% cities) 

# Statistics -----
# labels_vector <- setNames(labels$Label, labels$Variable)
# data_merged <- setNames(data_merged, labels_vector[names(data_merged)])
