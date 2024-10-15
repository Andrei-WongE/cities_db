# #### Request ----
require(readr)
require(here)
require(dplyr)
require(tidyr)
require(stringr)

show_in_excel <- function(.data){
  
require(writexl)

tmp <- paste0(tempfile(), ".csv")

writexl::write_xlsx(.data, tmp)

#fs:: file_show(path = tmp)
browseURL(tmp)
}

# Function to find partial matches
find_partial_matches <- function(x, choices) {
  sapply(x, function(i) {
    matches <- stringr::str_detect(choices, regex(i, ignore_case = TRUE)) |
      stringr::str_detect(i, regex(choices, ignore_case = TRUE))
    if (any(matches)) choices[matches] else NA_character_
  })
}

#  Function to remove all-NA columns and report dropped variables
remove_all_na_columns <- function(df) {
  # Store the original column names
  original_cols <- names(df)
  
  # Remove the all-NA columns
  df_cleaned <- df %>% dplyr::select(where(~ !all(is.na(.))))
  
  # Identify dropped columns
  dropped_vars <- setdiff(original_cols, names(df_cleaned))
  
  # Return both the cleaned dataframe and info about dropped variables
  return(list(
    cleaned_data = df_cleaned,
    dropped_variables = dropped_vars
  ))
}

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
# data %>% filter(Location=="Colombo") %>%
#   dplyr::select(where(~ !all(is.na(.)))) %>%
#   colnames(.)

mena_capitals <- c(
  "Algiers",         # Algeria
  "Manama",          # Bahrain
  "Cairo",           # Egypt
  "Tehran",          # Iran
  "Baghdad",         # Iraq
  "Jerusalem",       # Israel (Note: Status disputed, Tel Aviv hosts most embassies)
  "Amman",           # Jordan
  "Kuwait City",     # Kuwait
  "Beirut",          # Lebanon
  "Tripoli",         # Libya
  "Nouakchott",      # Mauritania
  "Rabat",           # Morocco
  "Muscat",          # Oman
  "Doha",            # Qatar
  "Riyadh",          # Saudi Arabia
  "Damascus",        # Syria
  "Tunis",           # Tunisia
  "Abu Dhabi",       # United Arab Emirates
  "Sana'a",          # Yemen (Note: De facto capital is currently Aden due to ongoing conflict)
  "Ramallah"         # Palestine (Note: Status disputed, de facto administrative capital)
)

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
  "United Arab Emirates",
  "Yemen",
  "Palestine"
)

# ### qs_rankings----
qs_rankings <- read_csv(here("Data", "qs_rankings_CCKSB.csv"))
colnames(qs_rankings)[colnames(qs_rankings) == "...1"] <- "id"

# data_colombo2 <- qs_rankings %>% filter(location=="Sri Lanka") %>%
#   dplyr::select(where(~ !all(is.na(.))))
# 
# show_in_excel(data_colombo2)

qs_rankings_mena <- qs_rankings %>% 
  mutate(matched_country = find_partial_matches(location, mena_countries)) %>%
  filter(!is.na(matched_country)) 

mismatches <- qs_rankings_mena %>%
  filter(location != matched_country) %>%
  dplyr::select(location, matched_country) %>%
  distinct() %>% 
  print()

qs_rankings_mena <- qs_rankings_mena %>% 
                    filter(location != "Romania")

qs_rankings_mena %>% distinct(.$location) %>% View()

remove_all_na_columns(qs_rankings_mena)

# Find cities where the universities are located, using Claude AI:
# Create a data frame of Middle Eastern Universities and their locations

# First, create the data as a list of vectors
universities_data <- list(
  c("King Abdulaziz University (KAU)", "Jeddah", "Saudi Arabia"),
  c("King Fahd University of Petroleum & Minerals", "Dhahran", "Saudi Arabia"),
  c("Khalifa University of Science and Technology", "Abu Dhabi", "United Arab Emirates"),
  c("Qatar University", "Doha", "Qatar"),
  c("The Hebrew University of Jerusalem", "Jerusalem", "Israel"),
  c("King Saud University", "Riyadh", "Saudi Arabia"),
  c("American University of Beirut (AUB)", "Beirut", "Lebanon"),
  c("Tel Aviv University", "Tel Aviv", "Israel"),
  c("United Arab Emirates University", "Al Ain", "United Arab Emirates"),
  c("American University of Sharjah", "Sharjah", "United Arab Emirates"),
  c("Sharif University of Technology", "Tehran", "Iran"),
  c("Sultan Qaboos University", "Muscat", "Oman"),
  c("Technion - Israel Institute of Technology", "Haifa", "Israel"),
  c("The American University in Cairo", "Cairo", "Egypt"),
  c("Amirkabir University of Technology", "Tehran", "Iran"),
  c("Umm Al-Qura University", "Mecca", "Saudi Arabia"),
  c("Imam Abdulrahman Bin Faisal University (IAU)", "Dammam", "Saudi Arabia"),
  c("Ben-Gurion University of The Negev", "Be'er Sheva", "Israel"),
  c("Bar-Ilan University", "Ramat Gan", "Israel"),
  c("University of Tehran", "Tehran", "Iran"),
  c("Iran University of Science and Technology", "Tehran", "Iran"),
  c("Saint Joseph University of Beirut (USJ)", "Beirut", "Lebanon"),
  c("University of Balamand", "Koura", "Lebanon"),
  c("Canadian University Dubai", "Dubai", "United Arab Emirates"),
  c("Cairo University", "Giza", "Egypt"),
  c("Applied Science University", "Manama", "Bahrain"),
  c("Holy Spirit University of Kaslik", "Kaslik", "Lebanon"),
  c("University of Jordan", "Amman", "Jordan"),
  c("Al Ain University", "Al Ain", "United Arab Emirates"),
  c("Lebanese American University", "Beirut and Byblos", "Lebanon"),
  c("Lebanese University", "Multiple locations across Lebanon", "Lebanon"),
  c("University of Sharjah", "Sharjah", "United Arab Emirates"),
  c("Abu Dhabi University", "Abu Dhabi", "United Arab Emirates"),
  c("Ahlia University", "Manama", "Bahrain"),
  c("Ajman University", "Ajman", "United Arab Emirates"),
  c("Alfaisal University", "Riyadh", "Saudi Arabia"),
  c("American University in Dubai", "Dubai", "United Arab Emirates"),
  c("Prince Mohammad Bin Fahd University", "Al Khobar", "Saudi Arabia"),
  c("American University of the Middle East", "Egaila", "Kuwait"),
  c("Jouf University", "Sakaka", "Saudi Arabia"),
  c("King Khalid University", "Abha", "Saudi Arabia"),
  c("Princess Nourah bint Abdulrahman University", "Riyadh", "Saudi Arabia"),
  c("University of Haifa", "Haifa", "Israel"),
  c("Zayed University", "Abu Dhabi and Dubai", "United Arab Emirates"),
  c("Shiraz University", "Shiraz", "Iran"),
  c("Université de Sousse", "Sousse", "Tunisia"),
  c("Ain Shams University", "Cairo", "Egypt"),
  c("Beirut Arab University", "Beirut", "Lebanon"),
  c("German Jordanian University", "Amman", "Jordan"),
  c("Gulf University for Science and Technology", "Kuwait City", "Kuwait"),
  c("Islamic University of Madinah", "Medina", "Saudi Arabia"),
  c("Jordan University of Science & Technology", "Irbid", "Jordan"),
  c("King Faisal University", "Al-Ahsa", "Saudi Arabia"),
  c("Northern Borders University", "Arar", "Saudi Arabia"),
  c("Notre Dame University-Louaize NDU", "Zouk Mosbeh", "Lebanon"),
  c("Princess Sumaya University for Technology", "Amman", "Jordan"),
  c("Qassim University", "Buraydah", "Saudi Arabia"),
  c("University of Baghdad", "Baghdad", "Iraq"),
  c("University of Bahrain", "Sakhir", "Bahrain"),
  c("University of Dubai", "Dubai", "United Arab Emirates"),
  c("Alexandria University", "Alexandria", "Egypt"),
  c("Assiut University", "Asyut", "Egypt"),
  c("Future University in Egypt", "New Cairo", "Egypt"),
  c("Imam Mohammad Ibn Saud Islamic University - IMSIU", "Riyadh", "Saudi Arabia"),
  c("Kuwait University", "Kuwait City", "Kuwait"),
  c("Mustansiriyah University", "Baghdad", "Iraq"),
  c("Mutah University", "Karak", "Jordan"),
  c("Shahid Beheshti University (SBU)", "Tehran", "Iran"),
  c("Taibah University", "Medina", "Saudi Arabia"),
  c("University of Kufa", "Kufa", "Iraq"),
  c("Al-Azhar University", "Cairo", "Egypt"),
  c("Al-Balqa Applied University", "As-Salt", "Jordan"),
  c("British University in Egypt", "El Sherouk City", "Egypt"),
  c("German University in Cairo", "New Cairo", "Egypt"),
  c("Helwan University", "Helwan", "Egypt"),
  c("Mansoura University", "Mansoura", "Egypt"),
  c("Suez Canal University", "Ismailia", "Egypt"),
  c("Tanta University", "Tanta", "Egypt"),
  c("The Hashemite University", "Zarqa", "Jordan"),
  c("Université de Tunis", "Tunis", "Tunisia"),
  c("Université de Tunis El Manar", "Tunis", "Tunisia"),
  c("University of Babylon", "Hillah", "Iraq"),
  c("University of Basrah", "Basra", "Iraq"),
  c("Yarmouk University", "Irbid", "Jordan"),
  c("Zagazig University", "Zagazig", "Egypt"),
  c("Damascus University", "Damascus", "Syria"),
  c("Université Mohammed V de Rabat", "Rabat", "Morocco")
)

# Convert the list to a data frame
universities_df <- data.frame(do.call(rbind, universities_data))

colnames(universities_df) <- c("University", "City", "Country")

qs_rankings_mena_merged <- merge(qs_rankings_mena, universities_df, 
                   by.x = "institution", 
                   by.y = "University", 
                   all.x = TRUE, 
                   suffixes = c("_qs", "_our"))

# Unmatched cases
unmatched <- qs_rankings_mena_merged[is.na(qs_rankings_mena_merged$City),]
print(paste("Number of unmatched universities:", nrow(unmatched)))

if(nrow(unmatched) > 0) {
  print("Unmatched universities:")
  print(unmatched$institution)
}



# ### wb_doingbusiness----
wb_doingbusiness <- read_csv(here("Data", "WB_doingbusiness_CCKSB.csv")) %>%
  rename(Location = `Location 1\r\n(city, port)`)

# data_colombo3 <- wb_doingbusiness %>% filter(Economy=="Sri Lanka") %>%
#   dplyr::select(where(~ !all(is.na(.))))
# 
# show_in_excel(data_colombo3)

wbdb_mena <- wb_doingbusiness %>%
  mutate(city = Location) %>% 
  mutate(location = Economy) %>% 
  mutate(matched_country = find_partial_matches(location, mena_countries)) %>%
  filter(!is.na(matched_country)) 

mismatches <- wbdb_mena %>%
  filter(location != matched_country) %>%
  dplyr::select(location, matched_country) %>%
  distinct() %>% 
  print()

wbdb_mena <- wbdb_mena %>% 
  filter(location != "Romania")

wbdb_mena %>% distinct(.$location) %>% View()

wbdb_mena <- remove_all_na_columns(wbdb_mena)
  
uwbdb_mena_cities <- as_tibble(wbdb_mena$cleaned_data) %>%
  mutate(matched_city = find_partial_matches(city, mena_capitals))

wbdb_mena_cities %>% distinct(.$city) %>% View()

# ### UCDB----
ucdb_full_vars <- read_csv(here("Data","UCDB_CCKSB_full_vars.csv")) %>%
  rename(Location = loc_latin)

# data_colombo4 <- ucdb_full_vars %>% filter(CTR_MN_NM=="Sri Lanka") %>%
#   dplyr::select(where(~ !all(is.na(.)))) %>%
#   filter(loc_latin != "Sammanturai") # Row contains all missing data
# 
# show_in_excel(data_colombo4)
# 
# data_ucdb <- ucdb_full_vars %>% filter(Location %in% cities)

ucdb_mena <- ucdb_full_vars %>%
  mutate(city = Location) %>% 
  mutate(location = country_latin) %>%
  mutate(matched_country = find_partial_matches(location, mena_countries)) %>%
  filter(!is.na(matched_country))

mismatches <- ucdb_mena %>%
  filter(location != matched_country) %>%
  dplyr::select(location, matched_country) %>%
  distinct() %>% 
  print()

ucdb_mena <- ucdb_mena %>% 
  filter(location != "Romania")

ucdb_mena %>% distinct(.$location) %>% View()

ucdb_mena <- remove_all_na_columns(ucdb_mena) 

ucdb_mena_cities <- as_tibble(ucdb_mena$cleaned_data) %>%
  mutate(matched_city = find_partial_matches(city, mena_capitals))
  
ucdb_mena_cities %>% distinct(.$city) %>% View()

# Statistics -----
# labels_vector <- setNames(labels$Label, labels$Variable)
# data_merged <- setNames(data_merged, labels_vector[names(data_merged)])
