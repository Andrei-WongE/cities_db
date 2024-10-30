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

# Delete unnecessary rows
data <- data[-c(1:7), ]

# Clean variables
data <- data %>% 
  mutate(Location = trimws(sub(" - Total$", "", Location))) # National level includes this word

# Create variables, National GDP if Country==Location
data <- data %>%
  group_by(Country, Year) %>%
  mutate(National_level = if_else(Location == Country, 1, 0)) %>%
  mutate(
    GDPTOTPPPC = if_else(is.na(as.numeric(as.character(GDPTOTPPPC))), NA_real_, as.numeric(as.character(GDPTOTPPPC))),
    EMPTOTT = if_else(is.na(as.numeric(as.character(EMPTOTT))), NA_real_, as.numeric(as.character(EMPTOTT))),
    National_GDP = ifelse(any(Location == Country & !is.na(GDPTOTPPPC)), 
                          GDPTOTPPPC[Location == Country & !is.na(GDPTOTPPPC)], 
                          NA_real_),
    National_EMP = ifelse(any(Location == Country & !is.na(EMPTOTT)), 
                          EMPTOTT[Location == Country & !is.na(EMPTOTT)], 
                          NA_real_)
  ) %>%
  ungroup() %>%
  mutate(
    City_GDP_share = if_else(Location == Country | is.na(GDPTOTPPPC) | is.na(National_GDP), 
                             NA_real_, 
                             GDPTOTPPPC / National_GDP),
    City_EMP_share = if_else(Location == Country | is.na(EMPTOTT) | is.na(National_EMP), 
                             NA_real_, 
                             EMPTOTT / National_EMP)
  )
