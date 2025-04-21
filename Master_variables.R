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
## Date Created: 2024-10-28
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

# install.packages(
#   "paint", 
#   repos = c(mm = "https://milesmcbain.r-universe.dev", getOption("repos")))

library("pacman")
library("here")
library("groundhog")

set.groundhog.folder(here("groundhog_library"))
groundhog.day = "2024-04-25" #"2020-05-12"
#Dowloaded fromn https://github.com/CredibilityLab/groundhog

pkgs = c("dplyr", "tidyverse", "janitor", "sf"
         , "ggplot2","xfun", "remotes", "sp", "spdep"
         , "foreach", "doParallel", "parallel", "progressr"
         , "doSNOW", "purrr", "patchwork"
         , "haven", "openxlsx", "MASS", "reticulate"
         , "future", "furrr", "data.table","leaflet"
         , "jtools", "tidyr", "ggspatial", "raster"
         , "prettymapr", "viridis", "labelled"
         , "writexl", "WDI", "wesanderson", "ggrepel",
         "ggbreak", "naniar", "fuzzyjoin", "ggpmisc"
)

groundhog.library(pkgs, groundhog.day
                  # , ignore.deps =  "fs"
                  )

#maptools removed fron CRAN      
# install.packages(c("systemfonts", "textshaping"), dependencies = TRUE)

## Program Set-up ------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
sf_use_s2(TRUE) # Use s2 spherical geometry for geographical coordinate operations 

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

# Create Region variable


