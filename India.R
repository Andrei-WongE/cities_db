# Request for Indian cities data

# Load packages and data, see Main file

if(!exists("data")) stop("Data not found")

source("Plot_functions_V1.R")

# Create output folder
dir.create("Output/India", showWarnings = FALSE)

# Filter data for India
oe_india <- data %>% filter(Country == "India")

oe_india %>% distinct(.$Location) %>% View() #73

# Plot graphs
# Cities: All cities in OE.

# Charts, all cities
# 
# 1 chart, with bar chart of population in decreasing order, for 2019.  (for first three try to make sure the labels are readable)
# 1 chart, with bar chart of GDP (real, PPP) in decreasing order, for 2019.
# 1 chart, with bar chart of GDP per capita in decreasing order, for 2019.
# 1 chart, with line chart of total GDP growth 2000-2019, highlight highest grower and worst performer, and focal cities
# Distance for frontiers, all cities in the dataset, color Indian cities, color for each size band of population with top 3% (use orange for Indian cities, and bright green for the frontier), the other think you can do here is estimate simple regressions of Y=Bp+r (where Y is GDP, and p is population) – you estimate B using regression  for only Indian cities, and B using regressions for only frontier cities, and I can use this to estimate the “distance to fronteer”.)
# Label cities that are standing out.
# 
 
# Charts, focus and comparators
# 
 
# For this group keep consistent color coding
# 
# - 1 chart, with bar chart of population in decreasing order, for 2019.
# - 1 chart, with bar chart of GDP (real, PPP) in decreasing order, for 2019.
# 
# - 1 chart, with bar chart of GDP per capita in decreasing order, for 2019.
# 
# - 1 chart, with line chart of total GDP growth 2000-2019, highlight highest grower and worst performer, and focal cities
# 
# 1 chart, bar chart 100%, structure of GVA
# 10 charts (or however many cities and comparators we get) , stacked plot, change in GVA, 2000-2019

# General comparisson charts
## Population
oe_india <- oe_india %>% 
  mutate(POPTOTT = as.numeric(POPTOTT))
  
generate_bar_plot(
  data = oe_india,
  years = "2019",
  variable_name = "POPTOTT",
  title = "Total population selected cities in India",
  subtitle = "2019",
  source_text = "Source: Oxford City Dabase, 2022",
)

## GDP

## GDP per capita

## GDP growth

## Frontier distance