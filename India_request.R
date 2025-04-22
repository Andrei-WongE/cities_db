# Request for Indian cities data 10/04/2025

# Load packages and data, see Main file
require(here)
require(stringdist)
require(stringr)
require(ggrepel)
require(haven)
require(rlang)

source(here("Master_variables.R"))

if(!exists("data")) stop("Data not found")

source("Plot_functions_V1.R")
source("plot_function.R")


# Create output folder
dir.create("Output/India_02", showWarnings = FALSE)
dir.create("Output/India_03", showWarnings = FALSE)

# Plot functions ----

create_visualizations_line <- function(data, year_range = c(2005, 2021), 
                                       main_var = "Population", 
                                       unit = "Thousands",
                                       categories = "mission_categories",
                                       output_dir = here::here("Output", "India_02")) {
  
  # Data preparation
  data_filtered <- data %>%
    filter(between(Year, year_range[1], year_range[2])) %>%
    mutate(Year = as.numeric(Year)) %>%
    dplyr::select(Location, Year, all_of(categories), all_of(main_var)) %>%
    rename(Value = !!sym(main_var),
           categories = !!sym(categories)) %>%  # Rename the column to "categories" for consistent processing
    mutate(Value = as.numeric(Value))  # Ensure numeric conversion
  
  # Function to create plots for each category
  create_category_plot <- function(cat_data) {
    
    category <- unique(cat_data$categories)
    
    start_year <- min(cat_data$Year)
    end_year <- max(cat_data$Year)
    
    label_years <- seq(start_year, end_year, by = 2)
    
    cat_data_labeled <- cat_data %>%
      mutate(show_label = Year %in% label_years,
             ValueLabel = scales::comma(Value, accuracy = 1))
    
    # Generate plot
    title_text <- paste(main_var, "Trend in", category, paste0("(", start_year, "-", end_year, ")"))
    y_axis_label <- paste0(main_var, " (", unit, ")")
    
    
    # Get data for the last point of each location to add location labels
    last_points <- cat_data %>%
      group_by(Location) %>%
      filter(Year == max(Year)) %>%
      ungroup()
    
    p <- ggplot(cat_data, aes(x = Year, y = Value, color = Location, group = Location)) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point(size = 3) +
      geom_text_repel(data = cat_data_labeled %>% filter(show_label),
                      aes(label = ValueLabel), 
                      size = 3.5, 
                      box.padding = 0.5, 
                      point.padding = 0.5) +
      # Add location labels at the end of each line
      geom_text_repel(data = last_points,
                      aes(label = Location),
                      nudge_x = 1.2,
                      hjust = 0,
                      direction = "y",
                      segment.color = NA,
                      size = 4,
                      fontface = "bold") +
      scale_color_manual(values = wes_palette("Zissou1", n = length(unique(cat_data$Location)), 
                                              type = "continuous")) +
      labs(
        title = gsub("_", " ", title_text),
        x = "Year",
        y = gsub("_", " ", y_axis_label),
        color = "City",
        caption = paste0("Source: Oxford City Database, 2022")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 0.8, size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(
          size = 8,
          hjust = 0,
          margin = margin(t = 20),
          color = "gray30"
        )
      ) +
      scale_y_continuous(labels = comma_format()) +
      scale_x_continuous(breaks = seq(year_range[1], year_range[2], by = 2))
    
    filename <- paste0(main_var, "_Trends_", gsub("_", " ", gsub(" ", "_", category)), 
                       "_", start_year, "-", end_year, ".png")
    
    ggsave(filename = file.path(output_dir, filename),
           plot = p,
           width = 18,
           height = 10,
           dpi = 600)
    
    return(p)
  }
  
  # Generate plots for each category
  plots <- data_filtered %>%
    group_by(categories) %>%
    group_map(~ create_category_plot(.x), .keep = TRUE)
  
  return(plots)
}

create_visualizations_bar <- function(data, year_range = c(2001, 2019), 
                                      main_var = "GVA", 
                                      categories = "mission_categories", 
                                      output_dir = here::here("Output", output_dir)) {
  
  # Data Preparation
  bar_data <- data %>%
    filter(between(Year, year_range[1], year_range[2])) %>%
    pivot_longer(
      cols = ends_with(paste0("_", main_var, "_Pct")), 
      names_to = "Sector", 
      values_to = "Percentage"
    ) %>%
    mutate(
      Sector = str_remove(Sector, paste0("_", main_var, "_Pct")),
      Sector = str_replace_all(Sector, "_", " "),
      Year = as.numeric(Year)
    ) %>% 
    rename(categories = !!sym(categories))
  
  # Function to create individual plots
  create_category_plot <- function(cat_data) {
    
    category_name <- unique(cat_data$categories)
    start_year <- min(cat_data$Year)
    end_year <- max(cat_data$Year)
    
    # Prepare label data with correct positioning
    label_data <- cat_data %>%
      group_by(Year) %>%
      arrange(Year, desc(Sector)) %>%  # Important: consistent ordering
      mutate(
        # Calculate cumulative percentages for area positions
        ymax = cumsum(Percentage),
        ymin = lag(ymax, default = 0),
        pos =  pmin(ymin + (ymax - ymin) * 0.5, ymax - 0.02),  # Center position for labels
        perc = scales::percent(Percentage, accuracy = 0.1)
      ) %>%
      ungroup()
    
    title_text <- if (main_var == "EMP") {
      paste("Employment Sectorial Contribution in",  gsub("_", " ", category_name), paste0("(", start_year, "-", end_year, ")"))
    } else {
      paste(main_var, "Sectorial Contribution in",  gsub("_", " ", category_name), paste0("(", start_year, "-", end_year, ")"))
    }
    
    # Generate stacked bar plot with locations grouped in each category
    p <- ggplot(cat_data, aes(x = Location, y = Percentage, fill = Sector)) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
      scale_fill_manual(
        values = wes_palette("Zissou1", n = length(unique(cat_data$Sector)), type = "continuous")
      ) +
      labs(
        title = title_text,
        x = "City",
        y = "Percentage",
        fill = "Sector",
        caption = paste0("Source: Oxford City Database, 2022")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(
          size = 8,
          hjust = 0,
          margin = margin(t = 20),
          color = "gray30")
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      geom_text(data = label_data, 
                aes(x = Location
                    # , y = pmin(pos, 1)
                    , label = perc),
                position = position_stack(vjust = 0.5), 
                size = 5, fontface = "bold", color = "white", check_overlap = FALSE)
    
    filename <- paste0(main_var, "_Sector_Bar_", start_year, "-", end_year, "_", gsub(" ", "_", category_name), ".png")
    ggsave(file.path(output_dir, filename)
           , plot = p
           , width = 18
           , height = 10
           , dpi = 600)
    
    return(p)
  }
  
  # Generate plots for each category efficiently
  plots <- bar_data %>%
    group_by(categories) %>%
    group_map(~ create_category_plot(.x), .keep = TRUE)
  
  return(plots)
}


create_visualizations_bar_dodge <- function(data, year_range = c(2001, 2019), 
                                            main_var = "GVA", 
                                            categories = "mission_categories", 
                                            output_dir = here::here("Output", "India_02")) {
  
  # Data Preparation
  bar_data <- data %>%
    filter(between(Year, year_range[1], year_range[2])) %>%
    pivot_longer(
      cols = ends_with(paste0("_", main_var, "_Pct")), 
      names_to = "Sector", 
      values_to = "Percentage"
    ) %>%
    mutate(
      Sector = str_remove(Sector, paste0("_", main_var, "_Pct")),
      Sector = str_replace_all(Sector, "_", " "),
      Year = as.numeric(Year)
    ) %>% 
    rename(categories = !!sym(categories))
  
  # Function to create individual plots
  create_category_plot <- function(cat_data) {
    
    category_name <- unique(cat_data$categories)
    start_year <- min(cat_data$Year)
    end_year <- max(cat_data$Year)
    
    # Generate stacked bar plot with locations grouped in each category
    p <- ggplot(cat_data, aes(x = Location, y = Percentage, fill = Sector)) +
      # geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
      scale_fill_manual(
        values = wes_palette("Zissou1", n = length(unique(cat_data$Sector)), type = "continuous")
      ) +
      labs(
        title = paste("GVA per Worker Sectorial Contribution in", gsub("_", " ",category_name), "(", start_year, "-", end_year, ")"),
        x = NULL,
        y = "Nominal Value (Thousands)",
        fill = "Sector",
        caption = paste0("Source: Oxford City Database, 2022") 
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(
          size = 8,
          hjust = 0,
          margin = margin(t = 20),
          color = "gray30")
      ) +
      scale_y_continuous(labels = scales::number_format()) +
      geom_text(
        aes(x = Location, y = Percentage, label = scales::comma(round(Percentage))),
        position = position_dodge(width = 0.9), 
        vjust = -0.5,   
        size = 3.5,
        fontface = "bold"
      )
    
    filename <- paste0(main_var, "_Sector_Bar_", start_year, "-", end_year, "_", gsub(" ", "_", category_name), ".png")
    ggsave(file.path(output_dir, filename)
           , plot = p
           , width = 18
           , height = 10
           , dpi = 600)
    
    return(p)
  }
  
  # Generate plots for each category efficiently
  plots <- bar_data %>%
    group_by(categories) %>%
    group_map(~ create_category_plot(.x), .keep = TRUE)
  
  return(plots)
}

create_area_visualizations <- function(data, year_range = c(2005, 2019), 
                                       main_var = "GVA",
                                       output_dir = here::here("Output", output_dir)) {
  
  
  # Data preparation
  pie_data <- data %>%
    pivot_longer(
      cols = ends_with(paste0("_",main_var, "_Pct")), 
      names_to = "Sector", 
      values_to = "Percentage"
    ) %>%
    filter(between(Year, year_range[1], year_range[2])) %>%
    mutate(
      Sector = str_remove(Sector, paste0("_",main_var, "_Pct")),
      Sector = str_replace_all(Sector, "_", " "),
      Year = as.numeric(Year)
    )
  
  # Function to create single location plot
  create_location_plot <- function(loc_data) {
    # Get location name
    location <- unique(loc_data$Location)
    
    # Get years for labels
    start_year <- min(loc_data$Year)
    end_year <- max(loc_data$Year)
    
    # Prepare label data with correct positioning
    label_data <- loc_data %>%
      group_by(Year) %>%
      arrange(Year, desc(Sector)) %>%  # Important: consistent ordering
      mutate(
        # Calculate cumulative percentages for area positions
        ymax = cumsum(Percentage),
        ymin = lag(ymax, default = 0),
        pos =  ymin + (Percentage /2),  # Center position for labels
        perc = scales::percent(Percentage, accuracy = 0.1)
      ) %>%
      ungroup()
    
    title_text <- if (main_var == "EMP") {
      paste("Employment Sectorial Contribution in",  gsub("_", " ", location), paste0("(", start_year, "-", end_year, ")"))
    } else {
      paste(main_var, "Sectorial Contribution in",  gsub("_", " ", location), paste0("(", start_year, "-", end_year, ")"))
    }
    
    # Create plot based on the number of years
    if (length(unique(loc_data$Year)) == 1) {
      # Single year: create a stacked bar chart
      p <- ggplot(loc_data, aes(x = factor(Year), y = Percentage, fill = Sector)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        scale_fill_manual(
          values = c(
            wes_palette("Zissou1", n = length(unique(loc_data$Sector)), 
                        type = "continuous"), 
            "#D3D3D3"
          )
        ) +
        labs(
          title = title_text,
          x = NULL,
          y = "Percentage",
          fill = "Sector",
          caption = paste0("Source: Oxford City Database, 2022") 
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank(),
          plot.caption = element_text(
            size = 8,
            hjust = 0,
            margin = margin(t = 20),
            color = "gray30")
        ) +
        scale_y_continuous(labels = scales::percent_format(), limit = c(0,1)) +
        geom_text( data = label_data,
                   aes(x = factor(Year), y = pos, label = perc, group = Sector),
                   size = 5,
                   fontface = "bold"
        ) 
      
    } else {
      
      # Multiple years: create an area chart
      p <- ggplot(loc_data, aes(x = Year, y = Percentage, fill = Sector)) +
        geom_area(position = "fill", alpha = 0.8) +
        scale_fill_manual(
          values = c(
            wes_palette("Zissou1", n = length(unique(loc_data$Sector)), 
                        type = "continuous"), 
            "#D3D3D3"
          )
        ) +
        labs(
          title = title_text,
          x = NULL,
          y = "Percentage",
          fill = "Sector",
          caption = paste0("Source: Oxford City Database, 2022") 
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12, 
                                     face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank(),
          plot.caption = element_text(
            size = 8,
            hjust = 0,
            margin = margin(t = 20),
            color = "gray30")
        ) +
        scale_y_continuous(labels = scales::percent_format(), limit = c(0,1)) +
        scale_x_continuous(breaks = unique(loc_data$Year)) +
        # Add labels and connecting lines with corrected positioning
        geom_text(
          data = label_data %>% 
            filter(Year == start_year | Year == end_year),
          aes(x = Year, y = pos, label = perc, group = Sector,
              hjust = ifelse(Year == start_year, 1.1, -0.1)),  # Adjusted hjust
          size = 5,
          fontface = "bold"
        ) +
        geom_segment(
          data = label_data %>% 
            filter(Year == start_year | Year == end_year),
          aes(x = Year, xend = Year, y = pos, yend = ymin),
          linetype = "dotted", 
          color = "gray50"
        )
    }
    
    # Save plot
    filename <- paste0(
      main_var, "_Sector_ALL-Cities_", 
      start_year, if (end_year != start_year) paste0("-", end_year), "_",
      gsub(" ", "_", location), 
      ".png"
    )
    
    ggsave(
      filename = file.path(output_dir, filename),
      plot = p,
      width = 15,
      height = 10,
      dpi = 600
    )
    
    return(p)
  }
  
  # Create plots for each location
  plots <- pie_data %>%
    group_by(Location) %>%
    group_map(~ create_location_plot(.x), .keep = TRUE)
  
  return(plots)
}

# Plot graphs ----

# Individual cities:

# Area chart of change of employment structure over time (2005-2021)
# Area chart of change of GVA structure over time like you did before. (2005-2021)


# Groups of cities:

# a set of charts on Rajasthan cities: Jaipur, Kota, Jodhpur.
# a set of charts on Uttar Pradesh cities: Lucknow, Varanasi, Kanpur.


# For each of these two it would be great to have  (for all of these use nominal USD GDP)

# Population growth over time (2005-2021) for each group with  line charts.
# GDP growth over time (2005-2021) for each group with  line charts.
# Employment growth over time – same thing
# GDP per worker – change over time line chart (2005-2021)
# Bar chart comparing structures of GVA in 2019 (stacted bars of all cities like the ones in my e-mail below)
# Bar charts comparing structures of Employment.
# A bar chart with GVA per worker in each of the 6 sectors.


# Charts, focus and comparators:

# Larger cities:  Jaipur, Bhubaneswar,  Varanasi,  Pune, Lucknow and Kanpur, Surabaya, IND, Ho Chi Min City (VIE), Kuala Lumpur (MAL) and Tianjin (Chn)
# Smaller cities: Kota and Jodhpur, Muscat (Oman), Okayama (JPN), Pean Baru (IND), Ulsan (KOR)

# Same charts as above for these groups. 

# For each of data set find 4 fastests growing cities

data_jaipur  <- read_dta(here("Data", "India", "Jaipur.dta"))
data_jodhpur <- read_dta(here("Data", "India", "Jodhpur.dta")) 
data_kota    <- read_dta(here("Data", "India", "Kota.dta"))
  
# For Jaipur find international comparator cities
  # Port Harcourt (Nigeria)
  # Santa Cruz (Bolivia) NO, Potosi in dataset
  # Quetta (pakistan)
  # Urumqui (china)
  # Arequipa (Peru)
  # Tyumen (Russia)
  
# 5 enviromental variables, regress on all indian cities, mark selected ones
  
# Data wrangling---- 
data <- data %>% 
  mutate(Population = as.numeric(POPTOTT)) %>% 
  mutate(GDP = as.numeric(GDPTOTUSN)) %>% # Using NOMINAL GDP
  mutate(Employment = as.numeric(EMPTOTT)) %>% 
  mutate(GDP_per_capita = GDP / Population) %>%
  mutate(GDP_per_worker = GDP / Employment) %>%
  mutate(GVATOTPPPN = as.numeric(GVATOTPPPN),
         GVAAPPPN  = as.numeric(GVAAPPPN),
         GVAGIR_UPPPN = as.numeric(GVAGIR_UPPPN),
         GVAK_NPPPN = as.numeric(GVAK_NPPPN),
         GVAB_FPPPN = as.numeric(GVAB_FPPPN),
         GVAO_QPPPN = as.numeric(GVAO_QPPPN),
         GVAHJPPPN = as.numeric(GVAHJPPPN),
  ) %>%
  mutate(Agriculture_GVA_Pct = GVAAPPPN / GVATOTPPPN
         , Consumer_services_GVA_Pct = GVAGIR_UPPPN / GVATOTPPPN
         , Financial_business_services_GVA_Pct = GVAK_NPPPN / GVATOTPPPN
         , Industry_GVA_Pct = GVAB_FPPPN / GVATOTPPPN          
         , Public_services_GVA_Pct =  GVAO_QPPPN / GVATOTPPPN 
         , Transport_Information_Communic_Services_GVA_Pct =  GVAHJPPPN / GVATOTPPPN
  ) %>% 
  mutate(Total_Emp = as.numeric(EMPTOTT),
         Public_Services_Emp = as.numeric(EMPO_Q),
         Industry_Emp = as.numeric(EMPB_F),
         Financial_Business_Services_Emp = as.numeric(EMPK_N),
         Consumer_Services_Emp = as.numeric(EMPGIR_U),
         Agriculture_Emp = as.numeric(EMPA),
         Transport_Information_Communic_Services_Emp = as.numeric(EMPHJ)
         ) %>%
  mutate(Public_Services_EMP_Pct = Public_Services_Emp / Total_Emp,
         Industry_EMP_Pct = Industry_Emp / Total_Emp ,
         Financial_Business_Services_EMP_Pct = Financial_Business_Services_Emp / Total_Emp,
         Consumer_Services_EMP_Pct = Consumer_Services_Emp / Total_Emp,
         Agriculture_EMP_Pct = Agriculture_Emp / Total_Emp,
         Transport_Information_Communic_Services_EMP_Pct = Transport_Information_Communic_Services_Emp / Total_Emp
         ) %>% 
  mutate(GVATOTPPPN = as.numeric(GVATOTPPPN),
         GVAAPPPN  = as.numeric(GVAAPPPN),
         GVAGIR_UPPPN = as.numeric(GVAGIR_UPPPN),
         GVAK_NPPPN = as.numeric(GVAK_NPPPN),
         GVAB_FPPPN = as.numeric(GVAB_FPPPN),
         GVAO_QPPPN = as.numeric(GVAO_QPPPN),
         GVAHJPPPN = as.numeric(GVAHJPPPN),  #Mominal value
  ) %>%
  mutate(Total_Emp = as.numeric(EMPTOTT),
         Public_Services_Emp = as.numeric(EMPO_Q),
         Industry_Emp = as.numeric(EMPB_F),
         Financial_Business_Services_Emp = as.numeric(EMPK_N),
         Consumer_Services_Emp = as.numeric(EMPGIR_U),
         Agriculture_Emp = as.numeric(EMPA),
         Transport_Information_Communic_Services_Emp = as.numeric(EMPHJ)
         ) %>%
  mutate(Agriculture_GVApw_Pct = GVAAPPPN / Agriculture_Emp
         , Consumer_services_GVApw_Pct = GVAGIR_UPPPN / Consumer_Services_Emp
         , Financial_business_services_GVApw_Pct = GVAK_NPPPN / Financial_Business_Services_Emp
         , Industry_GVApw_Pct = GVAB_FPPPN / Industry_Emp          
         , Public_services_GVApw_Pct =  GVAO_QPPPN / Public_Services_Emp 
         , Transport_Information_Communic_Services_GVApw_Pct =  GVAHJPPPN / Transport_Information_Communic_Services_Emp
  )  %>% 
  filter(Location != Country) %>% 
  dplyr::filter(Year %in% c(2005:2021)) 


  
countries <- c("India", "Vietnam", "Malaysia", "Taiwan", "Oman", "Japan", "Indonesia", "South Korea")
cities <- c("Jaipur", "Kota", "Jodhpur", "Lucknow", "Varanasi", "Kanpur",
            "Bhubaneswar", "Pune", "Surabaya", "Ho Chi Minh City", "Kuala Lumpur",
            "Taichung", "Muscat", "Okayama MMA", "Pekan Baru", "Ulsan")

oe_india <- data %>% 
  filter(Country %in% countries & Location %in% cities)

mission_categories <- list(
  Rajasthan_cities = c("Jaipur", 
                       "Kota",
                       "Jodhpur"),
  
  Uttar_Pradesh_cities = c("Lucknow",
                           "Varanasi",
                           "Kanpur")
)

size_categories <- list(
  Larger_cities = c("Jaipur", "Pune", 
                    "Lucknow", "Kanpur", "Surabaya", "Ho Chi Minh City", 
                    "Kuala Lumpur", "Taichung"),
  
  Smaller_cities = c("Kota", "Jodhpur", "Muscat", "Bhubaneswar", "Varanasi", "Okayama MMA", 
                     "Pekan Baru", "Ulsan")
)


oe_comparators <-
  add_group_category(oe_india, 
                     categories = mission_categories,
                     var_col = "Location",
                     new_col = "mission_categories",
                     warn_unmapped = TRUE) %>% 
                     dplyr::filter(mission_categories != " ")
  
oe_comparators2 <-
  add_group_category(oe_india, 
                     categories = size_categories,
                     var_col = "Location",
                     new_col = "size_categories",
                     warn_unmapped = TRUE) %>% 
                     dplyr::filter(size_categories != " ")

# General comparison line charts -----
## Population trend
create_visualizations_line(oe_comparators, 
                         year_range = c(2005, 2021),
                         main_var = "Population",
                         unit = "Thousands",
                         categories = "mission_categories",
                         output_dir = here::here("Output", output_dir))

create_visualizations_line(oe_comparators2, 
                           year_range = c(2005, 2021),
                           main_var = "Population",
                           unit = "Thousands",
                           categories = "size_categories",
                           output_dir = here::here("Output", output_dir))
## GDP trend
create_visualizations_line(oe_comparators, 
                           year_range = c(2005, 2021),
                           main_var = "GDP",
                           unit = "Millions",
                           categories = "mission_categories",
                           output_dir = here::here("Output", output_dir))

create_visualizations_line(oe_comparators2, 
                           year_range = c(2005, 2021),
                           main_var = "GDP",
                           unit = "Millions",
                           categories = "size_categories",
                           output_dir = here::here("Output", output_dir))
## Employment trend
create_visualizations_line(oe_comparators, 
                           year_range = c(2005, 2021),
                           main_var = "Employment",
                           unit = "Thousands",
                           categories = "mission_categories",
                           output_dir = here::here("Output", output_dir))

create_visualizations_line(oe_comparators2, 
                           year_range = c(2005, 2021),
                           main_var = "Employment",
                           unit = "Thousands",
                           categories = "size_categories",
                           output_dir = here::here("Output", output_dir))
## GDP per worker trend
create_visualizations_line(oe_comparators, 
                           year_range = c(2005, 2021),
                           main_var = "GDP_per_worker",
                           unit = "Thousands",
                           categories = "mission_categories",
                           output_dir = here::here("Output", output_dir))

create_visualizations_line(oe_comparators2, 
                           year_range = c(2005, 2021),
                           main_var = "GDP_per_worker",
                           unit = "Thousands",
                           categories = "size_categories",
                           output_dir = here::here("Output", output_dir))

# General comparison bar charts ----- Merci, ChatGPT

## GVA structure 2019, NOMINAL!!
oe_india_gva <- oe_india %>%
  mutate(GVATOTPPPN = as.numeric(GVATOTPPPN),
         GVAAPPPN  = as.numeric(GVAAPPPN),
         GVAGIR_UPPPN = as.numeric(GVAGIR_UPPPN),
         GVAK_NPPPN = as.numeric(GVAK_NPPPN),
         GVAB_FPPPN = as.numeric(GVAB_FPPPN),
         GVAO_QPPPN = as.numeric(GVAO_QPPPN),
         GVAHJPPPN = as.numeric(GVAHJPPPN),
  ) %>%
  mutate(Agriculture_GVA_Pct = GVAAPPPN / GVATOTPPPN
         , Consumer_services_GVA_Pct = GVAGIR_UPPPN / GVATOTPPPN
         , Financial_business_services_GVA_Pct = GVAK_NPPPN / GVATOTPPPN
         , Industry_GVA_Pct = GVAB_FPPPN / GVATOTPPPN          
         , Public_services_GVA_Pct =  GVAO_QPPPN / GVATOTPPPN 
         , Transport_Information_Communic_Services_GVA_Pct =  GVAHJPPPN / GVATOTPPPN
  ) # Decimal format


oe_comparators_gva <-
  add_group_category(oe_india_gva, 
                     categories = mission_categories,
                     var_col = "Location",
                     new_col = "mission_categories",
                     warn_unmapped = TRUE) %>% 
  dplyr::filter(mission_categories != " ")

oe_comparators2_gva <-
  add_group_category(oe_india_gva, 
                     categories = size_categories,
                     var_col = "Location",
                     new_col = "size_categories",
                     warn_unmapped = TRUE) %>% 
  dplyr::filter(size_categories != " ")


create_visualizations_bar(oe_comparators_gva 
                          , year_range = c(2019, 2019)
                          , main_var = "GVA"
                          , categories = "mission_categories"
                          , output_dir = here::here("Output", output_dir)
)

create_visualizations_bar(oe_comparators2_gva 
                          , year_range = c(2019, 2019)
                          , main_var = "GVA"
                          , categories = "size_categories"
                          , output_dir = here::here("Output", output_dir)
)

## Employment structure 2019
oe_india_emp <- oe_india %>%
  mutate(Total_Emp = as.numeric(EMPTOTT),
         Public_Services_Emp = as.numeric(EMPO_Q),
         Industry_Emp = as.numeric(EMPB_F),
         Financial_Business_Services_Emp = as.numeric(EMPK_N),
         Consumer_Services_Emp = as.numeric(EMPGIR_U),
         Agriculture_Emp = as.numeric(EMPA),
         Transport_Information_Communic_Services_Emp = as.numeric(EMPHJ)) %>%
  mutate(Public_Services_EMP_Pct = Public_Services_Emp / Total_Emp,
         Industry_EMP_Pct = Industry_Emp / Total_Emp ,
         Financial_Business_Services_EMP_Pct = Financial_Business_Services_Emp / Total_Emp,
         Consumer_Services_EMP_Pct = Consumer_Services_Emp / Total_Emp,
         Agriculture_EMP_Pct = Agriculture_Emp / Total_Emp,
         Transport_Information_Communic_Services_EMP_Pct = Transport_Information_Communic_Services_Emp / Total_Emp)


oe_comparators_emp <-
  add_group_category(oe_india_emp, 
                     categories = mission_categories,
                     var_col = "Location",
                     new_col = "mission_categories",
                     warn_unmapped = TRUE) %>% 
  dplyr::filter(mission_categories != " ")

oe_comparators2_emp <-
  add_group_category(oe_india_emp, 
                     categories = size_categories,
                     var_col = "Location",
                     new_col = "size_categories",
                     warn_unmapped = TRUE) %>% 
  dplyr::filter(size_categories != " ")

create_visualizations_bar(oe_comparators_emp 
                          , year_range = c(2019, 2019)
                          , main_var = "EMP"
                          , categories = "mission_categories"
                          , output_dir = here::here("Output", output_dir)
)

create_visualizations_bar(oe_comparators2_emp 
                          , year_range = c(2019, 2019)
                          , main_var = "EMP"
                          , categories = "size_categories"
                          , output_dir = here::here("Output", output_dir)
)

## GVA per worker structure 2019
oe_india_gvapw <- oe_india %>%
  mutate(GVATOTPPPN = as.numeric(GVATOTPPPN),
         GVAAPPPN  = as.numeric(GVAAPPPN),
         GVAGIR_UPPPN = as.numeric(GVAGIR_UPPPN),
         GVAK_NPPPN = as.numeric(GVAK_NPPPN),
         GVAB_FPPPN = as.numeric(GVAB_FPPPN),
         GVAO_QPPPN = as.numeric(GVAO_QPPPN),
         GVAHJPPPN = as.numeric(GVAHJPPPN),  #Mominal value
  ) %>%
  mutate(Total_Emp = as.numeric(EMPTOTT),
         Public_Services_Emp = as.numeric(EMPO_Q),
         Industry_Emp = as.numeric(EMPB_F),
         Financial_Business_Services_Emp = as.numeric(EMPK_N),
         Consumer_Services_Emp = as.numeric(EMPGIR_U),
         Agriculture_Emp = as.numeric(EMPA),
         Transport_Information_Communic_Services_Emp = as.numeric(EMPHJ)) %>%
  mutate(Agriculture_GVApw_Pct = GVAAPPPN / Agriculture_Emp
         , Consumer_services_GVApw_Pct = GVAGIR_UPPPN / Consumer_Services_Emp
         , Financial_business_services_GVApw_Pct = GVAK_NPPPN / Financial_Business_Services_Emp
         , Industry_GVApw_Pct = GVAB_FPPPN / Industry_Emp          
         , Public_services_GVApw_Pct =  GVAO_QPPPN / Public_Services_Emp 
         , Transport_Information_Communic_Services_GVApw_Pct =  GVAHJPPPN / Transport_Information_Communic_Services_Emp
  ) # Decimal format


oe_comparators_gvapw <-
  add_group_category(oe_india_gvapw, 
                     categories = mission_categories,
                     var_col = "Location",
                     new_col = "mission_categories",
                     warn_unmapped = TRUE) %>% 
  dplyr::filter(mission_categories != " ")

oe_comparators2_gvapw <-
  add_group_category(oe_india_gvapw, 
                     categories = size_categories,
                     var_col = "Location",
                     new_col = "size_categories",
                     warn_unmapped = TRUE) %>% 
  dplyr::filter(size_categories != " ")

create_visualizations_bar_dodge(oe_comparators_gvapw 
                          , year_range = c(2019, 2019)
                          , main_var = "GVApw"
                          , categories = "mission_categories"
                          , output_dir = here::here("Output", output_dir)
)

create_visualizations_bar_dodge(oe_comparators2_gvapw 
                          , year_range = c(2019, 2019)
                          , main_var = "GVApw"
                          , categories = "size_categories"
                          , output_dir = here::here("Output", output_dir)
)

# General comparison individual cities, I See 16 cities NOT 17 -----

# Create a plot for each location

## Area chart of change of GVA structure over time. (2005-2021)

unique(oe_india_gva$Location)

create_area_visualizations(oe_india_gva 
                          , main_var = "GVA"
                          , year_range = c(2005, 2021)
                          , output_dir = here::here("Output", output_dir)
)


## Area chart of change of employment structure over time (2005-2021)
unique(oe_india_emp$Location)

create_area_visualizations(oe_india_emp 
                          , main_var = "EMP"
                          , year_range = c(2005, 2021)
                          , output_dir = here::here("Output", output_dir)
)

# NUMBEO data ----

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



temp <- data_rankings_pollution_past %>% 
  filter(country %in% countries & city_name %in% cities) 
# 4 cities
# 10 cities

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


# Find if the city is present using Fuzzy matching 
temp <-  data_rankings_pollution_past %>% 
  filter(
    # # For each country
    # sapply(str_to_lower(str_trim(country)), function(x) {
    #   any(stringdist(x, str_to_lower(str_trim(countries)), method = "jw") / 
    #         pmax(nchar(x), nchar(str_to_lower(str_trim(countries)))) < 0.2)
    # }) &
      # For each city
      sapply(str_to_lower(str_trim(city_name)), function(x) {
        any(stringdist(x, str_to_lower(str_trim(all_cities)), method = "jw") / 
              pmax(nchar(x), nchar(str_to_lower(str_trim(all_cities)))) < 0.1)
      })
  ) %>%
  distinct(city_name, Year) %>%
  count(city_name, sort = TRUE) 


filtered_data <- pollution_ucdb %>% 
  mutate(Selected_cities = ifelse(Country %in% countries & Location %in% cities, "Selected cities", "Other"))


# From Congestion_indic.R obtain pollution_ucdb
if (!exists("pollution_ucdb")) {
  message("Data pollution_ucdb not found. Skipping code.")
} else {
  
  index <- c("PM2.5", "PM2.5_concentration")
  
  filtered_data <- pollution_ucdb %>% 
    mutate(Selected_cities = ifelse(Country %in% countries & Location %in% cities, "Selected cities", "Other"))
  
  plot_index <- function(index) {
    plot <- filtered_data %>% 
      filter(.data[[index]] > 0) %>%  
      ggplot(aes(x = log_gdp, y = .data[[index]], color = Selected_cities)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +  # Optional: Add a linear regression line
      geom_text_repel(
        data = . %>% filter(Selected_cities == "Selected cities"),
        aes(label = Location),
        size = 5,
        box.padding = 0.5,
        point.padding = 0.1,
        force = 3,
        segment.color = "grey50",
        fontface = "bold",
        nudge_x = 0.5
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "Log GDP", y = gsub("_", "", index)
           # , title = paste(index, "vs Log Density (2020)")
      ) +
      scale_color_manual(values = c("Selected cities" = "red", "Other" = "blue"))
    
    print(plot)
    
    ggsave(filename = here("Output", output_dir, paste0(index, "_vs_Log_GDP_2020.png"))
           , plot = plot, width = 12, height = 10, dpi = 800)
  }
  
  walk(index, plot_index)
  
}

# Merge datasets
pollution_data <- pollution_ucdb %>%
  left_join(data_rankings_pollution_past %>% filter(Year == 2019), by = c("Country" = "country", "Location" = "city_name")) %>% 
  filter(!is.na(pollution_index) |!is.na(exp_pollution_index)) %>% 
  mutate(Selected_cities = ifelse(Country %in% countries & Location %in% cities, "Selected cities", "Other"))

index <- c("pollution_index", "exp_pollution_index")
filtered_data <-  pollution_data 
walk(index, plot_index)

# Select fastest growing cities -----

calculate_top_growth <- function(data_cities_list, top = 4, year_ini = 2001, year_end = 2021) {
  bind_rows(purrr::imap(data_cities_list, function(data_cities, name) {
    location_list <- data_cities %>%
      as.data.frame() %>%
      pull(Location) %>%
      unname()
    
    attributes(location_list) <- NULL  # Clears Stata metadata
    
    data %>%
      dplyr::filter(Location %in% location_list) %>%
      group_by(Location) %>%
      arrange(Location, Year) %>%
      mutate(
        GDPTOTUSC = as.numeric(GDPTOTUSC),
        Year = as.numeric(Year),
        year_diff_ini = if_else(!is.na(GDPTOTUSC), abs(Year - year_ini), Inf),
        year_diff_end = if_else(!is.na(GDPTOTUSC), abs(Year - year_end), Inf),
        is_closest_ini = year_diff_ini == min(year_diff_ini),
        is_closest_end = year_diff_end == min(year_diff_end),
        ref_GDP_ini = if_else(is_closest_ini, GDPTOTUSC, NA_real_),
        ref_GDP_end = if_else(is_closest_end, GDPTOTUSC, NA_real_)
      ) %>%
      mutate(
        ref_GDP_ini = max(ref_GDP_ini, na.rm = TRUE),
        ref_GDP_end = max(ref_GDP_end, na.rm = TRUE),
        GDP_growth = (ref_GDP_end - ref_GDP_ini) / ref_GDP_ini,
        group = name
      ) %>%
      ungroup() %>%
      dplyr::select(Location, GDPTOTUSC, ref_GDP_ini, ref_GDP_end, GDP_growth, group) %>%
      distinct(Location, .keep_all = TRUE) %>%
      arrange(desc(GDP_growth), Location) %>%
      head(n = top)
  }))
}

data_cities_list <- list("Jaipur" = data_jaipur, "Jodhpur" = data_jodhpur, "Kota" = data_kota)
growth_results <- calculate_top_growth(data_cities_list, top = 4, year_ini = 2001, year_end = 2021)

## Generate subset of cities
rajasthan_comparators = list(
  Jaipur_comparators  = c("Jaipur","Addis Ababa", "Kumasi", "Faisalabad"),
  Jodhpur_comparators = c("Jodhpur","Phnom Penh","Cagayan de Oro", "Bandjarmasin", "Rajshahi"),
  Kota_comparators    = c("Kota","Kigali", "Mwanza", "Cagayan de Oro", "Kitwe")
  )


jaipur_comparators = list(
  Mining_comparators  = c("Jaipur", "Port Harcourt","Santa Cruz", "Quetta", "Urumqi, Xinjiang", "Arequipa")
)

oe_comparators <-
  add_group_category(data, 
                     categories = rajasthan_comparators,
                     var_col = "Location",
                     new_col = "rajasthan_comparators",
                     warn_unmapped = TRUE) %>% 
  dplyr::filter(rajasthan_comparators != " ")

oe_comparators2 <-
  add_group_category(data, 
                     categories = jaipur_comparators,
                     var_col = "Location",
                     new_col = "jaipur_comparators",
                     warn_unmapped = TRUE) %>% 
  dplyr::filter(jaipur_comparators != " ")


# Produce charts, output folder "India_03"
output_dir <- "India_03"

## Line charts
## Population trend
create_visualizations_line(oe_comparators, 
                           year_range = c(2005, 2021),
                           main_var = "Population",
                           unit = "Thousands",
                           categories = "rajasthan_comparators",
                           output_dir = here::here("Output", output_dir))

create_visualizations_line(oe_comparators2, 
                           year_range = c(2005, 2021),
                           main_var = "Population",
                           unit = "Thousands",
                           categories = "jaipur_comparators",
                           output_dir = here::here("Output", output_dir))
## GDP trend
create_visualizations_line(oe_comparators, 
                           year_range = c(2005, 2021),
                           main_var = "GDP",
                           unit = "Millions",
                           categories = "rajasthan_comparators",
                           output_dir = here::here("Output", output_dir))

create_visualizations_line(oe_comparators2, 
                           year_range = c(2005, 2021),
                           main_var = "GDP",
                           unit = "Millions",
                           categories = "jaipur_comparators",
                           output_dir = here::here("Output", output_dir))
## Employment trend
create_visualizations_line(oe_comparators, 
                           year_range = c(2005, 2021),
                           main_var = "Employment",
                           unit = "Thousands",
                           categories = "rajasthan_comparators",
                           output_dir = here::here("Output", output_dir))

create_visualizations_line(oe_comparators2, 
                           year_range = c(2005, 2021),
                           main_var = "Employment",
                           unit = "Thousands",
                           categories = "jaipur_comparators",
                           output_dir = here::here("Output", output_dir))
## GDP per worker trend
create_visualizations_line(oe_comparators, 
                           year_range = c(2005, 2021),
                           main_var = "GDP_per_worker",
                           unit = "Thousands",
                           categories = "rajasthan_comparators",
                           output_dir = here::here("Output", output_dir))

create_visualizations_line(oe_comparators2, 
                           year_range = c(2005, 2021),
                           main_var = "GDP_per_worker",
                           unit = "Thousands",
                           categories = "jaipur_comparators",
                           output_dir = here::here("Output", output_dir))

# Bar charts
create_visualizations_bar(oe_comparators 
                          , year_range = c(2019, 2019)
                          , main_var = "GVA"
                          , categories = "rajasthan_comparators"
                          , output_dir = here::here("Output", output_dir))

create_visualizations_bar(oe_comparators2 
                          , year_range = c(2019, 2019)
                          , main_var = "GVA"
                          , categories = "jaipur_comparators"
                          , output_dir = here::here("Output", output_dir))

create_visualizations_bar(oe_comparators 
                          , year_range = c(2019, 2019)
                          , main_var = "EMP"
                          , categories = "rajasthan_comparators"
                          , output_dir = here::here("Output", output_dir))

create_visualizations_bar(oe_comparators2 
                          , year_range = c(2019, 2019)
                          , main_var = "EMP"
                          , categories = "jaipur_comparators"
                          , output_dir = here::here("Output", output_dir))


## GVA per worker structure 2019
create_visualizations_bar_dodge(oe_comparators 
                                , year_range = c(2019, 2019)
                                , main_var = "GVApw"
                                , categories = "rajasthan_comparators"
                                , output_dir = here::here("Output", output_dir))

create_visualizations_bar_dodge(oe_comparators2 
                                , year_range = c(2019, 2019)
                                , main_var = "GVApw"
                                , categories = "jaipur_comparators"
                                , output_dir = here::here("Output", output_dir))

create_visualizations_bar_dodge(oe_comparators2 
                                , year_range = c(2019, 2019)
                                , main_var = "GVApw"
                                , categories = "jaipur_comparators"
                                , output_dir = here::here("Output", output_dir))

## Individual cities
all_cities <- unique(unlist(c(rajasthan_comparators, jaipur_comparators)))

data_mining_cities <- data %>%
  dplyr::filter(Location %in% all_cities)

unique(data_mining_cities$Location)

## Area chart of change of GVA structure over time. (2005-2021)
create_area_visualizations(data_mining_cities  
                           , main_var = "GVA"
                           , year_range = c(2005, 2021)
                           , output_dir = here::here("Output", output_dir))

## Area chart of change of employment structure over time (2005-2021)
create_area_visualizations(data_mining_cities  
                           , main_var = "EMP"
                           , year_range = c(2005, 2021)
                           , output_dir = here::here("Output", output_dir))

##  Pollution data, need pollution_ucdb! -----
filtered_data <- pollution_ucdb %>% 
  mutate(Selected_cities = ifelse(Location %in% all_cities, "Selected cities", "Other")) %>% 
  mutate(India = ifelse(Country == "India", "India", "Other"))

table(filtered_data$Selected_cities)
# Other Selected cities 
# 10833              17 

table(filtered_data$India)
# India Other 
# 1805  9047 

index <- c("PM2.5", "PM2.5_concentration")

plot_index <- function(index) {
  plot <- filtered_data %>%
    filter(.data[[index]] > 0) %>%
    ggplot() +
    
    # All other cities (Grey)
    geom_point(data = . %>% filter(!(India == "India" | Selected_cities == "Selected cities")),
               aes(x = log_gdp, y = .data[[index]]), color = "lightgrey", size = 3) +
    
    # Indian cities (Red, but not selected cities)
    geom_point(data = . %>% filter(India == "India" & Selected_cities != "Selected cities"),
               aes(x = log_gdp, y = .data[[index]]), color = "red", size = 4, alpha = 0.3) +
    
    # Selected cities (Always orange, priority over other classifications)
    geom_point(data = . %>% filter(Selected_cities == "Selected cities"),
               aes(x = log_gdp, y = .data[[index]]), fill = "#FF8C00" , color = "black", size = 5,  alpha = 0.8, shape = 22) +
    
    # LM trend line for all cities (Blue)
    geom_smooth(
      aes(x = log_gdp, y = .data[[index]]),
      method = "lm", se = FALSE, color = "blue"
    ) +
    
    # LM trend line for Indian cities only (Red)
    geom_smooth(
      data = . %>% filter(India == "India"),
      aes(x = log_gdp, y = .data[[index]]),
      method = "lm", se = FALSE, color = "red"
    ) +
    
    # Labels only for Selected cities (Orange)
    geom_text_repel(
      data = . %>% filter(Selected_cities == "Selected cities"),
      aes(x = log_gdp, y = .data[[index]], label = Location),
      size = 5,
      box.padding = 1,
      point.padding = 0.3,
      force = 5,
      segment.color = "grey50",
      fontface = "bold",
      color = "#FF8C00",
      nudge_x = 2
    ) +
    
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Log GDP", y = gsub("_", "", index)) 
  
  print(plot)
  
  ggsave(
    filename = here("Output", output_dir, paste0(index, "_vs_Log_GDP_2020.png")),
    plot = plot, width = 12, height = 10, dpi = 400
  )
}

walk(index, plot_index)

# Merge datasets, require pollution_ucdb AND data_rankings_pollution_past!!! ee Congestion_indc.R lines 39733 first!!!
pollution_data <- pollution_ucdb %>%
  left_join(data_rankings_pollution_past %>% filter(Year == 2019), by = c("Country" = "country", "Location" = "city_name")) %>% 
  filter(!is.na(pollution_index) |!is.na(exp_pollution_index)) %>% 
  mutate(Selected_cities = ifelse(Location %in% all_cities, "Selected cities", "Other"))%>% 
  mutate(India = ifelse(Country == "India", "India", "Other"))

table(pollution_data$Selected_cities)
# Other Selected cities 
# 204               3 

table(pollution_data$India)
# India Other 
# 36   171 

index <- c("pollution_index", "exp_pollution_index")
filtered_data <-  pollution_data 
walk(index, plot_index)

# Enviromenmtal exposure data
index <- c("CO2_per_capita", "GHG_per_capita", "NOx_per_capita", "PM2.5_mortality")
filtered_data <-  pollution_data 
walk(index, plot_index)

# Boxplot by region
plot_box <- function(indicator) {
  plot_data <- pollution_data %>%
    group_by(Regions) %>%
    mutate(median_value = median(.data[[indicator]], na.rm = TRUE)) %>%
    ungroup()
  
  india_data <- pollution_data %>%
    filter(India == "India") %>%
    mutate(Regions2 = "India",
           median_value = median(.data[[indicator]], na.rm = TRUE),
           n = n()) %>%
    ungroup()
  
  plot_data <- bind_rows(plot_data, india_data) %>%
    mutate(Regions2 = ifelse(India == "India", "India", as.character(Regions))) %>%
    mutate(Regions2 = factor(Regions2, levels = unique(c(Regions2, "India")))) %>% 
    group_by(Regions2) %>%
    mutate(Regions2 = reorder(Regions2, median_value)) %>% 
    mutate(n = n()) %>%
    ungroup()
  
  plot <- ggplot(plot_data, aes(x = .data[[indicator]], y = Regions2, fill = Regions2 == "India")) +
    geom_boxplot(outlier.size = 2, outlier.alpha = 0.6) +
    labs(x = indicator, 
         y = NULL,
         title = paste("Boxplot of", indicator, "by Region")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12, face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none") +
    scale_x_continuous(limits = range(plot_data[[indicator]], na.rm = TRUE)) +
    scale_fill_manual(values = c("TRUE" = "red")) +
    annotate("text", x = Inf, y = plot_data$Regions2,
             label = paste0(" (n = ", plot_data$n, ")"),
             hjust = 1, vjust = 0.5, size = 4)
  
  ggsave(filename = here("Output", output_dir, paste0(indicator, "_vs_Log_GDP_2020.png")),
         plot = plot, width = 12, height = 10, dpi = 400)
}

# List of PM2.5 indices
index <- c("PM2.5_total", "PM2.5_agriculture", "PM2.5_energy", "PM2.5_industry",
           "PM2.5_residential", "PM2.5_transportation", "PM2.5_waste")

# Generate plots for each index
walk(index, ~plot_box(.x))

# Share by region
pm25_data <- pollution_data %>%
  mutate(
    Regions2 = ifelse(India == "India", "India", as.character(Regions)),
    Regions2 = factor(Regions2, levels = unique(c(Regions2, "India")))
    ) %>%
  group_by(Regions2) %>%
  summarise(
    Total_PM2.5_Agriculture = sum(PM2.5_agriculture, na.rm = TRUE),
    Total_PM2.5_Energy = sum(PM2.5_energy, na.rm = TRUE),
    Total_PM2.5_Industry = sum(PM2.5_industry, na.rm = TRUE),
    Total_PM2.5_Residential = sum(PM2.5_residential, na.rm = TRUE),
    Total_PM2.5_Transportation = sum(PM2.5_transportation, na.rm = TRUE),
    Total_PM2.5_Waste = sum(PM2.5_waste, na.rm = TRUE),
    Total_PM2.5 = sum(PM2.5_total, na.rm = TRUE)
  ) %>%
  mutate(
    Agriculture_PM2.5_Pct = Total_PM2.5_Agriculture / Total_PM2.5,
    Energy_PM2.5_Pct = Total_PM2.5_Energy / Total_PM2.5,
    Industry_PM2.5_Pct = Total_PM2.5_Industry / Total_PM2.5,
    Residential_PM2.5_Pct = Total_PM2.5_Residential / Total_PM2.5,
    Transportation_PM2.5_Pct = Total_PM2.5_Transportation / Total_PM2.5,
    Waste_PM2.5_Pct = Total_PM2.5_Waste / Total_PM2.5,
    Year = as.character(2020),
    Location = Regions2
  )

create_visualizations_bar(pm25_data 
                          , year_range = c(2020, 2020)
                          , main_var = "PM2.5"
                          , categories = "Regions2"
                          , output_dir = here::here("Output", output_dir)
)

# Lack of infrastructure, CIS Index
summary(pollution_data$CISI_total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.112   0.176   0.220   0.264   0.880 

index <- c("CISI_total", "CISI_energy", "CISI_transport", "CISI_water",
  "CISI_waste", "CISI_telecom", "CISI_health", "CISI_education")

# Generate plots for each index
walk(index, plot_index)

# Generate box plots for each index
walk(index, ~plot_box(.x))
.