# Request for Indian cities data 10/04/2025

# Load packages and data, see Main file
require(here)

source(here("Master_variables.R"))

if(!exists("data")) stop("Data not found")

source("Plot_functions_V1.R")
source("plot_function.R")


# Create output folder
dir.create("Output/India_02", showWarnings = FALSE)

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


# Data wrangling---- 
oe_india <- data %>% 
  mutate(Population = as.numeric(POPTOTT)) %>% 
  mutate(GDP = as.numeric(GDPTOTUSN)) %>% # Using NOMINAL GDP
  mutate(Employment = as.numeric(EMPTOTT)) %>% 
  mutate(GDP_per_capita = GDP / Population) %>%
  mutate(GDP_per_worker = GDP / Employment) %>%
  filter(Location != Country) %>% 
  dplyr::filter(Year %in% c(2005:2021)) 
  
countries <- c("India", "Vietnam", "Malaysia", "China", "Oman", "Japan", "Indonesia", "South Korea")
cities <- c("Jaipur", "Kota", "Jodhpur", "Lucknow", "Varanasi", "Kanpur",
            "Bhubaneswar", "Pune", "Surabaya", "Ho Chi Minh City", "Kuala Lumpur",
            "Tianjin", "Muscat", "Okayama MMA", "Pekan Baru", "Ulsan")

oe_india <- oe_india %>% 
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
  Larger_cities = c("Jaipur", "Bhubaneswar", "Varanasi", "Pune", 
                    "Lucknow", "Kanpur", "Surabaya", "Ho Chi Minh City", 
                    "Kuala Lumpur", "Tianjin"),
  
  Smaller_cities = c("Kota", "Jodhpur", "Muscat", "Okayama MMA", 
                     "Pekan Baru", "Ulsan")
)

oe_comparators <- oe_india 

oe_comparators <-
  add_group_category(oe_comparators, 
                     categories = mission_categories,
                     var_col = "Location",
                     new_col = "mission_categories",
                     warn_unmapped = TRUE) 
oe_comparators <-
  add_group_category(oe_comparators, 
                     categories = size_categories,
                     var_col = "Location",
                     new_col = "size_categories",
                     warn_unmapped = TRUE)

# General comparison line charts -----
require(rlang)

create_visualizations_line <- function(data, year_range = c(2005, 2021), 
                                  main_var = "Population", 
                                  unit = "Thousands",
                                  output_dir = here::here("Output", "India_02")) {
  
  # Data preparation
  data_filtered <- data %>%
    filter(between(Year, year_range[1], year_range[2])) %>%
    mutate(Year = as.numeric(Year)) %>%
    dplyr::select(Location, Year, mission_categories, all_of(main_var)) %>%
    rename(Value = !!sym(main_var)) %>%
    mutate(Value = as.numeric(Value))  # Ensure numeric conversion
  
  # Function to create plots for each category
  create_category_plot <- function(cat_data) {
    
    category <- unique(cat_data$mission_categories)
    
    start_year <- min(cat_data$Year)
    end_year <- max(cat_data$Year)
    
    label_years <- seq(start_year, end_year, by = 2)
    
    cat_data_labeled <- cat_data %>%
      mutate(show_label = Year %in% label_years,
             ValueLabel = scales::comma(Value, accuracy = 1))
    
    # Generate plot
    title_text <- paste(main_var, "Trend in", category, paste0("(", start_year, "-", end_year, ")"))
    y_axis_label <- paste0(main_var, " (", unit, ")")
    
    
    p <- ggplot(cat_data, aes(x = Year, y = Value, color = Location, group = Location)) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point(size = 3) +
      # geom_text(data = cat_data_labeled %>% filter(show_label),
      #           aes(label = ValueLabel), 
      #           vjust = -0.8, size = 3.5) +
      geom_text_repel(data = cat_data_labeled %>% filter(show_label),
                      aes(label = ValueLabel), 
                      size = 3.5, 
                      box.padding = 0.5, 
                      point.padding = 0.5) +
      scale_color_manual(values = wes_palette("Zissou1", n = length(unique(cat_data$Location)), 
                                              type = "continuous")) +
      labs(
        title = gsub("_", " ", title_text),
        x = "Year",
        y = gsub("_", " ", y_axis_label),
        color = "City") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      scale_y_continuous(labels = comma_format()) +
      scale_x_continuous(breaks = seq(year_range[1], year_range[2], by = 2))
    
    filename <- paste0(main_var, "_Trends_", gsub("_", " ", gsub(" ", "_", category)), 
                       "_", start_year, "-", end_year, ".png")
    
    ggsave(filename = file.path(output_dir, filename),
           plot = p,
           width = 12,
           height = 8,
           dpi = 600)
    
    return(p)
  }
  
  # Generate plots for each category
  plots <- data_filtered %>%
    group_by(mission_categories) %>%
    group_map(~ create_category_plot(.x), .keep = TRUE)
  
  return(plots)
}

## Population trend
create_visualizations_line(oe_comparators, 
                         year_range = c(2005, 2021),
                         main_var = "Population",
                         unit = "Thousands",
                         output_dir = here::here("Output", "India_02"))

## GDP trend
create_visualizations_line(oe_comparators, 
                           year_range = c(2005, 2021),
                           main_var = "GDP",
                           unit = "Millions",
                           output_dir = here::here("Output", "India_02"))

## Employment trend
create_visualizations_line(oe_comparators, 
                           year_range = c(2005, 2021),
                           main_var = "Employment",
                           unit = "Thousands",
                           output_dir = here::here("Output", "India_02"))

## GDP per worker trend
create_visualizations_line(oe_comparators, 
                           year_range = c(2005, 2021),
                           main_var = "GDP_per_worker",
                           unit = "Thousands",
                           output_dir = here::here("Output", "India_02"))



# General comparison bar charts -----

create_visualizations_bar <- function(data, year_range = c(2001, 2019), 
                                      output_dir = here::here("Output", "India_02")) {
  
  # Data preparation
  pie_data <- data %>%
    pivot_longer(
      cols = ends_with("_Pct"), 
      names_to = "Sector", 
      values_to = "Percentage"
    ) %>%
    filter(between(Year, year_range[1], year_range[2])) %>%
    mutate(
      Sector = str_remove(Sector, "_EMP_Pct"|"_GDP_Pct"|"_GVApw_Pct"),
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
        pos = (ymax + ymin) / 2,  # Center position for labels
        perc = scales::percent(Percentage, accuracy = 0.1)
      ) %>%
      ungroup()
    
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
          title = paste("Employment by Sector in", location, 
                        paste0("(", start_year, ")")),
          x = NULL,
          y = "Percentage",
          fill = "Sector"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank()
        ) +
        scale_y_continuous(labels = scales::percent_format()) +
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
          title = paste("Employment by Sector in", location, 
                        paste0("(", start_year, "-", end_year, ")")),
          x = NULL,
          y = "Percentage",
          fill = "Sector"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12, 
                                     face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank()
        ) +
        scale_y_continuous(labels = scales::percent_format()) +
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
        geom_line(
          data = label_data %>% 
            filter(Year == start_year | Year == end_year),
          aes(x = Year, y = pos, group = Sector),
          linetype = "dotted", 
          color = "gray50"
        )
    }
    
    # Save plot
    filename <- paste0(
      ,"_Sector_", 
      start_year, if (end_year != start_year) paste0("-", end_year), "_",
      gsub(" ", "_", location), 
      ".png"
    )
    
    ggsave(
      filename = file.path(output_dir, filename),
      plot = p,
      width = 12,
      height = 8,
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


create_visualizations_bar(oe_india_gva 
                          # , year_range =  c(2001, 2019)
                          , year_range = c(2019, 2019)
                          , location = "India"
                          , output_dir = here::here("Output", "India_02")
)


## Emplpoyment structure 2019
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

create_visualizations_bar(oe_india_emp 
                          # , year_range =  c(2001, 2019)
                          , year_range = c(2019, 2019)
                          , 
                          , output_dir = here::here("Output", "India_02")
)


## GVA per worker structure 2019
oe_india_gvapw <- oe_india %>%
  mutate(GVATOTPPPN = as.numeric(GVATOTPPPN),
         GVAGIR_UPPPN = as.numeric(GVAGIR_UPPPN),
         GVAK_NPPPN = as.numeric(GVAK_NPPPN),
         GVAB_FPPPN = as.numeric(GVAB_FPPPN),
         GVAO_QPPPN = as.numeric(GVAO_QPPPN),
         GVAHJPPPN = as.numeric(GVAHJPPPN),
  ) %>%
  mutate(Total_Emp = as.numeric(EMPTOTT),
         Public_Services_Emp = as.numeric(EMPO_Q),
         Industry_Emp = as.numeric(EMPB_F),
         Financial_Business_Services_Emp = as.numeric(EMPK_N),
         Consumer_Services_Emp = as.numeric(EMPGIR_U),
         Agriculture_Emp = as.numeric(EMPA),
         Transport_Information_Communic_Services_Emp = as.numeric(EMPHJ)) %>%
  mutate(Agriculture_GVApw_Pct = GVAAPPPC / Agriculture_Emp
         , Consumer_services_GVApw_Pct = GVAGIR_UPPPC / Consumer_Services_Emp
         , Financial_business_services_GVApw_Pct = GVAK_NPPPC / Financial_Business_Services_Emp
         , Industry_GVApw_Pct = GVAB_FPPPC / Industry_Emp          
         , Public_services_GVApw_Pct =  GVAO_QPPPC / Public_Services_Emp 
         , Transport_Information_Communic_Services_GVApw_Pct =  GVAHJPPPC / Transport_Information_Communic_Services_Emp
  ) # Decimal format



# General comparison individual cities, I See 16 cities NOT 17 -----

# Create a plot for each location
create_gva_visualizations <- function(data, year_range = c(2001, 2019), 
                                      main_var = "GVA",
                                      output_dir = here::here("Output", "India_02")) {
  
  
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
          title = paste(main_var, "Contribution by Sector in", location, 
                        paste0("(", start_year, ")")),
          x = NULL,
          y = "Percentage",
          fill = "Sector"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank()
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
          title = paste(main_var, "Contribution by Sector in", location, 
                        paste0("(", start_year, "-", end_year, ")")),
          x = NULL,
          y = "Percentage",
          fill = "Sector"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12, 
                                     face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank()
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

## Area chart of change of GVA structure over time. (2005-2021)

unique(oe_india_gva$Location)

create_gva_visualizations(oe_india_gva 
                          , main_var = "GVA"
                          , year_range = c(2005, 2021)
                          , output_dir = here::here("Output", "India_02")
)


## Area chart of change of employment structure over time (2005-2021)
unique(oe_india_emp$Location)

create_gva_visualizations(oe_india_emp 
                          , main_var = "EMP"
                          , year_range = c(2005, 2021)
                          , output_dir = here::here("Output", "India_02")
)



