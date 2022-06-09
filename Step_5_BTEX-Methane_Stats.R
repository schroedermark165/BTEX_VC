# PROJECT INFORMATION:
#     Name:Denver-Julesburg Basin Water-Well BTEX Study 
#     Description: Basic statistics to analyze whether BTEX and methane are significantly related
# 
# HISTORY:
# 2021-05-05            Created for statistical analyses
# 
#================================================================================
# LOAD PACKAGES

library(tidyverse)
library(lubridate)
library(janitor)
library(openxlsx)
library(arcgisbinding)
arc.check_product()
library(sf)
library(ggsci)
library(scales)
library(gridExtra)

# Stats Question 1 --------------------------------------------------------
# Are BTEX and methane associated with each other in water wells?
# Null hypothesis: BTEX appears in water wells independent of methane
rm(list=ls())

# COGCC sample data
# 1. Bind it all together and plot in GIS
# 2. Select points within Wattenberg
# 3. Query for unique wells sampled for BTEX AND Methane
# 4. Which had positive BTEX?
# 5. Which had positive Methane?

facilities <- c("Domestic Well",
                "Ground Water", "Groundwater",
                "Stock or  Irrigation",
                "Irrigation",
                "Municipal",
                "Commercial")

btex <- c("ETHYLBENZENE",
          "TOLUENE",
          "BENZENE",
          "m-+p-XYLENE",
          "o-XYLENE",
          "TOTAL XYLENES",
          "m-XYLENE")

gas <- c("METHANE",
         "ETHANE",
         "PROPANE",
         "DELTA 13C C1")


d_wattenberg <- arc.open("D:\\Google Drive\\ArcGIS\\DJ_Basin_BTEX_Study\\Data\\1_Geodatabase\\Base_Features.gdb\\Wattenberg_Field_UTM_polygon")
sf_wattenberg <- arc.select(d_wattenberg, where_clause = "description = '2020 revision'")
sf_wattenberg <- arc.data2sf(sf_wattenberg)

water_wells <- read_csv("Input/Step_1_Original_Data/DL_Locations.csv",
                        col_types = cols(FacilityID = col_character(),
                                         PermitNumber = col_character())) %>% clean_names()
samples <- read_csv("Input/Step_1_Original_Data/DL_Samples.csv",
                    col_types = cols(FacilityID = col_character(),
                                     SampleID = col_character())) %>% clean_names()
results <- data.table::fread("Input/Step_1_Original_Data/DL_Results.csv") %>% clean_names()
results$result_id <- as.character(results$result_id)
results$sample_id <- as.character(results$sample_id)

sf_samples <- samples %>%
      inner_join(water_wells, by = c("facility_id")) %>%
      mutate(x = utm_x83,
             y = utm_y83,
             wattenberg = as.character(""))

sf_samples2 <- st_as_sf(sf_samples,
                        coords = c("x", "y"),
                        crs = 26913)

sf_samples_wb <- st_join(sf_samples2, sf_wattenberg, join = st_within) %>%
      filter(description == "2020 revision") %>%
      select(-c(OBJECTID, description)) %>%
      mutate(wattenberg = TRUE)

# arc.write(file.path("D:\\Google Drive\\ArcGIS\\DJ_Basin_BTEX_Study\\Data\\1_Geodatabase\\Water_Wells.gdb\\COGCC_ALL_SAMPLES"),
#           data = sf_samples2, overwrite = T)

sf_results <- sf_samples_wb %>%
      left_join(results, by = c("sample_id")) %>%
      # convert to datetime
      mutate(sample_date = lubridate::mdy_hms(sample_date)) 

# How many unique Wattenberg samples total?
# 8852 unique samples
unique_samples_wb <- as.data.frame(sf_results) %>% 
      distinct(facility_id, facility_type, sample_id, sample_date, permit_number, receipt_number)

# How many unique water wells were sampled in Wattenberg?
# For specific facility types in list above (same as BTEX)
# 1837 unique water wells
unique_facids_sampled <- unique_samples_wb %>% 
      distinct(facility_id, facility_type, permit_number, receipt_number) %>% 
      filter(facility_type %in% facilities)

# How many Wattenberg sampled for btex and/or methane
# Not necessarily positive
# 1664 sampled for BTEX
unique_facids_btex <- as.data.frame(sf_results) %>% 
      filter(facility_type %in% facilities,
             param_description %in% btex) %>%
      distinct(facility_id, facility_type, permit_number, receipt_number, utm_x83, utm_y83) %>%
      mutate(btex_sampled = "yes")

# 1780 sampled for methane
unique_facids_ch4 <- as.data.frame(sf_results) %>% 
      filter(facility_type %in% facilities,
             param_description == "METHANE") %>%
      distinct(facility_id, facility_type, permit_number, receipt_number, utm_x83, utm_y83) %>%
      mutate(ch4_sampled = "yes")

# How many unique water wells sampled?
# 1794 total were sampled
# 130 were not sampled for BTEX, but were for methane
# 14 were not sampled for methane, but were for BTEX
unique_facids_both <- full_join(unique_facids_ch4, unique_facids_btex)


# Read GIS features (BTEX and methane in water)
gdb_btex <- arc.open("D:/Google Drive/ArcGIS/DJ_Basin_BTEX_Study/Data/1_Geodatabase/Water_Wells.gdb/Water_Wells_Sampled_BTEX_methane_20210302")
d_btex <- arc.select(gdb_btex)
d_btex2 <- d_btex %>% 
      select(FacilityID, PermitNumber) %>% 
      rename(facility_id = FacilityID,
             permit_number = PermitNumber) %>%
      mutate(facility_id = as.character(facility_id),
             btex_detected = TRUE)

gdb_ch4 <- arc.open("D:/Google Drive/ArcGIS/DJ_Basin_BTEX_Study/Data/1_Geodatabase/Water_Wells.gdb/Water_Wells_Sampled_Gas_Data_20210213")
d_ch4 <- arc.select(gdb_ch4, where_clause = "Wattenberg = 'Wattenberg'")
d_ch42 <- d_ch4 %>% 
      select(FacilityID, PermitNumber) %>% 
      rename(facility_id = FacilityID,
             permit_number = PermitNumber) %>%
      mutate(facility_id = as.character(facility_id),
             ch4_detected = TRUE)

# Read csv of water wells gas data for ch4 origin
# Unique wells, thermogenic > mixed > microbial origins kept in that order
# Because some water wells had CH4 with thermogenic and microbial
d_ch4_origins <- read_csv("Output/COGCC_CH4_Results_Origin_2021-06-06.csv",
                          col_types = cols(facility_id = col_character(),
                                           permit_number = col_character())) %>% 
   clean_names() %>%
   select(c(facility_id, methane, ethane, propane, fraction, delta_13c_c1, origin))


# EDIT NEEDED: FILTER FOR ALL METHANE AND BTEX SAMPLES
# GROUPBY FACID AND APPLY YES/NO TO EACH FOR CH4/BTEX
d_btexch4_results <- as.data.frame(sf_results) %>%
      filter(facility_type %in% facilities,
             permit_number != "NA") %>%
      mutate(btex_sampled = if_else(param_description %in% btex, TRUE, FALSE),
             ch4_sampled = if_else(param_description == "METHANE", TRUE, FALSE))

d_btexch4_sampled <- d_btexch4_results %>%
      select(facility_id, facility_type, utm_x83, utm_y83, permit_number,
             receipt_number, well_depth, wattenberg, btex_sampled, ch4_sampled) %>%
      left_join(d_btex2, by = c("facility_id", "permit_number")) %>% 
      left_join(d_ch42, by = c("facility_id", "permit_number")) %>%
      mutate(btex_detected = if_else(is.na(btex_detected), FALSE, btex_detected),
             ch4_detected = if_else(is.na(ch4_detected), FALSE, ch4_detected))

d_btexch4_sampled <- d_btexch4_sampled %>% 
      distinct()

d_summary <- d_btexch4_sampled %>%
      group_by(facility_id, facility_type, utm_x83, utm_y83, permit_number, receipt_number,
               well_depth, wattenberg) %>%
      summarize(btex_sampled = any(btex_sampled),
                ch4_sampled = any(ch4_sampled),
                btex_detected = any(btex_detected),
                ch4_detected = any(ch4_detected))

d_summary2 <- d_summary %>%
      left_join(d_ch4_origins, by = c("facility_id"))

# write.csv(d_summary2, "Output/Wattenberg_Well_Summary_2021-06-07.csv")


c_tbl <- d_summary2 %>%
   ungroup() %>%
   filter(ch4_sampled == TRUE, btex_sampled == TRUE) %>%
   mutate(btex_detected2 = if_else(btex_detected == TRUE, "BTEX Detected", "BTEX Not Detected"),
          ch4_detected2 = if_else(ch4_detected == TRUE, "CH4 Detected", "CH4 Not Detected")) %>%
   select(c(facility_id, btex_sampled, ch4_sampled, btex_detected2, ch4_detected2))

c2_test <- chisq.test(table(c_tbl$btex_detected2, c_tbl$ch4_detected2))


# Stats Question 2 --------------------------------------------------------
# Does the rate of occurrences (btex/samples) increase over time
# Use Mann-Kendall analysis for monotonic increase/decrease
# Null hypothesis: there is no increase or decrease in BTEX occurrences
# over time


rm(list=ls())
graphics.off()

# Read final cases
d_cases <- read.xlsx("Output/Final_Exports/BTEX_Cases_2021-06-28.xlsx",
                     detectDates = TRUE)

# Read features from GIS
# Create spatial dataframes from features
f_btex <- arc.open("D:/Google Drive/ArcGIS/DJ_Basin_BTEX_Study/Data/1_Geodatabase/Water_Wells.gdb/Water_Wells_Sampled_BTEX_methane_20210302")
f_gas <- arc.open("D:/Google Drive/ArcGIS/DJ_Basin_BTEX_Study/Data/1_Geodatabase/Water_Wells.gdb/Water_Wells_Sampled_GAS_20210628")
all_samples <- arc.open("D:/Google Drive/ArcGIS/DJ_Basin_BTEX_Study/Data/1_Geodatabase/Water_Wells.gdb/All_Water_Samples")

d_btex <- arc.select(f_btex) %>%
   janitor::clean_names() %>%
   mutate(sample_date = as.Date(sample_date))

d_gas <- arc.select(f_gas) %>%
   janitor::clean_names() %>%
   mutate(wattenberg = as.logical(wattenberg),
          sample_date = as.Date(sample_date))

d_s <- arc.select(all_samples) %>%
   janitor::clean_names() %>%
   mutate(sample_date = as.Date(sample_date))

# Summarize gas data
d_gas <- d_gas %>%
   mutate(origin = case_when(delta_13c_c1 < -55.0 & fraction > 100 ~ "biogenic",
                             delta_13c_c1 >= -55.0 & fraction <= 100 ~ "thermogenic",
                             TRUE ~ "mixed"))

# Summarize wells by sample origin
# Might have same well with 1 sample thermogenic, 1 sample mixed origins
d_gas2 <- d_gas %>%
   group_by(facility_id, facility_type, utm_x83, utm_y83, permit_number, receipt_number, origin) %>%
   summarize(n_samples = n(),
             mean_fraction = mean(fraction),
             mean_d13 = mean(delta_13c_c1),
             min_d13 = min(delta_13c_c1),
             max_d13 = max(delta_13c_c1),
             sample_date = min(sample_date)) %>%
   ungroup()

d_thermogenic <- d_gas2 %>% 
   filter(origin %in% c("thermogenic", "mixed")) %>%
   group_by(facility_id) %>%
   summarize(min_date = min(sample_date),
             origin = origin) %>%
   ungroup() %>%
   arrange(min_date) %>%
   mutate(id = 1:n())

thermogenic_plot <- ggplot(data = d_thermogenic, aes(x = min_date, y = id, fill = origin)) +
   geom_point(shape = 21, size = 3) +
   scale_x_date(limits = as.Date(c("1990-01-01", "2020-01-01")),
                breaks = "1 year",
                date_labels = "%Y") +
   scale_fill_manual(values = c("#4A148C", "#33a02c")) + 
   labs(title = "Cumulative Occurrences of Thermogenic Methane in Water Wells",
        subtitle = paste0("Between ", min(d_thermogenic$min_date)," and ", max(d_thermogenic$min_date)),
        fill = "Co-Occurring Methane",
        x = "Sample Date",
        y = paste0("Cumulative Thermogenic Methane Detection (n = ", max(d_thermogenic$id), ")")) +
   theme(legend.position=c(0.01,0.99), legend.justification=c(0,1))
thermogenic_plot

# Get list of unique BTEX occurrences by FACID and SampleDate
btex_facid <- as.list(unique(paste(d_btex$facility_id, d_btex$sample_date)))

# Merge the BTEX detections with the gas data
d_btex_gas <- d_cases %>%
   group_by(facility_id) %>%
   filter(permit_number != "Unknown") %>%
   ungroup() %>%
   arrange(earliest_detection) %>%
   mutate(id = 1:n(),
          origin2 = ifelse(is.na(origin), "no ch4", origin))

# Detections and samples plots
blankPlot <- ggplot()+geom_blank(aes(1,1))+
   theme(plot.background = element_blank(), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         panel.border = element_blank(),
         panel.background = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.x = element_blank(), 
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

cumulative_plot <- ggplot(data = d_btex_gas, aes(x = earliest_detection, y = id,
                                                 fill = factor(origin2,
                                                               levels = c("thermogenic",
                                                                          "mixed",
                                                                          "biogenic", 
                                                                          "no ch4"),
                                                               labels = c("Thermogenic Origin",
                                                                          "Mixed Origin",
                                                                          "Biogenic Origin",
                                                                          "No Co-Occurring Methane")))) +
   geom_point(shape = 21, size = 3) +
   scale_fill_manual(values = c("#33a02c", "#4A148C", "#1f78b4", "#a6cee3")) +
   scale_x_date(limits = as.Date(c("2001-01-01", "2020-01-01")),
                breaks = "1 year",
                date_labels = "%Y") +
   labs(title = "Cumulative Occurrences of BTEX in Water Wells",
        subtitle = "Between 2001 and 2020",
        fill = "Co-Occurring Methane",
        x = "Sample Date",
        y = "Cumulative BTEX Detection") +
   theme(legend.position = c(0.01, 0.986), legend.justification = c(0,1))
cumulative_plot

# Will add to the cumulative plot
samples <- ggplot(d_s, aes(x = sample_date)) +
   geom_histogram(binwidth = 365) +
   scale_x_date(limits = as.Date(c("2001-01-01", "2020-01-01")),
                breaks = "1 year",
                date_labels = "%Y") +
   labs(x = "Sample Date",
        y = "Number of Samples")
duo_plot <- grid.arrange(cumulative_plot, samples, ncol=1, nrow=2)

# Normalized bar plot
# time_period <- "year"
# time_period <- "month"
# time_period <- "quarter"
time_period <- "halfyear"
time_period <- "year"

period_cases <- d_btex_gas %>%
   group_by(period = floor_date(earliest_detection, time_period)) %>%
   summarize(btex_occurrences = n())

period_samples <- d_s %>%
   group_by(period = floor_date(sample_date, time_period)) %>%
   summarize(samples = n()) %>%
   ungroup() %>%
   left_join(period_cases, by = "period") %>%
   mutate(normalized_cases = btex_occurrences/samples)

norm_plt <- ggplot(period_samples, aes(x = period, y = normalized_cases)) +
   geom_bar(stat = "identity") +
   scale_x_date(limits = as.Date(c("2001-01-01", "2020-01-01")),
                breaks = "1 year",
                date_labels = "%Y") +
   labs(title = "Number of BTEX Detections Normalized to Number of Samples",
        subtitle = paste0("During a ", time_period, " period"),
        x = "Year",
        y = "BTEX Detections per Sample")
norm_plt
duo_plot <- grid.arrange(cumulative_plot, norm_plt, ncol=1, nrow=2)

library(kableExtra)
library(mblm)
library(trend)

