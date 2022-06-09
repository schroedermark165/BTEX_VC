# Purpose -----------------------------------------------------------------
# Load COGCC water well sampling data. It is provided as MS Access database
# of locations, samples, and sample results.
# 
# Merge the locations to the sample IDs and their respective results. This
# can be plotted in ARCMAP.
# 
# PROJECT INFORMATION:
#     Name:Denver-Julesburg Basin Water-Well BTEX Study 
#     Description: Analysis of water quality impacts from oil and gas development.
# 
# HISTORY:
# 06/07/2021      MS created script to organize gas sample data

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(lubridate)
library(janitor)
library(ggsci)
library(scales)
library(arcgisbinding)
library(sf)

arc.check_product()

# Load Initial Data -------------------------------------------------------
rm(list = ls())

d_wattenberg <- arc.open("D:\\Google Drive\\ArcGIS\\DJ_Basin_BTEX_Study\\Data\\1_Geodatabase\\Base_Features.gdb\\Wattenberg_Field_UTM_polygon")
sf_wattenberg <- arc.select(d_wattenberg, where_clause = "description = '2020 revision'")
sf_wattenberg <- arc.data2sf(sf_wattenberg)

water_wells <- read_csv("Input/Step_1_Original_Data/DL_Locations.csv", 
                        col_types = cols(PermitNumber = col_character(), 
                                         ReceiptNumber = col_character())) %>% clean_names()
samples  <- read_csv("Input/Step_1_Original_Data/DL_Samples.csv") %>%
      mutate(`Sample Date` = as.Date(`Sample Date`, format="%m/%d/%Y")) %>% clean_names()
results <- read.csv("Input/Step_1_Original_Data/DL_Results.csv")  %>% clean_names()      # use "read.csv()" because of per-mille char encoding

# Selection Criteria ------------------------------------------------------
facilities <- c("Domestic Well",
                "Ground Water", "Groundwater",
                "Stock or  Irrigation",
                "Irrigation",
                "Municipal",
                "Commercial")

gas <- c("METHANE",
         "ETHANE",
         "PROPANE",
         "DELTA 13C C1")

nd_qual <- c("U", "UJ", "nd", "ND", "U, H", "<", "u")

# Water Well and Sample Information ---------------------------------------

# Filter for sampled wells that are desired facilities
water_wells <- water_wells %>% filter(facility_type %in% facilities)

# Merge water wells with all samples
# This df to be used for methane origin analysis
d_samples <- samples %>% 
      left_join(water_wells, by = c("facility_id"))

d_results <- results %>%
      left_join(d_samples, by = c("sample_id"))

d_results_gas <- d_results %>% filter(param_description %in% gas)

# Results for GAS Samples -------------------------------------------------

# Cleanup sample results before left_joining gas samples to water wells
d_gas <- d_results_gas %>%
   # Convert all units to "per mil" and "mol %"
   mutate(result2 = case_when(units %in% c("\u0089", "‰", "per mil", "Per mil", "per mil VPDB", "per mil VSMOW") ~ result_value,
                              units %in% c("%", "MOL %", "Mol %") ~ result_value),
          units2 = case_when(units %in% c("‰", "per mil", "Per mil", "per mil VPDB", "per mil VSMOW") ~ "per mil",
                             units %in% c("%", "MOL %", "Mol %") ~ "MOL %",
                             TRUE ~ as.character(units))) %>% filter(!is.na(result2))

# Output and Tables for GIS -----------------------------------------------
# Format output table of methane and isotope data
# Wide table for easier reading
d_gas_wd <- d_gas %>%
   pivot_wider(id_cols = c(facility_id, facility_type, utm_x83, utm_y83, permit_number, receipt_number, well_depth, matrix, sample_id, sample_date),
               names_from = c(param_description),
               values_from = c(result2),
               values_fn = mean) %>%
   clean_names() %>%
   filter(complete.cases(delta_13c_c1, methane, ethane, propane, utm_x83, utm_y83),      # Has well information
          permit_number != "",
          utm_x83 > 0)  %>%
   mutate(sample_date = as.character(sample_date),
          methane_units = "mol %",
          d13c_units = "per mil",
          fraction = methane/(ethane+propane),
          origin = case_when((delta_13c_c1 < -55.0 & fraction > 100) ~ "microbial",
                             (delta_13c_c1 >= -55.0 & fraction <= 100) ~ "thermogenic",
                             TRUE ~ "mixed"),
          x = utm_x83, y = utm_y83) %>%
   relocate(methane_units, .after = propane)

sf_gas_wd <- st_as_sf(d_gas_wd,
                      coords = c("x", "y"),
                      crs = 26913)

sf_gas_wd2 <- st_join(sf_gas_wd, sf_wattenberg, join = st_within) %>%
   mutate(wattenberg = ifelse(is.na(description), "FALSE", "TRUE")) %>%
   select(-c(OBJECTID, description))

output <- data.frame(sf_gas_wd2) %>% select(-geometry)

# Write Output ------------------------------------------------------------
# feature_name_gas <- paste("D:/Google Drive/ArcGIS/DJ_Basin_BTEX_Study/Data/1_Geodatabase/Water_Wells.gdb/Water_Wells_Sampled_GAS_",
#                           format(Sys.Date(), "%Y%m%d"), sep = "")
# 
# arc.write(path = file.path(feature_name_gas),
#           data = sf_gas_wd2,
#           shape_info = list(type = "Point", WKID = 26913),
#           overwrite = TRUE)
write.xlsx(output, paste0("Output/Water_Wells_Sampled_Methane_Isotope_", Sys.Date(), ".xlsx"))
