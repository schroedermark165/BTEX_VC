#================================================================================
# PURPOSE:
# The spatial analysis in ArcGIS now is to be summarized as table.
# This takes the Wattenberg field BTEX detections and summarizes the well locations,
# BTEX detections, and neighboring evidence that could explain the occurrence.
#
# PROJECT INFORMATION:
#     Name:Denver-Julesburg Basin Water-Well BTEX Study 
#     Description: Analysis of water quality impacts from oil and gas development.
# 
# HISTORY:
# 06-06-2020      MS created script
# 10/17/2020      Changed to BTEX data clean only
# 
#================================================================================
# LOAD PACKAGES
library(tidyverse)
library(openxlsx)
library(lubridate)
library(readr)
library(arcgisbinding)
library(janitor)
arc.check_product()


# Load data ---------------------------------------------------------------
rm(list = ls())

d_wells <- read.xlsx("Input/BTEX_Wells_DWR_Info.xlsx") #Manually collected well info (permit #s, web addresses)
d_water <- read.csv("Output/Water_Wells_Sampled_DetectedBTEX_CH4_2021-07-08.csv")

gdb_btex <- arc.open("D:/Google Drive/ArcGIS/DJ_Basin_BTEX_Study/Data/1_Geodatabase/Water_Wells.gdb/Water_Wells_Sampled_BTEX_methane_20210302")
d_btex <- arc.select(gdb_btex) %>%
   select(-c(OBJECTID, Join_Count, TARGET_FID)) %>%
   mutate(SampleDate = as.Date(SampleDate, format="%m/%d/%Y"),
          Methane_mg_l = as.numeric(Methane_mg_l)) %>%
   clean_names()

gdb_gas <- arc.open("D:/Google Drive/ArcGIS/DJ_Basin_BTEX_Study/Data/1_Geodatabase/Water_Wells.gdb/Water_Wells_Sampled_GAS_20210628")
d_gas <- arc.select(gdb_gas, where_clause = "wattenberg = 'TRUE'") %>% clean_names()

d_gas <- d_gas %>% mutate(origin = case_when(delta_13c_c1 < -55.0 & fraction > 100 ~ "biogenic",
                                             delta_13c_c1 >= -55.0 & fraction <= 100 ~ "thermogenic",
                                             TRUE ~ "mixed"),
                          sample_date = as.Date(sample_date))


# Clean and organize data
d_gas2 <- d_gas %>%
   group_by(facility_id, facility_type, utm_x83, utm_y83, permit_number, receipt_number, origin) %>%
   summarize(n_samples = n(),
             mean_fraction = mean(fraction),
             mean_d13 = mean(delta_13c_c1),
             min_d13 = min(delta_13c_c1),
             max_d13 = max(delta_13c_c1))

d_gas3 <- d_gas2 %>%
   group_by(facility_id) %>%
   summarize(n_samples = sum(n_samples),
             mean_fraction = mean(mean_fraction),
             mean_d13 = mean(mean_d13),
             min_d13 = min(min_d13),
             max_d13 = max(max_d13)) %>%
   ungroup() %>%
   mutate(origin = case_when((max_d13 >= -55.0 & mean_fraction <= 100) ~ "thermogenic",
                             (max_d13 < -55.0 & mean_fraction > 100) ~ "biogenic",
                             TRUE ~ as.character("mixed")))

# Additional well info to append
well_info <- d_wells %>% 
   clean_names() %>%
   select(-c(permit_number, receipt_number, utm_x83, utm_y83, facility_type, notes)) %>%
   mutate(well_depth = as.numeric(well_depth))

d_btex2 <- d_btex %>%
   left_join(well_info, by = "facility_id", suffix = c("_gis", "_addl")) %>%
   filter(permit_number != "Unknown") 

d_unique <- d_btex2 %>%
   mutate(well_depth = case_when(is.na(well_depth_gis) ~ well_depth_addl,
                                 TRUE ~ well_depth_gis)) %>%
   group_by(facility_id, facility_type, utm_x83, utm_y83, permit_number, receipt_number,
            well_integrity, scp, c_scp, well_depth, aquifer, nearest_og_well,
            nearest_scp, nearest_cscp, more_information) %>%
   mutate_at(vars(11:17), ~replace_na(., 0)) %>%
   summarize(earliest_detection = min(sample_date),
             benzene_ugl = max(benzene_ug_l),
             toluene_ugl = max(toluene_ug_l),
             ethylbenzene_ugl = max(ethylbenzene_ug_l),
             mp_xylenes_ugl = max(mp_xylenes_ug_l),
             o_xylene_ugl = max(o_xylene_ug_l),
             tot_xylenes_ugl = max(tot_xylenes_ug_l),
             methane_mgl = max(methane_mg_l)) %>%
   left_join(d_gas3) %>%
   mutate(order_id = case_when(origin == "thermogenic" ~ 1,
                               origin == "biogenic" ~ 2,
                               origin == "mixed" ~ 3,
                               is.na(origin) ~ 4)) %>%
   mutate(origin = ifelse(methane_mgl == 0, NA, origin))

cases <- d_unique %>%
   relocate(c(well_depth, aquifer), .after = c(receipt_number)) %>%
   relocate(c(17:23), .after = c(aquifer)) %>%
   relocate(c(origin), .after = methane_mgl)

wb = createWorkbook()
sheet1 = paste("BTEX_Cases")
addWorksheet(wb, sheet1)
writeData(wb, sheet1, cases)
saveWorkbook(wb, paste0("Output/Final_Exports/BTEX_Cases_", Sys.Date(), ".xlsx"), overwrite = T)

write.csv(cases, paste0("Output/Final_Exports/BTEX_Cases_", Sys.Date(), ".csv"))
