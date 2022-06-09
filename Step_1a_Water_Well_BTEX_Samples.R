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
# 06-06-2020      MS created script
# 10/17/2020      Changed to BTEX data clean only
# 02/12/2021      Changed to include methane data in water samples along with BTEX (wide format)
#                 Removed trip blanks
# 06/07/2021      Script only for water data. Gas data moved into Step_1b_Water_Well_Gas_Samples.R

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

water_wells <- read_csv("Input/Step_1_Original_Data/DL_Locations.csv", 
                        col_types = cols(PermitNumber = col_character(), 
                                         ReceiptNumber = col_character())) %>% clean_names()
samples  <- read_csv("Input/Step_1_Original_Data/DL_Samples.csv") %>%
      mutate(`Sample Date` = as.Date(`Sample Date`, format="%m/%d/%Y")) %>% clean_names()
results <- read.csv("Input/Step_1_Original_Data/DL_Results.csv")  %>% clean_names() # use "read.csv()" because of per-mille char encoding


# Selection Criteria ------------------------------------------------------
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

nd_qual <- c("U", "UJ", "nd", "ND", "U, H", "<", "u")

# Water Well and Sample Information ---------------------------------------
# Filter for sampled wells that are desired facilities
wells_sel <- water_wells %>% filter(facility_type %in% facilities)

# Merge water wells with all samples
# This df to be used for methane origin analysis
samples_all <- wells_sel %>% left_join(samples, by = c("facility_id"))

# Merge water wells with the sample information
# This df to be used for BTEX results
samples_water <- wells_sel %>% 
      left_join(samples, by = c("facility_id")) %>%
      filter(matrix %in% c("WATER", "Water", "GW", "LIQUID", ""))

# Results for WATER Data (BTEX and Methane) -------------------------------
# Cleanup sample results before left_joining to water wells
# Make units consistent
results_water_btex <- results %>%
      replace_na(list(detection_limit = 0)) %>%
      filter(param_description %in% btex) %>%
      mutate(chem_class = "BTEX",
             result_ugL = case_when(units == "mg/L" ~ result_value*1000,
                                    units %in% c("ug/L", "UG/L") ~ result_value),
             detection_limit = case_when(units == "mg/L" ~ detection_limit*1000,
                                         units %in% c("ug/L", "UG/L") ~ detection_limit),
             units2 = case_when(units == "mg/L" ~ "ug/L",
                                units %in% c("ug/L", "UG/L") ~ "ug/L",
                                TRUE ~ as.character(units))) %>%
      filter(!is.na(units2))
length(unique(results_water_btex$sample_id))

# Get BTEX detections
# If positive result is above a DL=0, check if qualifier is ND
det_lims <- readRDS("Intermediate/det_lim_summary.RDS") %>%
      mutate(match_param = c("BENZENE", "TOLUENE", "ETHYLBENZENE", "m-+p-XYLENE", "o-XYLENE", "TOTAL XYLENES"))
      
results_water_btex_det <- results_water_btex %>%
      mutate(detection_limit = ifelse(detection_limit==0, 
                                      case_when(param_description == "BENZENE" ~ det_lims$dl_max[1],
                                                param_description == "TOLUENE" ~ det_lims$dl_max[2],
                                                param_description == "ETHYLBENZENE" ~ det_lims$dl_max[3],
                                                param_description == "m-+p-XYLENE" ~ det_lims$dl_max[4],
                                                param_description == "o-XYLENE" ~ det_lims$dl_max[5],
                                                param_description == "TOTAL XYLENES" ~ det_lims$dl_max[6]),
                                      detection_limit),
             det = result_ugL>detection_limit) %>%
      filter(result_ugL > detection_limit,
             !qualifier %in% nd_qual)

# Get sample IDs for samples with positive BTEX detections
det_btex_IDs <- unique(results_water_btex$sample_id)

# Get all results for methane sampled in water
# Make units consistent
results_water_ch4 <- results %>%
      filter(param_description == "METHANE") %>%
      mutate(chem_class = "Methane",
             result_mgL = case_when(units %in% c("mg/L", "MG/L", "mg/l", "ppm") ~ result_value,
                                    units %in% c("ug/L", "UG/L") ~ result_value/1000),
             units2 = case_when(units %in% c("mg/L", "MG/L", "mg/l", "ppm") ~ "mg/L",
                                units %in% c("ug/L", "UG/L") ~ "mg/L",
                                TRUE ~ as.character(units))) %>%
      filter(!is.na(result_mgL))
length(unique(results_water_ch4$sample_id))

# Get methane detections
# If positive result is above a DL=0, check if qualifier is ND
results_water_ch4_det <- results_water_ch4 %>%
      filter(result_value > detection_limit,
             !qualifier %in% nd_qual)

# Bind the BTEX (detected) with methane (both detected and not)
# Methane is both detected and ND to show if BTEX occurs with methane, and what type of methane
results_water <- bind_rows(results_water_btex_det, results_water_ch4)

# Bind the BTEX (both detected and not) with methane (both detected and not)
results_water_nd <- bind_rows(results_water_btex, results_water_ch4)
# %>%
#    distinct(sample_id, chem_class) %>%
#    group_by(sample_id) %>%
#    summarize(n = n()) %>%
#    filter(n == 2)
length(unique(results_water_nd$sample_id))

# Join RESULT DATA with SAMPLE DATA ---------------------------------------
# Select relevant columns

d_water <- samples_water %>%
      inner_join(results_water, by = c("sample_id")) %>%
      select(facility_id, facility_type, utm_x83, utm_y83, county, permit_number,
             receipt_number, well_depth, sample_id, sample_date, matrix, sample_reason,
             result_id, param_description, result_ugL, qualifier, units2,
             detection_limit, chem_class)

d_water_nds <- samples_water %>%
      inner_join(results_water_nd, by = c("sample_id")) %>%
      select(facility_id, facility_type, utm_x83, utm_y83, county, permit_number,
             receipt_number, well_depth, sample_id, sample_date, matrix, sample_reason,
             result_id, param_description, result_ugL, qualifier, units2,
             detection_limit, chem_class) %>%
      mutate(x = utm_x83, y = utm_y83)

# Remove Trip Blanks and/or Contaminated Samples --------------------------

# NOT NEEDED FOR METHANE - currently only care about origin (microbial or thermogenic)
# df of the trip blanks with high values
tb_fail <- samples_water %>%
      left_join(results_water_btex, by = c("sample_id")) %>%
      filter(grepl("blank", sample_reason, ignore.case = T),
             !qualifier %in% c("U", "<", "ND"),
             result_value > detection_limit) %>%
      select(facility_id, sample_id, sample_date, sample_reason, param_description, result_ugL)

tb <- samples_water %>%
      left_join(results_water_btex, by = c("sample_id")) %>%
      filter(grepl("blank", sample_reason, ignore.case = T)) %>%
      select(facility_id, sample_id, sample_date, sample_reason, param_description, result_ugL)

# df of the samples collected where trip blanks were contaminated
wells_tb_fail <- d_water %>%
      filter(facility_id %in% as.list(tb_fail$facility_id),
             sample_date %in% as.list(tb_fail$sample_date)) %>%
      select(sample_id, facility_id, sample_date, sample_reason)

# remove samples collected with contaminated trip blanks
d_water2 <- d_water %>%
      filter(!grepl("blank", sample_reason, ignore.case = T),
             !sample_id %in% as.list(wells_tb_fail$sample_id)) %>%
      mutate(x = utm_x83, y = utm_y83)

# Select by GIS location in Wattenberg Field

sf_water <- st_as_sf(d_water2,
                     coords = c("x", "y"),
                     crs = 26913)
sf_water_nd <- st_as_sf(d_water_nds,
                        coords = c("x", "y"),
                        crs = 26913)

d_wattenberg <- arc.open("Input\\Base_Features.gdb\\Wattenberg_Field_UTM_polygon")
sf_wattenberg <- arc.select(d_wattenberg, where_clause = "description = '2020 revision'")
sf_wattenberg <- arc.data2sf(sf_wattenberg)

sf_water2 <- st_join(sf_water, sf_wattenberg, join = st_within) %>%
      filter(description == "2020 revision") %>%
      select(-c(OBJECTID, description)) %>%
      mutate(wattenberg = TRUE)
length(unique(sf_water2$sample_id))
length(unique(sf_water2$facility_id))

sf_water_nd2 <- st_join(sf_water_nd, sf_wattenberg, join = st_within) %>%
      filter(description == "2020 revision") %>%
      select(-c(OBJECTID, description)) %>%
      mutate(wattenberg = TRUE)
# how many measured both btex and methane?
data.frame(sf_water_nd2) %>% distinct(sample_id, chem_class) %>% group_by(sample_id) %>% summarize(n=n()) %>% filter(n>1) %>%nrow
data.frame(sf_water_nd2) %>% distinct(facility_id, chem_class) %>% group_by(facility_id) %>% summarize(n=n()) %>% filter(n>1) %>%nrow


# FACIDs that should be exluded for following reasons:
exclude_facids <- data.frame(facility_id = c("700923","703535","705037","705043",
                                             "750100","752255","754338","754563",
                                             "754564","755482","757246"),
                             reason = c("No permit information","No permit information","No permit information","No permit information",
                                        "No permit information","No permit information","Contaminated monitoring well","Contaminated monitoring well",
                                        "Contaminated monitoring well","Contaminated monitoring well","Permit does not exist"))


# Final Set of BTEX Detections --------------------------------------------

sf_water3 <- sf_water2 %>%
      filter(chem_class == "BTEX") %>%
      # Add manually found permit/receipt numbers and reconcile
      left_join(read.xlsx("Input/BTEX_Wells_DWR_Info.xlsx", sheet="summary"), by = c("facility_id" = "FacilityID")) %>%
      mutate(permit_number = ifelse(is.na(permit_number), PermitNumber, permit_number),
             receipt_number = ifelse(is.na(receipt_number), ReceiptNumber, receipt_number)) %>%
      select(-c(PermitNumber, ReceiptNumber)) %>%
      filter(!facility_id %in% as.list(exclude_facids$facility_id),
             !is.na(permit_number),
             !is.na(detection_limit)) %>%
      mutate(param_description = factor(param_description,
                                        levels = c("BENZENE", "TOLUENE", "ETHYLBENZENE",
                                                   "m-+p-XYLENE", "o-XYLENE", "TOTAL XYLENES"),
                                        labels = c("Benzene", "Toluene", "Ethylbenzene",
                                                   "mp-Xylenes", "o-Xylene", "Total Xylenes")))

sf_water_nd3 <- sf_water_nd2 %>%
      filter(!facility_id %in% as.list(exclude_facids$facility_id)) %>%
      mutate(param_description = factor(param_description,
                                        levels = c("BENZENE", "TOLUENE", "ETHYLBENZENE",
                                                   "m-+p-XYLENE", "o-XYLENE", "TOTAL XYLENES"),
                                        labels = c("Benzene", "Toluene", "Ethylbenzene",
                                                   "mp-Xylenes", "o-Xylene", "Total Xylenes")))

# Create summary tables:
# Table 1: Summary of detection limits
summary_btex_dl <- sf_water3 %>%
      as.data.frame() %>%
      group_by(param_description, units2) %>%
      summarize(dl_min = min(detection_limit),
                dl_max = max(detection_limit),
                n_samples = n())
# saveRDS(summary_btex_dl, "Intermediate/det_lim_summary.RDS")

#Table 2: Summary of detected BTEX results
summary_btex <- sf_water3 %>%
      as.data.frame() %>%
      group_by(param_description, units2) %>%
      summarize(result_min = min(result_ugL),
                result_max = max(result_ugL),
                result_med = median(result_ugL),
                result_mean = mean(result_ugL),
                n_wells = length(unique(facility_id)),
                n_samples = length(unique(sample_id))) %>%
      ungroup() %>%
      arrange(param_description)

summary_btex_cases <- sf_water3 %>%
      as.data.frame() %>%
      arrange(param_description) %>%
      rename(Result = result_ugL, `Detection Limit` = detection_limit) %>%
      pivot_wider(id_cols = c(facility_id, facility_type, utm_x83, utm_y83, permit_number, receipt_number, well_depth, matrix, sample_id, sample_date),
                  names_from = c(param_description),
                  names_sep = "; ",
                  values_from = c(Result, `Detection Limit`),
                  names_vary = "slowest",
                  values_fn = mean)
length(unique(summary_btex_cases$facility_id))

summary_btex_cases_mean <- sf_water3 %>%
   as.data.frame() %>%
   arrange(param_description) %>%
   rename(Result = result_ugL, `Detection Limit` = detection_limit) %>%
   pivot_wider(id_cols = c(facility_id, facility_type, utm_x83, utm_y83, permit_number, receipt_number, well_depth, matrix),
               names_from = c(param_description),
               names_sep = "; ",
               values_from = c(Result, `Detection Limit`),
               names_vary = "slowest",
               values_fn = mean)

write.xlsx(list(`BTEX Cases` = summary_btex_cases,
                `BTEX Cases (Mean)` = summary_btex_cases_mean,
                `Detection Limits Summary` = summary_btex_dl,
                `BTEX Summary` = summary_btex),
           paste0("Output/Final_Exports/BTEX_Cases_", Sys.Date(), ".xlsx"),
           overwrite = TRUE,
           firstRow = TRUE,
           colWidths = "auto",
           withFilter = TRUE)

# Double checks
# tmp_checks <- sf_water3 %>% 
#    as.data.frame() %>% 
#    filter(param_description == "TOLUENE") %>%
#    distinct(sample_id)
#    
#    group_by(param_description) %>% 
#    summarize(n_wells = n(facility_id),
#              n_samples = n(sample_id))

# Output and Tables for GIS -----------------------------------------------

# Format output table of BTEX 
# FOR ALL LOCATIONS: d_water2; FOR WATTENBERG FIELD: sf_water3
# Wide table for easier reading
d_export <- d_water2 %>%
      filter(sample_id %in% det_btex_IDs) %>%
      pivot_wider(id_cols = c(facility_id, facility_type, utm_x83, utm_y83, permit_number, receipt_number, well_depth, matrix, sample_id, sample_date),
                  names_from = c(param_description),
                  values_from = c(result_ugL),
                  values_fn = mean) %>%
      rename(benzene_ugl = `BENZENE`,
             toluene_ugl = `TOLUENE`,
             ethylbenzene_ugl = `ETHYLBENZENE`,
             mp_xylenes_ugl = `m-+p-XYLENE`,
             o_xylene_ugl = `o-XYLENE`,
             tot_xylenes_ugl = `TOTAL XYLENES`,
             methane_mgl = `METHANE`) %>%
      relocate(c(benzene_ugl, toluene_ugl, ethylbenzene_ugl, mp_xylenes_ugl, o_xylene_ugl, tot_xylenes_ugl, methane_mgl),
               .after = sample_date) %>%
      clean_names() %>%
      mutate(x = utm_x83, y = utm_y83,
             btex_sum = select(., 11:16) %>% rowSums(na.rm = TRUE),
             btex_det = ifelse(btex_sum > 0, TRUE, FALSE))

# Create dataframe of water wells sampled for BTEX/ch4
# ALSO INCLUDES ND SAMPLES
d_export_water_NDs <- d_water_nds %>%
      pivot_wider(id_cols = c(facility_id, facility_type, utm_x83, utm_y83, permit_number, receipt_number, well_depth, matrix, sample_id, sample_date),
                  names_from = c(param_description),
                  values_from = c(result_ugL),
                  values_fn = mean) %>%
      rename(benzene_ugl = `BENZENE`,
             toluene_ugl = `TOLUENE`,
             ethylbenzene_ugl = `ETHYLBENZENE`,
             mp_xylenes_ugl = `m-+p-XYLENE`,
             o_xylene_ugl = `o-XYLENE`,
             tot_xylenes_ugl = `TOTAL XYLENES`,
             methane_mgl = `METHANE`) %>%
      relocate(c(benzene_ugl, toluene_ugl, ethylbenzene_ugl, mp_xylenes_ugl, o_xylene_ugl, tot_xylenes_ugl, methane_mgl),
               .after = sample_date) %>%
      clean_names() %>%
      mutate(x = utm_x83, y = utm_y83,
             btex_sum = select(., 11:16) %>% rowSums(na.rm = TRUE),
             btex_det = ifelse(btex_sum > 0, as.character("TRUE"), as.character("FALSE")))

# Write Output ------------------------------------------------------------
feature_name_water_det <- paste("D:/Google Drive/ArcGIS/DJ_Basin_BTEX_Study/Data/1_Geodatabase/Water_Wells.gdb/Water_Wells_Sampled_BTEX_",
                                format(Sys.Date(), "%Y%m%d"), sep = "")

feature_name_water_ND <- paste("D:/Google Drive/ArcGIS/DJ_Basin_BTEX_Study/Data/1_Geodatabase/Water_Wells.gdb/Water_Wells_Sampled_BTEX_ND_",
                               format(Sys.Date(), "%Y%m%d"), sep = "")

arc.write(path = file.path(feature_name_water_det),
          data = d_export,
          coords = c("utm_x83", "utm_y83"),
          shape_info = list(type = "Point", WKID = 26913),
          overwrite = TRUE)

arc.write(path = file.path(feature_name_water_ND),
          data = d_export_water_NDs,
          coords = c("utm_x83", "utm_y83"),
          shape_info = list(type = "Point", WKID = 26913),
          overwrite = TRUE)

write.csv(d_export, paste0("Output/Water_Wells_Sampled_DetectedBTEX_CH4_", Sys.Date(), ".csv"))
