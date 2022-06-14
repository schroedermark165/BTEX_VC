# Purpose -----------------------------------------------------------------
# Create wide table for Greg
# Has ALL samples for BTEX and/or METHANE
# 
# Merge the locations to the sample IDs and their respective results. This
# can be plotted in ARCMAP.
# 
# PROJECT INFORMATION:
#     Name:Denver-Julesburg Basin Water-Well BTEX Study 
#     Description: Analysis of water quality impacts from oil and gas development.
# 
# HISTORY:
# 07/14/2021      Created script

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

# Methane data from Step_1b_Water_Well_Gas_Samples.R
d_isotope <- readRDS("Intermediate/Water_Wells_Sampled_Methane_Isotope.RDS") %>%
      mutate(sample_date = as.Date(sample_date),
             facility_id = as.character(facility_id)) %>%
      select(facility_id, sample_date, methane, ethane, propane, delta_13c_c1, fraction, origin)

# Read original well/sample/results datasets
water_wells <- read_csv("Input/Step_1_Original_Data/DL_Locations.csv", 
                        col_types = cols(PermitNumber = col_character(), 
                                         ReceiptNumber = col_character())) %>% clean_names()
samples  <- read_csv("Input/Step_1_Original_Data/DL_Samples.csv") %>%
      mutate(`Sample Date` = as.Date(`Sample Date`, format="%m/%d/%Y")) %>% clean_names()
results <- read.csv("Input/Step_1_Original_Data/DL_Results.csv")  %>% clean_names()      # use "read.csv()" because of per-mille char encoding

# Manually collected water well data (permits, etc)
well_info <- read.xlsx("Input/BTEX_Wells_DWR_Info.xlsx") %>%
      clean_names() %>%
      mutate(facility_id = as.character(facility_id))

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

alkanes3 <- c("ETHANE", "PROPANE", "BUTANE", "PENTANE", "HEXANES",
              "ISOBUTANE", "ISOPENTANE")

# Colorado MCLs
MCLs <- data.frame(analyte = c("BENZENE", "TOLUENE", "ETHYLBENZENE", "TOTAL XYLENES"),
                   co_mcl = c(5, 560, 700, 1400))

# Water Well and Sample Information ---------------------------------------
# Filter for sampled wells that are desired facilities
wells_sel <- water_wells %>% filter(facility_type %in% facilities)

# Merge water wells with all samples
# This df to be used for methane origin analysis
samples_all <- wells_sel %>% 
      left_join(samples, by = c("facility_id"))

# Merge water wells with all samples; filter for relevant facility types
# This df to be used for BTEX results
samples_water <- wells_sel %>% 
      left_join(samples, by = c("facility_id")) %>%
      filter(matrix %in% c("WATER", "Water", "GW", "LIQUID", ""))

# Results for WATER Data (BTEX and Methane) -------------------------------
# Cleanup sample results before left_joining to water wells
# Make units consistent

# Get BTEX detections
# If positive result is above a DL=0, check if qualifier is ND
det_lims <- readRDS("Intermediate/det_lim_summary.RDS") %>%
      mutate(match_param = c("BENZENE", "TOLUENE", "ETHYLBENZENE", "m-+p-XYLENE", "o-XYLENE", "TOTAL XYLENES"))

results_water_btex <- results %>%
      replace_na(list(detection_limit = 0)) %>%
      filter(param_description %in% btex) %>%
      # Convert all units to ug/l
      mutate(chem_class = "BTEX",
             result_ugL = case_when(units == "mg/L" ~ result_value*1000,
                                    units %in% c("ug/L", "UG/L") ~ result_value),
             detection_limit = case_when(units == "mg/L" ~ detection_limit*1000,
                                         units %in% c("ug/L", "UG/L") ~ detection_limit),
             units2 = case_when(units == "mg/L" ~ "ug/L",
                                units %in% c("ug/L           ", "ug/l","ug/L", "UG/L") ~ "ug/L",
                                TRUE ~ as.character(units))) %>%
      filter(units2 == "ug/L") %>%
      # Add the Colorado MCLs
      left_join(MCLs, by = c("param_description" = "analyte")) %>%
      # deal with records that are non-detect
      mutate(detection_limit = ifelse(detection_limit==0, 
                                      case_when(param_description == "BENZENE" ~ det_lims$dl_max[1],
                                                param_description == "TOLUENE" ~ det_lims$dl_max[2],
                                                param_description == "ETHYLBENZENE" ~ det_lims$dl_max[3],
                                                param_description == "m-+p-XYLENE" ~ det_lims$dl_max[4],
                                                param_description == "o-XYLENE" ~ det_lims$dl_max[5],
                                                param_description == "TOTAL XYLENES" ~ det_lims$dl_max[6]),
                                      detection_limit),
             result_ugL = case_when(result_value <= detection_limit ~ as.numeric(NA),
                                    qualifier %in% c("U", "<", "u", "nd", "ND") ~ as.numeric(NA),
                                    TRUE ~ result_ugL),
             above_co_mcl = case_when(result_ugL >= co_mcl ~ TRUE,
                                      TRUE ~ FALSE),
             sampled_for = 1)

# Get all results for methane sampled in water
# Make units consistent
results_water_ch4 <- results %>%
      filter(param_description == "METHANE") %>%
      mutate(result_ugL = case_when(units %in% c("mg/L", "MG/L", "mg/l", "ppm") ~ result_value,
                                    units %in% c("ug/L", "UG/L") ~ result_value/1000),
             units2 = case_when(units %in% c("mg/L", "MG/L", "mg/l", "ppm") ~ "mg/L",
                                units %in% c("ug/L", "UG/L") ~ "mg/L",
                                TRUE ~ as.character(units))) %>%
      filter(units2 == "mg/L") %>%
      mutate(result_ugL = case_when(result_value > detection_limit ~ result_ugL,
                                    result_value <= detection_limit ~ as.numeric(NA),
                                    result_value == 0 ~ as.numeric(NA),
                                    is.na(detection_limit) ~ as.numeric(NA),
                                    qualifier %in% c("U", "<", "u", "nd", "ND") ~ as.numeric(NA),
                                    TRUE ~ result_ugL),
             sampled_for = 2)

# Get all results for alkanes sampled in water
# Make units consistent
# Longer chain hydrocarbons indicate oil/gas presence because
# microbes only really produce methane and maybe some ethane. They should not produce propane, etc
# Filters for alkanes with 3+ carbons (propane -> hexane)
results_water_alkanes <- results %>%
      filter(param_description %in% alkanes3) %>%
      mutate(units = str_to_lower(units)) %>%
      mutate(result_ugL = case_when(units == "mg/l" ~ result_value,
                                    units == "ug/l" ~ result_value/1000),
             units2 = case_when(units == "mg/l" ~ "mg/l",
                                units == "ug/l" ~ "mg/l",
                                TRUE ~ as.character(units))) %>%
      filter(units2 == "mg/l") %>%
      # Not filtering bc we want to keep records of ND observations
      mutate(result_ugL = case_when(result_value > detection_limit ~ result_ugL,
                                    result_value <= detection_limit ~ as.numeric(NA),
                                    result_value == 0 ~ as.numeric(NA),
                                    is.na(detection_limit) ~ as.numeric(NA),
                                    qualifier %in% c("U", "<", "u", "nd", "ND") ~ as.numeric(NA),
                                    TRUE ~ result_ugL),
             sampled_for = 3)

# Bind the BTEX with methane (non-detects are included)
results_water_nd <- bind_rows(results_water_btex, results_water_ch4, results_water_alkanes)


# Join RESULT DATA with SAMPLE DATA ---------------------------------------
# Select relevant columns
d_water_nds <- samples_water %>%
      inner_join(results_water_nd, by = c("sample_id")) %>%
      select(facility_id, facility_type, utm_x83, utm_y83, county, permit_number,
             receipt_number, well_depth, sample_id, sample_date, matrix, sample_reason,
             result_id, param_description, result_ugL, qualifier, units2,
             detection_limit, chem_class, sampled_for, co_mcl, above_co_mcl)

# Remove Trip Blanks and/or Contaminated Samples --------------------------

# NOT NEEDED FOR METHANE - currently only care about origin (microbial or thermogenic)
# df of the trip blanks with high values
tb_fail <- samples_water %>%
      left_join(results_water_btex, by = c("sample_id")) %>%
      filter(grepl("blank", sample_reason, ignore.case = T),
             !qualifier %in% c("U", "<", "ND"),
             result_value > detection_limit) %>%
      select(facility_id, sample_id, sample_date, sample_reason, param_description, result_ugL)

# df of the samples collected where trip blanks were contaminated
wells_tb_fail <- d_water_nds %>%
      filter(facility_id %in% as.list(tb_fail$facility_id),
             sample_date %in% as.list(tb_fail$sample_date)) %>%
      select(sample_id, facility_id, sample_date, sample_reason)

# remove samples collected with contaminated trip blanks
d_water2 <- d_water_nds %>%
      filter(!grepl("blank", sample_reason, ignore.case = T),
             !sample_id %in% as.list(wells_tb_fail$sample_id)) %>%
      # Flag sample if above MCL
      group_by(sample_id, facility_id, sample_date, above_co_mcl) %>%
      mutate(btex_above_mcl = ifelse(sum(above_co_mcl) > 0, TRUE, FALSE)) %>%
      ungroup()

# Output and Tables for GIS -----------------------------------------------

# Format output table of BTEX
# Wide table of all samples and results for BTEX, CH4, and alkanes for easier reading
d_water_wd <- d_water2 %>%
      pivot_wider(id_cols = c(facility_id, facility_type, utm_x83, utm_y83, permit_number,
                              receipt_number, well_depth, matrix, sample_id, sample_date, btex_above_mcl),
                  names_from = c(param_description),
                  values_from = c(result_ugL),
                  values_fn = mean) %>%
      rename(benzene_ugl = `BENZENE`,
             toluene_ugl = `TOLUENE`,
             ethylbenzene_ugl = `ETHYLBENZENE`,
             mp_xylenes_ugl = `m-+p-XYLENE`,
             o_xylene_ugl = `o-XYLENE`,
             tot_xylenes_ugl = `TOTAL XYLENES`,
             methane_mgl = `METHANE`,
             ethane_mgl = `ETHANE`, 
             propane_mgl = `PROPANE`,
             butane_mgl = `BUTANE`,
             isobutane_mgl = `ISOBUTANE`,
             pentane_mgl = `PENTANE`,
             hexanes_mgl = `HEXANES`) %>%
      relocate(c(benzene_ugl, toluene_ugl, ethylbenzene_ugl, mp_xylenes_ugl, o_xylene_ugl, tot_xylenes_ugl, methane_mgl),
               .after = btex_above_mcl) %>%
      relocate(c(ethane_mgl, propane_mgl, butane_mgl, isobutane_mgl, pentane_mgl, hexanes_mgl),
               .after = methane_mgl) %>%
      clean_names() %>%
      mutate(x = utm_x83, y = utm_y83,
             # Get sums for each sample
             sum_btex = rowSums(across(benzene_ugl:tot_xylenes_ugl), na.rm = TRUE),
             `sum_alk_2-6` = rowSums(across(ethane_mgl:hexanes_mgl), na.rm = TRUE),
             `sum_alk_3-6` = rowSums(across(propane_mgl:hexanes_mgl), na.rm = TRUE),
             `sum_alk_4-6` = rowSums(across(butane_mgl:hexanes_mgl), na.rm = TRUE),
             # Booleans for whether detected or not
             detected_btex = ifelse(sum_btex > 0, TRUE, FALSE),
             detected_ch4 = ifelse(!is.na(methane_mgl), TRUE, FALSE),
             detected_ethane_26 = ifelse(`sum_alk_2-6` > 0, TRUE, FALSE),
             detected_propane_36 = ifelse(`sum_alk_3-6` > 0, TRUE, FALSE),
             detected_butane_46 = ifelse(`sum_alk_4-6` > 0, TRUE, FALSE),
             btex_above_mcl = ifelse(is.na(btex_above_mcl), FALSE, btex_above_mcl))

# This variable will tell whether analytes were sampled for
sampled_for <- d_water2 %>%
      pivot_wider(id_cols = c(facility_id, facility_type, utm_x83, utm_y83, permit_number, 
                              receipt_number, well_depth, matrix, sample_id, sample_date),
                  names_from = sampled_for,
                  values_from = sampled_for,
                  values_fn = mean) %>%
      mutate(sampled_for_btex = ifelse(is.na(`1`), FALSE, TRUE),
             sampled_for_ch4 = ifelse(is.na(`2`), FALSE, TRUE),
             sampled_for_alkanes = ifelse(is.na(`3`), FALSE, TRUE)) %>%
      select(-c(`1`, `2`, `3`))

# Take the summary table and join the TRUE/FALSE sampled-for dataset
d_water_wd <- d_water_wd %>%
      left_join(sampled_for, by=c("facility_id", "facility_type", "utm_x83", "utm_y83",
                                  "permit_number", "receipt_number", "well_depth", 
                                  "matrix", "sample_id", "sample_date"))

sf_water_wd <- st_as_sf(d_water_wd,
                        coords = c("x", "y"),
                        crs = 26913)

d_wattenberg <- arc.open("Input\\Base_Features.gdb\\Wattenberg_Field_UTM_polygon")
sf_wattenberg <- arc.select(d_wattenberg, where_clause = "description = '2020 revision'")
sf_wattenberg <- arc.data2sf(sf_wattenberg)

# FACIDs that should be exluded for following reasons:
exclude_facids <- data.frame(facility_id = c("700923","703535","705037","705043",
                                             "750100","752255","754338","754563",
                                             "754564","755482","757246"),
                             reason = c("No permit information","No permit information","No permit information","No permit information",
                                        "No permit information","No permit information","Contaminated monitoring well","Contaminated monitoring well",
                                        "Contaminated monitoring well","Contaminated monitoring well","Permit does not exist"))

sf_water_wd2 <- st_join(sf_water_wd, sf_wattenberg, join = st_within) %>%
      filter(description == "2020 revision") %>%
      select(-c(OBJECTID, description)) %>%
      mutate(wattenberg = TRUE) %>%
      select(-geometry) %>%
      filter(!facility_id %in% as.list(exclude_facids$facility_id)) %>%
      mutate(facility_id = as.character(facility_id),
             across(`benzene_ugl`:`sum_alk_4-6`, ~replace_na(., 0))) %>%
      # Convert booleans to character for importing to GIS
      mutate(across(c(sample_date, detected_btex:wattenberg), as.character))

summary_export <- as.data.frame(sf_water_wd2) %>% 
      select(-geometry) %>%
      mutate(sample_date = as.Date(sample_date)) %>%
      left_join(d_isotope, by = c("facility_id", "sample_date")) %>%
      left_join(well_info, by = c("facility_id")) %>%
      mutate(permit_number = case_when((is.na(permit_number.x) & !is.na(permit_number.y)) ~ permit_number.y,
                                       (!is.na(permit_number.x) & is.na(permit_number.y)) ~ permit_number.x,
                                       permit_number.x == permit_number.y ~ permit_number.x),
             receipt_number = case_when((is.na(receipt_number.x) & !is.na(receipt_number.y)) ~ receipt_number.y,
                                        (!is.na(receipt_number.x) & is.na(receipt_number.y)) ~ receipt_number.x,
                                        receipt_number.x == receipt_number.y ~ receipt_number.x)) %>%
      filter(!is.na(permit_number)) %>%
      select(-c(permit_number.x, permit_number.y, receipt_number.x, receipt_number.y, utm_x83.y, utm_y83.y,
                facility_type.y, well_depth.y)) %>%
      rename(utm_x83 = utm_x83.x, utm_y83 = utm_y83.x,
             facility_type = facility_type.x, well_depth = well_depth.x) %>%
      filter(complete.cases(utm_x83, utm_y83))

write.xlsx(summary_export,
           paste0("Output/Final_Exports/Summary_Table_Exported_", Sys.Date(),".xlsx"),
           firstRow=TRUE,
           withFilter=TRUE,
           colWidths="auto",
           overwrite=TRUE)
write_csv(summary_export,
          paste0("Output/Final_Exports/Summary_Table_Exported_", Sys.Date(),".csv"))
