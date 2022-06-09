# PURPOSE -----------------------------------------------------------------
# Load COGCC water well sampling data. It is provided as MS Access database
# of locations, samples, and sample results.
# 
# Merge the locations to the sample IDs and their respective results. This
# can be plotted in ARCMAP.
# 
# PROJECT INFORMATION:
#     Name:Denver-Julesburg Basin Water-Well BTEX Study 
#     Description: Piper Diagram for Basic Water Chemistry
# 
# HISTORY:
# 02-16-2021      Created script to organize plots from exported GIS data
# 10-10-2021      Saved separate script for organization


# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)
library(openxlsx)
library(ggsci)
library(scales)
library(gridExtra)
library(mblm)
library(arcgisbinding)
library(sf)
library(trend)
arc.check_product()


# Load Data ---------------------------------------------------------------

#  ---------------------------------
rm(list = ls())

water_wells <- read_csv("Input/Step_1_Original_Data/DL_Locations.csv",
                        col_types = cols(PermitNumber = col_character()))
samples <- read.csv("Input/Step_1_Original_Data/DL_Samples.csv")
results <- read.csv("Input/Step_1_Original_Data/DL_Results.csv") 
GIS_btex <- read.xlsx("Input/Step_2_From_GIS/Water_Wells_Sampled_BTEX_Gas_GIS_20210213.xlsx", sheet = "btex", detectDates = T, na.strings = "<Null>")
d_aq <- read.xlsx("Input/BTEX_Wells_DWR_Info.xlsx")
GIS_gas <- read.xlsx("Input/Step_2_From_GIS/Water_Wells_Sampled_BTEX_Gas_GIS_20210213.xlsx", sheet = "gas", detectDates = T, na.strings = "<Null>")

facilities <- c("Domestic Well",
                "Ground Water",
                "Groundwater",
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

# Clean data
water_wells$PermitNumber <- as.character(water_wells$PermitNumber)
water_wells$ReceiptNumber <- as.character(water_wells$ReceiptNumber)
samples$Sample.Date <- as.Date(samples$Sample.Date, "%m/%d/%Y %H:%M:%S")

btex_cols <- colnames(GIS_btex[13:19])
GIS_btex[btex_cols] <- sapply(GIS_btex[btex_cols], as.numeric)
GIS_btex <- GIS_btex %>% filter(!is.na(PermitNumber))
gas_cols <- colnames(GIS_gas[13:15])
GIS_gas[gas_cols] <- sapply(GIS_gas[gas_cols], as.numeric)
rm(btex_cols, gas_cols)

dd_aq <- select(d_aq, c(FacilityID, WellDepth, Aquifer))

# Clean gas samples from Wattenberg area
gas_btex <- GIS_gas %>%
      filter(complete.cases(METHANE, ETHANE, PROPANE),
             Wattenberg == "Wattenberg") %>%
      mutate(key = paste(FacilityID, SampleDate),
             fraction = METHANE/(ETHANE + PROPANE),
             ch4_origin = case_when((DELTA.13C.C1 <= -55.0 & fraction >= 100) ~ "Biogenic origin",
                                    (DELTA.13C.C1 > -55.0 & fraction < 100) ~ "Thermogenic origin",
                                    (DELTA.13C.C1 <= -55.0 & fraction < 100) ~ "Mixed origin")) %>%
      select(-c(`ObjectID.*`, `Shape.*`, Thermogenic))

gas_w_btex <- gas_btex %>% filter(!is.na(ch4_origin))   
# Dissolved BTEX data; keep locations with DWR permits; rename columns
water_btex <- GIS_btex %>%
      filter(!is.na(PermitNumber)) %>%
      left_join(gas_btex, by = c("FacilityID", "SampleDate")) %>%
      select(-c(`ObjectID.*`, `Shape.*`, FacilityType.y, UtmX83.y, UtmY83.y, PermitNumber.y, ReceiptNumber.y,
                WellDepth.y, Matrix.y, SampleID.y)) %>%
      rename(Benzene_ugL = `Benzene_ug#l`,
             Toluene_ugL = `Toluene_ug#l`,
             Ethylbenzene_ugL = `Ethylbenzene_ug#l`,
             mp_Xylenes_ugL = `mp_Xylenes_ug#l`, o_Xylene = `o_Xylene_ug#l`, Tot_Xylenes = `Tot_Xylenes_ug#l`,
             Methane_ugL = `Methane_mg#l`)

# Get list of unique BTEX occurrences by FACID and SampleDate
btex_facid <- as.list(unique(GIS_btex$FacilityID))
sample_ids <- water_btex[, c("FacilityID", "ch4_origin")]

# MAJOR IONS
# Create df for basic chemistry to merge to results
# Convert results to meq/L for plotting on Piper
ions <- data.frame(major_ion = c("CALCIUM", "CHLORIDE", "MAGNESIUM", "POTASSIUM",
                                 "SODIUM", "SULFATE", "BICARBONATE ALKALINITY as CACO3",
                                 "CARBONATE ALKALINITY AS CACO3"),
                   charge = c(2, -1, 2, 1, 1, -2, -1, -2),
                   mw = c(40.08, 35.45, 24.31, 39.10, 22.98, 96.06, 100.09, 100.09))

# Filter for sampled wells that are desired facilities
wells_sel <- water_wells %>% filter(Facility.Type %in% facilities)
wells_sel$County <- str_trim(wells_sel$County)

# Merge ALL water wells with the sample information
# This df to be used for BTEX results
samples_water <- wells_sel %>%
      left_join(samples, by = c("FacilityID")) %>%
      filter(Matrix %in% c("WATER", "Water", "GW", "LIQUID", ""),
             County == "WELD")

# Left join water results to selected locations and their samples
results_water <- samples_water %>%
      left_join(results, by = c("SampleID"))

results_water_ions <- results_water %>%
      filter(ParamDescription %in% c(as.list(ions$major_ion)),
             !Units %in% c("mg/L as CaCO3", "mg/Kg"),
             FacilityID %in% btex_facid) %>%
      left_join(ions, by = c("ParamDescription" = "major_ion")) %>%
      mutate(Results_mgL = case_when(Units %in% c("ug/L", "ug/l") ~ ResultValue/1000,
                                     TRUE ~ ResultValue),
             Results_meqL = abs((Results_mgL * charge)/mw))

# Widen table for use in Piper diagram
results_water_ions_wide <- results_water_ions %>%
      pivot_wider(id_cols = c(FacilityID, Facility.Type, UtmX83, UtmY83, County, PermitNumber, ReceiptNumber, SampleID, Sample.Date),
                  names_from = ParamDescription,
                  values_from = Results_meqL,
                  values_fn = mean) %>%
      drop_na(c(PermitNumber, CALCIUM, CHLORIDE, POTASSIUM, SODIUM, SULFATE, MAGNESIUM,
                `BICARBONATE ALKALINITY as CACO3`, `CARBONATE ALKALINITY AS CACO3`)) %>%
      rename("ca_meq" = "CALCIUM",
             "cl_meq" = "CHLORIDE",
             "k_meq" = "POTASSIUM",
             "na_meq" = "SODIUM",
             "so4_meq" = "SULFATE",
             "mg_meq" = "MAGNESIUM",
             "hco3_meq" = "BICARBONATE ALKALINITY as CACO3",
             "co3_meq" = "CARBONATE ALKALINITY AS CACO3") %>%
      left_join(sample_ids, by = c("FacilityID"))

results_water_ions_wide$ch4_origin[is.na(results_water_ions_wide$ch4_origin)] <- "black"  

with(results_water_ions_wide,
     piperPlot(xCat = ca_meq,
               yCat = mg_meq,
               zCat = na_meq + k_meq,
               xAn = cl_meq,
               yAn = co3_meq + hco3_meq,
               zAn = so4_meq,
               Plot = list(name = ch4_origin,
                           what = "points", type = "solid", width = "standard", symbol = "circle",
                           filled = TRUE, size = 0.05, color = setColor(ch4_origin)),
               xAn.title = "Chloride"))
graphics.off()
