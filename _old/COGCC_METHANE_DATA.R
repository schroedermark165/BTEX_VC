#================================================================================
#     COGCC_METHANE_DATA.R
#================================================================================
# PURPOSE:
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
# 10-17-2020      MS created script
# 
#================================================================================
# LOAD PACKAGES

library(tidyverse)
library(readxl)
library(lubridate)
library(ggsci)
library(scales)

#================================================================================
# LOAD DATA
rm(list = ls())

water_wells <- read.csv("Input/DL_Locations.csv")
samples <- read.csv("Input/DL_Samples.csv")
results <- read.csv("Input/DL_Results.csv") 

#================================================================================
# QUERY CHEMISTRY DATA
facilities <- c("Domestic Well",
                "Ground Water",
                "Groundwater",
                "Stock or  Irrigation",
                "Irrigation",
                "Municipal",
                "Commercial")

gas <- c("METHANE",
         "ETHANE",
         "PROPANE",
         "DELTA 13C C1")

units <- c("mg/L",
           "ug/L",
           "per mil VPDB",
           "%", 
           "UG/L",
           "mg/l",
           "MG/L",
           "per mil",
           "‰",
           "ppm",
           "per mil VSMOW",
           "Per mil",
           "MOL %",
           "Mol %")

nd <- c("U", "nd", "<", "u", "ND")

water_wells <- filter(water_wells, Facility.Type %in% facilities)
s_results <- merge(samples, results, by = "SampleID")
data_complete <- merge(water_wells, s_results, "FacilityID")

#convert units to all be mg/L
gr <- data_complete %>% filter(ParamDescription %in% gas,
                               Units %in% units)

grr <- gr %>%
   mutate(concentration = ifelse(Units == "ug/L" | Units == "UG/L", ResultValue*0.001, ResultValue)) %>%
   mutate(concentration = ifelse(Units == "%", ResultValue*10, concentration)) %>%
   mutate(units_clean = ifelse(Units %in% c("per mil VPDB","%","per mil","‰", "per mil VSMOW","Per mil"),
                               "per mil", Units)) %>%
   mutate(units_clean = ifelse(Units %in% c("mg/L","ug/L","UG/L","mg/l","MG/L","ppm"),
                               "mg/L", units_clean))


#pivot data to wide format with methane isotopes as columns, by sampleID
gw <- grr %>%
   filter(Matrix == "GAS") %>%
   distinct(SampleID, FacilityID, ResultValue, .keep_all = T) %>%
   group_by(SampleID) %>%
   pivot_wider(id_cols = c("SampleID", "FacilityID", "UtmX83", "UtmY83", "Sample.Date", "Matrix"),
               names_from = ParamDescription,
               values_from = concentration,
               values_fn = mean)

# gww <- gw[complete.cases(gw[7:10]), ]

gw_origin <- gw %>%
   mutate(fraction = METHANE/(ETHANE + PROPANE)) %>%
   as.data.frame() %>%
   drop_na(7:11)

# Plot the ratio vs d13CC1 isotope
char_ch4 <- ggplot(gw_origin, aes(x=`DELTA 13C C1`, y=fraction)) +
   geom_point() +
   scale_x_continuous(limits=c(-90, -40), breaks=seq(-90,-40, 10)) +
   scale_y_log10(limits=c(1, 1e5),
                 breaks = trans_breaks("log10", function(x) 10^x),
                 labels = trans_format("log10", math_format(10^.x))) +
   labs(x = "Delta 13C C1 (‰)",
        y = "C1 / (C2 + C3)")
char_ch4

det_btex <- read.csv("Output/Water_Wells_Sampled_DetectedBTEX_2020-09-01.csv")

btex_wd <- det_btex %>%
   pivot_wider(id_cols = c("SampleID"))