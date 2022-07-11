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

water_wells <- water_wells %>%
   filter(Facility.Type %in% facilities)
water_wells[water_wells==""] <- NA   

s_results <- merge(samples, results, by = "SampleID")
data_complete <- merge(water_wells, s_results, "FacilityID")

#convert units to all be mg/L
gr <- data_complete %>% filter(ParamDescription %in% gas,
                               Units %in% units)

grr <- gr %>%
   mutate(concentration = ifelse(Units == "ug/L" | Units == "UG/L", ResultValue*0.001, ResultValue),
          concentration = ifelse(Units == "%", ResultValue*10, concentration),
          units_clean = ifelse(Units %in% c("per mil VPDB","%","per mil","‰", "per mil VSMOW","Per mil"),
                               "per mil", Units),
          units_clean = ifelse(Units %in% c("mg/L","ug/L","UG/L","mg/l","MG/L","ppm"),
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

#calculate fraction of C1/(C2+C3)
gw_origin <- gw %>%
   mutate(fraction = METHANE/(ETHANE + PROPANE)) %>%
   as.data.frame() %>%
   drop_na(7:11)

#Add well information, then plot in ArcGIS, select DJ Basin, and re-export table
#Then use NEW table for the Bernard Plot
gw_origin2 <- gw_origin %>%
   merge(water_wells, by = "FacilityID") %>%
   drop_na(ReceiptNumber)

#write.csv(gw_origin, paste("Output/methane_gw_origin_", Sys.Date(), ".csv", sep=""))
gw_origin_dj <- read.csv("Input/methane_origin_dj_basin.csv")

# Plot the ratio vs d13CC1 isotope
char_ch4 <- ggplot(gw_origin_dj, aes(x=`DELTA_13C_C1`, y=fraction)) +
   geom_point() +
   coord_cartesian(ylim = c(1, 1e4), xlim = c(-90,-40)) +
   scale_x_continuous(breaks=seq(-90,-40, 10)) +
   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                 labels = trans_format("log10", math_format(10^.x))) +
   labs(x = "Delta 13C C1 (‰)",
        y = "C1 / (C2 + C3)") +
   annotate("rect", xmin=-100, xmax=-60, ymin=100, ymax=1e5, alpha = 0.2) +
   annotate("rect", xmin=-55, xmax=-30, ymin=0.1, ymax=100, alpha = 0.2) +
   annotate("text", x = -77, y = 1e4, label = "Microbial", size = 6) +
   annotate("text", x = -45, y = 1, label = "Thermogenic", size = 6)
char_ch4


#Merge btex to the methane data
det_btex <- read.csv("Output/Water_Wells_Sampled_DetectedBTEX_2020-09-01.csv")

btex_wd <- det_btex %>%
   pivot_wider(id_cols = c("SampleID", "FacilityID", "UtmX83", "UtmY83", "Sample.Date", "Matrix"),
               names_from = ParamDescription,
               values_from = ResultValue,
               values_fn = mean)

btex_methane <- merge(btex_wd, gw_origin, by = "FacilityID", all.x = T)
