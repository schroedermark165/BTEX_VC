#================================================================================
#     Summary_Table.R
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
# 06-06-2020      MS created script
# 
#================================================================================
# LOAD PACKAGES

library(tidyr)
library(dplyr)
library(readxl)
library(lubridate)

#================================================================================
# LOAD DATA
setwd("~/R_Scripts/BTEX_Study")
rm(list = ls())

btex <- read.csv("Input/Water_wells_BTEX_GIS_Export_20200915.csv")

btex1 <- btex %>% select(-c("ï..OBJECTID", "Latitude83", "Longitude8", "Field1", "QuarterQua", "Meridian", "MethodCode", "FractionTy"))

btex_wide <- btex1 %>% pivot_wider(names_from = ParamDescr, values_from = ResultValu)

test <- btex_wide %>% group_by(FacilityID)

btex_summary <- btex_wide %>% group_by(FacilityID) %>%
      summarise(Benzene = sum(BENZENE>0),
                Toluene = sum(TOLUENE>0),
                Ethylbenzene = sum(ETHYLBENZENE>0),
                mp_Xylenes = sum(`m-+p-XYLENE`>0),
                o_Xylene = sum(`o-XYLENE`>0),
                Total_Xylenes = sum(`TOTAL XYLENES`>0))
                