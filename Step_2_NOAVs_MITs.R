#================================================================================
#     Step_2_NOAVs_MITs.R
#================================================================================
# PURPOSE:
# Clean up NOAV and MIT data and merge to location  data of COGCC OG wells
# 
# PROJECT INFORMATION:
#     Name:Denver-Julesburg Basin Water-Well BTEX Study 
#     Description: Analysis of water quality impacts from oil and gas development.
# 
# HISTORY:
# 06-17-2020      MS created script
# 
#================================================================================
# LOAD PACKAGES
library(tidyverse)
library(openxlsx)
library(stringr)

rm(list=ls())

#================================================================================
# Oil gas location data
og <- read.csv("Input/Step_1_Original_Data/COGCC_OG_Wells.csv")
og_coords <- og %>% select("API_Label", "Latitude", "Longitude", "Utm_X", "Utm_Y")

#================================================================================
# MIT data
mit <- read.xlsx("Input/COGCC_MIT_20200524/MIT.xlsx")

mit <- mit %>% filter(State == "CO")
mit_api <- mit[]
mit_api["API_Label"] <- NA
mit_api <- drop_na(mit_api, `API.State.Code`, `API.County.Code`, `API.Sequence.Number`)
mit_api["API State Code"] <- "05"
mit_api$API_Label <- paste(mit_api$`API State Code`, mit_api$`API County Code`, mit_api$`API Sequence Number`, sep = "-")
mit_coord <- merge(mit_api, og_coords, by = "API_Label")
mit_coord[c("Utm_X", "Utm_Y")] <- lapply(mit_coord[c("Utm_X", "Utm_Y")], as.numeric)
          
#================================================================================
# NOAV data
noav_cogcc_export <- read.xlsx("Input/COGCC_NOAV_20200524/NOAV.xlsx")
noav_search <- read.xlsx("Input/COGCC_NOAV_20200524/NoavIncidentSearch_322021.xlsx")

keywords <- c("groundwater", "ground water", "contamination", "BTEX", "methane", "surface casing", 
              "leak", "leakage", "aquifer", "benzene", "toluene", "ethylbenzene", "xylene", "xylenes",
              "water", "odor", "bubble", "release", "migration", "stray gas", "stray gas migration",
              "remediation") 

# Clean up the COGCC official NOAV export
# This is downloaded straight off the website but only include 2010-2020
noav_api <- noav_cogcc_export
noav_api <- drop_na(noav_api, APIStateCode, APICountyCode, APISequenceNumber)
noav_api["APIStateCode"] <- "05"
noav_api$API_Label <- paste(noav_api$APIStateCode, noav_api$APICountyCode, noav_api$APISequenceNumber, sep = "-")
noav_coord <- merge(noav_api, og_coords, by = "API_Label")
noav_coord$Utm_X <- as.numeric(noav_coord$Utm_X)
noav_coord$Utm_Y <- as.numeric(noav_coord$Utm_Y)

noav_rel <- noav_coord %>%
      filter(grepl(paste(keywords, collapse="|"),
                   paste(RuleDesription,
                         AllegedViolationDescription,
                         CorrectiveActionDescription,
                         EnforcementAction,
                         FinalResolutionComments),
                   ignore.case = TRUE))

# Clean up the searched NOAV query
# This is downloaded from NOAV document query tool.
# From 1/1/2001 to 1/1/2020
noav_api <- read.xlsx("Input/COGCC_NOAV_20200524/NoavIncidentSearch_322021.xlsx", sheet = "noavs", na.strings = "")
noav_api$violation_date <- as.Date(noav_api$violation_date, format = "%m/%d/%Y")     #11/1/2019 12:00:00 AM	

noav_api <- drop_na(noav_api, APINumber)
noav_api$StateCode <- "05"
noav_api$CountyCode <- str_sub(noav_api$APINumber, 1, 3)
noav_api$SeqNumber <- str_sub(noav_api$APINumber, -5, -1)
noav_api$API_Label <- paste(noav_api$StateCode, noav_api$CountyCode, noav_api$SeqNumber, sep = "-")

noav_coord <- merge(noav_api, og_coords, by = "API_Label")
noav_coord$Utm_X <- as.numeric(noav_coord$Utm_X)
noav_coord$Utm_Y <- as.numeric(noav_coord$Utm_Y)

noav_rel <- noav_coord %>% filter(grepl(paste(keywords, collapse="|"), AllegedViolation, ignore.case = TRUE))

# WRITE for ARCMAP
write.xlsx(noav_key, paste("Output/NOAV_relevant_geocoded_", Sys.Date(), ".xlsx", sep=""),
           sheetName = "Post-2010 Relevant NOAVs")
write.xlsx(noav_coord, paste("Output/NOAV_all_geocoded_", Sys.Date(), ".xlsx", sep=""),
           sheetName = "Post-2010 NOAVs")
