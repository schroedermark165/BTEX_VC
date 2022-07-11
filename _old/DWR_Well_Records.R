#================================================================================
#     DWR_Well_Records.R
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
# 11-10-2020      MS created script
# 
#================================================================================
# LOAD PACKAGES

library(tidyverse)
library(readxl)

#================================================================================
# select DWR wells

rm(list = ls())

status <- c("Well Abandoned",
            "Permit Canceled",
            "Permit Expired",
            "Well Constructed",
            "Permit Issued",
            "Well Replaced",
            "Permit Extended")

dwr <- read.csv("Input/DWR_Well_Application_Permit_2020-11-10.csv")

dwr2 <- dwr %>%
      drop_na(c("UTM.x", "UTM.y")) %>%
      filter(Current.Status %in% status)

write.csv(dwr2, paste("Output/DWR_Well_Application_Permit_select_", Sys.Date(), ".csv", sep = ""))

#================================================================================
# compare btex wells to make sure they have permits/receipt from DWR
rm(list=ls())

btex1 <- read_excel("Input/compare_to_dwr/BTEX_find_well_depths.xlsx")
btex <- read.csv("Input/compare_to_dwr/btex.csv")
dwr1k <- read.csv("Input/compare_to_dwr/dwr.csv")

btex_s <- btex %>% distinct(ReceiptNumber, PermitNumber)

match_receipt <- merge(btex1, dwr1k[ , c("Receipt", "Permit", "More_Information")], by.x = "ReceiptNumber", by.y = "Receipt", all.x = T)
match_permit <- merge(btex1, dwr1k[ , c("Receipt", "Permit", "More_Information")], by.x = "PermitNumber", by.y = "Permit", all.x = T)
