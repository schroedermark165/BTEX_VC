# Purpose -----------------------------------------------------------------
# Working script for analysis and looking into details
# 
# PROJECT INFORMATION:
#     Name:Denver-Julesburg Basin Water-Well BTEX Study 
#     Description: Analysis of water quality impacts from oil and gas development.

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
rm(list=ls())
dat_summary <- read.xlsx("Output/Final_Exports/BTEX_Cases_2021-11-05.xlsx") %>%
   mutate(sample_date = as.Date(sample_date, origin = "1899-12-30"))

# Data analysis and handling ----------------------------------------------
# Summarize detections
summary <- dat_summary %>%
   # must have isotope data and be tested for btex/ch4/alkanes
   filter(!is.na(origin),
          sampled_for_btex == TRUE,
          sampled_for_ch4 == TRUE,
          sampled_for_alkanes == TRUE) %>%
   group_by(facility_id) %>%
   summarize(n=n(),
             detected_btex = any(detected_btex),
             detected_ch4 = any(detected_ch4),
             detected_alk = any(detected_alk)) %>%
   mutate(chisq_btex = ifelse(detected_btex == TRUE, "BTEX Detected", "BTEX Not Detected"),
          chisq_ch4 = ifelse(detected_ch4 == TRUE, "Methane Detected", "Methane Not Detected"),
          chisq_alk = ifelse(detected_alk == TRUE, "Alkanes (Ethane-Hexane) Detected", "Alkanes (Ethane-Hexane) Not Detected"))
   
chisq.test(table(summary$chisq_btex, summary$chisq_ch4))["observed"]
chisq.test(table(summary$chisq_btex, summary$chisq_alk))["observed"]


btex_summary <- dat_summary %>%
   filter(detected_btex == TRUE) %>%
   group_by(facility_id) %>%
   summarize(facility_type = toString(unique(facility_type)),
             matrix = toString(unique(matrix)),
             sample_id = toString(unique(sample_id)),
             tot_btex_occurrences = toString(unique(sort(sum_btex, decreasing = TRUE))),
             tot_methane_occurrences = toString(unique(sort(methane_mgl, decreasing = TRUE))),
             tot_alkane16_occurrences = toString(unique(sort(`sum_alk_1-6`, decreasing = TRUE))),
             tot_alkane26_occurrences = toString(unique(sort(`sum_alk_2-6`, decreasing = TRUE))),
             estimated_origin = toString(unique(origin)))
