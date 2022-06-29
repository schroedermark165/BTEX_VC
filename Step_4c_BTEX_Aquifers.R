# PURPOSE -----------------------------------------------------------------
# Load COGCC water well sampling data. It is provided as MS Access database
# of locations, samples, and sample results.
# 
# Merge the locations to the sample IDs and their respective results. This
# can be plotted in ARCMAP.
# 
# PROJECT INFORMATION:
#     Name:Denver-Julesburg Basin Water-Well BTEX Study 
#     Description: BTEX concentrations by aquifer and depth
# 
# HISTORY:
# 02-16-2021      Created script to organize plots from exported GIS data
# 


# Load packages -----------------------------------------------------------
library(tidyverse)
library(janitor)
library(openxlsx)
library(scales)
library(gridExtra)
library(trend)

# Load Data ---------------------------------------------------------------
rm(list=ls())
# graphics.off()

btex <- read.xlsx("Output/Final_Exports/BTEX_Cases_Well_Data.xlsx")

sel_cols <- c("benzene_ugl", "toluene_ugl", "ethylbenzene_ugl", "mp_xylenes_ugl",
              "o_xylene_ugl", "tot_xylenes_ugl", "methane_mgl")

btex_l <- btex %>%
      pivot_longer(cols = all_of(sel_cols),
                   names_to = "analyte",
                   values_to = "result") %>%
      mutate(analyte = case_when(analyte == "benzene_ugl" ~ "benzene",
                                 analyte == "toluene_ugl" ~ "toluene",
                                 analyte == "ethylbenzene_ugl" ~ "ethylbenzene",
                                 analyte == "mp_xylenes_ugl" ~ "mp-xylenes",
                                 analyte == "o_xylene_ugl" ~ "o-xylene",
                                 analyte == "tot_xylenes_ugl" ~ "total xylenes",
                                 analyte == "methane_mgl" ~ "methane"),
             units = case_when(analyte == "methane" ~ "mg/L",
                               TRUE ~ "ug/L"),
             earliest_detection = as.Date(earliest_detection, origin="1899-12-30"))

btex_l_det <- btex_l %>%
      filter(analyte != "methane", !is.na(aquifer)) %>%
      mutate(analyte2 = case_when(analyte %in% c("mp-xylenes", "o-xylene", "total xylenes") ~ "xylenes",
                                  TRUE ~ as.character(analyte)))

aquifer_plot <- ggplot(btex_l_det, aes(x = result,
                                       y = factor(aquifer,
                                                  levels = c("Laramie-Fox Hills",
                                                             "Lower Arapahoe",
                                                             "Upper Arapahoe",
                                                             "Surficial"),
                                                  labels = c("laramie-fox hills",
                                                             "lower arapahoe",
                                                             "upper arapahoe",
                                                             "surficial"),
                                                  ordered = FALSE), 
                                       color = factor(analyte2,
                                                      levels = c("benzene",
                                                                 "toluene",
                                                                 "ethylbenzene",
                                                                 "xylenes")))) +
      # geom_violin() +
      geom_jitter(height = 0.2) +
      scale_x_continuous(limits = c(0, 250)) +
      
      scale_fill_manual(values = c("#33a02c", "#4A148C", "#1f78b4", "#a6cee3")) +
      # scale_color_brewer(palette = "Set1") +
      labs(x = "BTEX concentration (µg/l)",
           y = "aquifer",
           color = "analyte") +
      theme_bw() +
      theme(legend.position = c(.82, .75))
aquifer_plot

ggsave(filename = paste0("Output/Final_Exports/figures/Figure_X_Aquifer_Detections.svg"),
       width = 5, height = 3, units = "in",
       dpi = 300)
