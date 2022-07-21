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
library(ggpubr)

# Load Data ---------------------------------------------------------------
rm(list=ls())
# graphics.off()

btex <- openxlsx::read.xlsx("Output/Final_Exports/BTEX_Cases_Well_Data.xlsx") %>%
      # manually add depth to Facility ID:752587; permit  #: 234828
      # Source of depth: https://dwr.state.co.us/Tools/WellPermits/0479283
      mutate(well_depth = ifelse(facility_id == 752587, 780, well_depth))

aquifers <- data.frame(
      aquifer = c("Surficial", "Upper Arapahoe", 
                  "Lower Arapahoe", "Laramie-Fox Hills"),
      upper_depth = c(0, 150, 200, 400),
      lower_depth = c(75, 250, 500, 1200)
)

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
      filter(analyte != "methane",
             !is.na(aquifer)) %>%
      mutate(analyte2 = case_when(analyte %in% c("mp-xylenes", "o-xylene", "total xylenes") ~ "xylenes",
                                  TRUE ~ as.character(analyte)),
             analyte2 = factor(analyte2,
                               levels = c("benzene",
                                          "toluene",
                                          "ethylbenzene",
                                          "xylenes"),
                               labels = c("benzene",
                                          "toluene",
                                          "ethylbenzene",
                                          "total xylenes")))


# Make Plots --------------------------------------------------------------

# Plot concentrations by aquifer
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
                                       color = analyte2)) +
      # geom_violin() +
      geom_jitter(height = 0.2) +
      scale_x_continuous(limits = c(0, 250)) +
      
      scale_fill_manual(values = c("#33a02c", "#4A148C", "#1f78b4", "#a6cee3")) +
      # scale_color_brewer(palette = "Set1") +
      labs(x = "BTEX concentration (µg/l)",
           y = "aquifer",
           color = "analyte") +
      theme_bw() +
      theme(legend.position = c(.82, .75),
            legend.box.background = element_rect(size=.5,
                                                 linetype= "solid",
                                                 color = "black"))
aquifer_plot

ggsave(filename = paste0("Output/Figures/Figure_X_Aquifer_Detections.svg"),
       width = 5, height = 3, units = "in",
       dpi = 300)

mcls <- data.frame(
      analyte2 = c("benzene", "toluene", "ethylbenzene", "total xylenes"),
      mcl = c(5, 560, 700, 1400)
)

# Plot concentrations by depth
depth_plot <- ggplot() +
      geom_vline(data = mcls,
                 aes(xintercept = mcl, color = analyte2)) +
      geom_point(data = btex_l_det,
                 aes(x = result,
                     y = well_depth,
                     color = analyte2)) +
      geom_hline(yintercept = 0) +
      scale_fill_manual(values = c("#33a02c", "#4A148C", "#1f78b4", "#a6cee3")) +
      scale_y_reverse(limits = c(1200, 0), 
                      breaks = seq(0, 1200, 200),
                      expand = c(NA, 0)) +
      scale_x_continuous(limits = c(0, 1500)) +
      # scale_color_brewer(palette = "Set1") +
      labs(x = bquote("BTEX concentration " (µg~L^-1)),
           y = "depth of well screen (ft)",
           color = "analyte") +
      theme_bw() +
      theme(legend.position = c(.84, .70),
            legend.box.background = element_rect(size=.5,
                                                 linetype= "solid",
                                                 color = "black"))
depth_plot

ggsave(depth_plot,
       filename = paste0("Output/Figures/Figure_5_Conc_vs_Depth.png"),
       width = 5, height = 3, units = "in",
       dpi = 300)

