# PURPOSE -----------------------------------------------------------------
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
# 02-16-2021      Created script to organize plots from exported GIS data
# 10-10-2021      Saved as separate plot


# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)
library(openxlsx)
library(ggsci)
library(scales)
library(gridExtra)

# FIGURE X. Bernard Plot --------------------------------------------------
rm(list=ls())

# Dataset with all BTEX and Methane results
d_btex <- read.csv("Output/Final_Exports/Summary_Table_Exported_2022-06-29.csv")

# Gas sample dataset
d_gas <- readRDS("Intermediate/Water_Wells_Sampled_Methane_Isotope.RDS")

# Get list of unique BTEX occurrences by FACID and SampleDate
btex_facid <- d_btex %>%
      distinct(facility_id, sample_date) %>%
      mutate(key = paste(facility_id, sample_date))

# Clean gas samples from Wattenberg area
gas_wattenberg <- d_btex %>%
      filter(
            wattenberg == TRUE, 
            detected_ch4 == TRUE,
      ) %>%
      mutate(key = paste(facility_id, sample_date),
             fraction = methane/(ethane + propane),
             ch4_origin = case_when((delta_13c_c1 <= -55.0 & fraction >= 100) ~ "Biogenic origin",
                                    (delta_13c_c1 > -55.0 & fraction < 100) ~ "Thermogenic origin",
                                    (delta_13c_c1 <= -55.0 & fraction < 100) ~ "Mixed origin")) %>%
      select(sample_id, well_depth, sample_date, methane_mgl, ethane_mgl, propane_mgl,
             detected_btex, detected_ch4, wattenberg, delta_13c_c1, fraction, ch4_origin, origin) %>%
      arrange(detected_btex)

char_ch4_plot <- ggplot(gas_wattenberg, 
                        aes(x = `delta_13c_c1`, 
                            y = fraction, 
                            fill = factor(detected_btex,
                                          levels = c(TRUE, FALSE),
                                          labels = c("BTEX co-occurrence", "no BTEX co-occurrence")
                                          )
                            )
                        ) +
      # Add plot annotation
      annotate("rect", xmin = -100, xmax = -55, ymin = 100, ymax = 1e5, alpha = 0.2) +
      annotate("rect", xmin = -55, xmax = -30, ymin = 0.1, ymax = 100, alpha = 0.2) +
      # Add data to plot
      geom_point(shape = 21, size = 3) +
      # Format the points by color
      scale_fill_manual(values = c("darkorange2", "cadetblue2")) +
      # Manually set axes
      coord_cartesian(ylim = c(1, 1e4), xlim = c(-90,-40)) +
      scale_x_continuous(breaks = seq(-90,-40, 10)) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      # Set labels
      labs(x = "delta 13c c1 (‰)",
           y = "c1/(c2 + c3)") +
      annotate("text", x = -80, y = 6e3, label = "biogenic", size = 8) +
      annotate("text", x = -46, y = 1.5, label = "thermogenic", size = 8) + 
      theme_bw() +
      theme(legend.position=c(0.05,0.05), 
            legend.justification=c(0,0), 
            legend.title = element_blank(),
            legend.text = element_text(size=18))
char_ch4_plot

# Save figure to png
ggsave("./../Manuscript/Figures/Figure_4_Bernard.png", 
       char_ch4_plot,
       width = 12, height = 8,
       units = "in",
       dpi = 300)

