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
library(mblm)
library(arcgisbinding)
library(sf)
library(trend)
arc.check_product()

# FIGURE X. Bernard Plot --------------------------------------------------
rm(list=ls())

f_btex <- arc.open("D:/Google Drive/ArcGIS/DJ_Basin_BTEX_Study/Data/1_Geodatabase/Water_Wells.gdb/Water_Wells_Sampled_BTEX_methane_20210302")
f_gas <- arc.open("D:/Google Drive/ArcGIS/DJ_Basin_BTEX_Study/Data/1_Geodatabase/Water_Wells.gdb/Water_Wells_Sampled_Gas_Data_20210213")
d_btex <- arc.select(f_btex)
d_gas <- arc.select(f_gas)

# Get list of unique BTEX occurrences by FACID and SampleDate
btex_facid <- d_btex %>%
   mutate(SampleDate = as.Date(SampleDate, format = "%Y-%m-%d")) %>%
   distinct(FacilityID, SampleDate) %>%
   mutate(key = paste(FacilityID, SampleDate))

# Clean gas samples from Wattenberg area
gas_wattenberg <- d_gas %>%
      filter(complete.cases(METHANE, ETHANE, PROPANE),
             Wattenberg == "Wattenberg") %>%
      mutate(SampleDate = as.Date(SampleDate, format = "%Y-%m-%d"),
             key = paste(FacilityID, SampleDate),
             fraction = METHANE/(ETHANE + PROPANE),
             btex_det = case_when(key %in% as.list(btex_facid$key) ~ "Methane and BTEX",
                                  TRUE ~ "No BTEX"),
             ch4_origin = case_when((DELTA_13C_C1 <= -55.0 & fraction >= 100) ~ "Biogenic origin",
                                    (DELTA_13C_C1 > -55.0 & fraction < 100) ~ "Thermogenic origin",
                                    (DELTA_13C_C1 <= -55.0 & fraction < 100) ~ "Mixed origin")) %>%
      select(-c(`OBJECTID`)) %>%
      arrange(desc(btex_det))

# Plot the ratio vs d13CC1 isotope
btex_only <- gas_wattenberg %>% filter(btex_det == "Methane and BTEX")

char_ch4_plot <- ggplot(gas_wattenberg, aes(x = `DELTA_13C_C1`, y = fraction, fill = btex_det)) +
      annotate("rect", xmin = -100, xmax = -55, ymin = 100, ymax = 1e5, alpha = 0.2) +
      annotate("rect", xmin = -55, xmax = -30, ymin = 0.1, ymax = 100, alpha = 0.2) +
      geom_point(shape = 21, size = 3) +
      #geom_point(data = gas_btex, shape = 21, size = 3, fill = "darkorange2", aes(x = DELTA.13C.C1, y = fraction)) +
      scale_fill_manual(values = c("darkorange2", "cadetblue2")) +
      coord_cartesian(ylim = c(1, 1e4), xlim = c(-90,-40)) +
      scale_x_continuous(breaks = seq(-90,-40, 10)) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      labs(x = "Delta 13C C1 (‰)",
           y = "C1/(C2 + C3)",
           fill = "BTEX Co-Occurrence") +
           #title = "Bernard Plot",
           #subtitle = "Methane origin as function of D13C and molar ratios") +
      annotate("text", x = -77, y = 1e4, label = "Biogenic", size = 6) +
      annotate("text", x = -46, y = 1, label = "Thermogenic", size = 6) + 
      theme(legend.position=c(0,0), legend.justification=c(0,0))
char_ch4_plot

# xdensity <- ggplot(btex_only, aes(x = `DELTA_13C_C1`, fill = btex_det)) +
#       geom_density(alpha = 0.5) +
#       scale_fill_manual(values = c("darkorange2", "cadetblue2")) +
#       labs(x = "", y = "Density") +
#       coord_cartesian(xlim = c(-90,-40)) +
#       theme(legend.position = "none")
# ydensity <- ggplot(btex_only, aes(y = fraction, fill=btex_det)) + 
#       geom_density(alpha=.5) + 
#       scale_fill_manual(values = c("darkorange2", "cadetblue2")) +
#       scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                     labels = trans_format("log10", math_format(10^.x))) + 
#       labs(x = "Density", y = "") +
#       coord_cartesian(ylim = c(1, 1e4)) +
#       theme(legend.position = "none")
# 
# blankPlot <- ggplot()+geom_blank(aes(1,1))+
#       theme(plot.background = element_blank(), 
#             panel.grid.major = element_blank(),
#             panel.grid.minor = element_blank(), 
#             panel.border = element_blank(),
#             panel.background = element_blank(),
#             axis.title.x = element_blank(),
#             axis.title.y = element_blank(),
#             axis.text.x = element_blank(), 
#             axis.text.y = element_blank(),
#             axis.ticks = element_blank())
# 
# grid.arrange(xdensity, blankPlot, char_ch4_plot, ydensity, 
#              ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

# Save figure to png
ggsave("Output/Figures/Fig_X_Bernard.png", char_ch4_plot)


# Test2 -------------------------------------------------------------------

# FIGURE X. Bernard Plot --------------------------------------------------
rm(list=ls())

dat_all <- read.xlsx("Output/Final_Exports/BTEX_Cases_2021-11-17.xlsx")

dat_plt <- dat_all %>%
   filter(detected_ch4 == TRUE,
          sampled_for_alkanes == TRUE,
          sampled_for_btex == TRUE,
          sampled_for_ch4 == TRUE,
          complete.cases(methane, ethane, propane, delta_13c_c1))

# Plot the ratio vs d13CC1 isotope
bernard_plot <- ggplot(dat_plt, aes(x = `delta_13c_c1`, y = fraction)) +
   
   # Set up annotation rectangles for "origin" zones
   annotate("rect", xmin = -100, xmax = -55, ymin = 100, ymax = 1e5, alpha = 0.2) +
   annotate("rect", xmin = -55, xmax = -30, ymin = 0.1, ymax = 100, alpha = 0.2) +
   
   # Plot sample points
   geom_point(size = 3, aes(fill = detected_propane_36, shape = detected_btex)) +
   scale_fill_manual(values = c("cadetblue2", "darkorange2")) +
   scale_shape_manual(values = c(21, 24)) +
   
   # Set axes and labels
   coord_cartesian(ylim = c(1, 1e4), xlim = c(-90,-40)) +
   scale_x_continuous(breaks = seq(-90,-40, 10)) +
   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                 labels = trans_format("log10", math_format(10^.x))) +
   labs(x = "Delta 13C C1 (‰)",
        y = "C1/(C2 + C3)",
        fill = "Co-Occurrence of Propane-Hexane",
        shape = "Co-Occurrence of BTEX",
        title = "Bernard Plot",
        subtitle = "Methane origin as function of D13C and molar ratios") +
   annotate("text", x = -77, y = 1e4, label = "Biogenic", size = 6) +
   annotate("text", x = -46, y = 1, label = "Thermogenic", size = 6) + 
   theme(legend.position=c(0,0), legend.justification=c(0,0)) +
   guides(fill=guide_legend(override.aes=list(shape=21)))

bernard_plot
