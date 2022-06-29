# PURPOSE -----------------------------------------------------------------
# Load COGCC water well sampling data. It is provided as MS Access database
# of locations, samples, and sample results.
# 
# Merge the locations to the sample IDs and their respective results. This
# can be plotted in ARCMAP.
# 
# PROJECT INFORMATION:
#     Name:Denver-Julesburg Basin Water-Well BTEX Study 
#     Description: Cumulative Occurrences of BTEX by Year.
#                  Show dates of detection and normalize to # oil and gas wells
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
library(arcgisbinding)
library(sf)
library(here)
arc.check_product()

# Load Data ---------------------------------------------------------------

rm(list=ls())
# graphics.off()

# Blank duoplot template
blankPlot <- ggplot()+geom_blank(aes(1,1))+
      theme(plot.background = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(),
            axis.ticks = element_blank())

# Read final cases
d_cases <- read.xlsx(here("Output", "Final_Exports","BTEX_Cases_Exported_2022-06-03.xlsx"),
                     sheet = "BTEX Cases (Mean)",
                     detectDates = TRUE)

# Read all sample summary (BTEX+Methane)
d_summary <- read_csv("Output/Final_Exports/Summary_Table_Exported_2022-06-29.csv") %>%
      filter(wattenberg == TRUE) %>%
      mutate(facility_id = as.character(facility_id))

# Read timeline of COGCC Rules
d_rules <- read.xlsx("./../Manuscript/Manuscript_Tables.xlsx")
d_rules <- data.frame(date = as.Date(c("12-01-2005", "04-01-2009", "09-01-2011", "01-01-2012", "05-01-2013"), format="%m-%d-%Y"),
                      label = c("318A.e", "305.d", "318A.e(4)", "COGA", "609/318A.e/f"))

# Summarize isotopic gas data
d_gas <- d_summary %>% filter(!is.na(origin))

# Summarize Isotopic Methane Results for each facid
d_gas2 <- d_gas %>%
      group_by(facility_id, facility_type, utm_x83, utm_y83, permit_number, receipt_number, origin) %>%
      summarize(n_samples = n(),
                mean_fraction = mean(fraction),
                mean_d13 = mean(delta_13c_c1),
                min_d13 = min(delta_13c_c1),
                max_d13 = max(delta_13c_c1),
                sample_date = min(sample_date)) %>%
      ungroup()

# Filter isotopic methane results for thermogenic
# Get earliest thermogenic methane occurrence in each facid
d_thermogenic <- d_gas2 %>% 
      filter(origin %in% c("thermogenic", "mixed")) %>%
      group_by(facility_id) %>%
      summarize(min_date = min(sample_date),
                origin = origin) %>%
      ungroup() %>%
      arrange(min_date) %>%
      mutate(id = 1:n())

# Plot thermogenic cumulative occurrences
cumulative_ch4 <- ggplot(data = d_thermogenic, aes(x = min_date, y = id, fill = factor(origin,
                                                                                       levels = c("thermogenic",
                                                                                                  "mixed")))) +
      geom_point(shape = 21, size = 3) +
      scale_x_date(breaks = "1 year",
                   date_labels = "%Y") +
      scale_fill_manual(values = c("#33a02c", "#a6cee3")) + 
      labs(
            title = "cumulative occurrences of thermogenic methane in water wells",
            subtitle = paste0("between ", min(d_thermogenic$min_date)," and ", max(d_thermogenic$min_date)),
            fill = "methane origin",
            x = "sample date",
            y = paste0("number of thermogenic methane detections (n = ", max(d_thermogenic$id), ")")
            ) +
      theme_bw() +
      theme(legend.position=c(0.01,0.99), legend.justification=c(0,1))
cumulative_ch4

# Summarize BTEX Results
# Get earliest detection of BTEX in each facid
d_btex_gas <- d_summary %>%
      filter(detected_btex == TRUE) %>%
      mutate(origin2 = factor(origin,
                              levels = c("thermogenic",
                                         "mixed",
                                         "microbial", 
                                         NA),
                              labels = c("thermogenic origin",
                                         "mixed origin",
                                         "microbial origin",
                                         "no co-occurring oethane"),
                              ordered = TRUE,
                              exclude = TRUE)) %>%
      group_by(facility_id) %>%
      summarize(earliest_detection = min(sample_date),
                origin2 = min(origin2)) %>%
      ungroup() %>%
      arrange(earliest_detection) %>%
      mutate(id = 1:n())

# Plot BTEX cumulative occurrences
cumulative_btex_unique_id <- ggplot(data = d_btex_gas, aes(x = earliest_detection, y = id,
                                                           fill = origin2)) +
      geom_point(shape = 21, size = 3) +
      scale_fill_manual(values = c("#33a02c", "#4A148C", "#1f78b4", "#a6cee3")) +
      scale_x_date(limits = as.Date(c("2011-01-01", "2020-01-01")),
                   breaks = "1 year",
                   date_labels = "%Y") +
      labs(title = "cumulative occurrences of BTEX in water wells",
           subtitle = "between 2001 and 2020",
           fill = "co-occurring methane",
           x = "sample date",
           y = "cumulative BTEX detection") +
      theme_bw() +
      theme(legend.position = c(0.01, 0.986), legend.justification = c(0,1))
cumulative_btex_unique_id


# Normalized bar plot
# time_period <- "halfyear"
time_period <- "quarter"

period_cases <- d_summary %>%
      filter(detected_btex == TRUE) %>%
      group_by(period = floor_date(sample_date, time_period)) %>%
      summarize(btex_occurrences = n())

period_samples <- d_summary %>%
      group_by(period = floor_date(sample_date, time_period)) %>%
      summarize(samples = n()) %>%
      ungroup() %>%
      left_join(period_cases, by = "period") %>%
      mutate(btex_occurrences = replace_na(btex_occurrences, 0),
             normalized_cases = btex_occurrences/samples) %>%
      filter(period >= as.Date("2000-01-01"))

d_btex_all <- d_summary %>%
      filter(detected_btex == TRUE) %>%
      mutate(origin2 = factor(origin,
                              levels = c("thermogenic",
                                         "mixed",
                                         "microbial", 
                                         NA),
                              labels = c("thermogenic origin",
                                         "mixed origin",
                                         "microbial origin",
                                         "no co-occurring methane"),
                              ordered = TRUE,
                              exclude = TRUE)) %>%
      arrange(sample_date) %>%
      mutate(id = 1:n())

# Plot BTEX cumulative occurrences
cumulative_btex_all <- ggplot() +
      geom_point(data = d_btex_all, aes(x = sample_date, 
                                        y = id,
                                        shape = factor(btex_above_mcl,
                                                      levels = c(TRUE, FALSE),
                                                      labels = c("exceeds Colorado MCL", "does not exceed Colorado MCL"))
                                        ),
                 size = 3) +
      scale_shape_manual(values = c(16, 1), name="") +
      scale_x_date(limits = as.Date(c("2001-01-01", "2020-01-01")),
                   breaks = "1 year",
                   date_labels = "%Y") +
      # scale_y_continuous(sec.axis = sec_axis(trans = ~./100, name = "BTEX Detections per Sample")) +
      labs(title = "cumulative occurrences of BTEX in water wells",
           subtitle = paste0("between ", format(min(d_btex_all$sample_date), "%Y"), " and 2020"),
           x = "sample date",
           y = "cumulative BTEX detection") +
      theme_bw() +
      theme(legend.position = c(0.01, 0.986), legend.justification = c(0,1), legend.title=element_blank())
cumulative_btex_all

# Plot # of samples tested for BTEX
# Add it to the cumulative plot
samples <- ggplot(d_summary %>% filter(sampled_for_btex == TRUE), 
                  aes(x = sample_date)) +
      geom_histogram(binwidth = 365/4,
                     boundary = 365/4,
                     # center = 0
                     ) +
      geom_vline(xintercept = c(d_rules$date)) +
      geom_text(data = d_rules, aes(x = date, y = 150, label = label), 
                color = "black", size = 2.5, fontface = "bold", angle = 90, nudge_x = -66) +
      scale_x_date(limits = as.Date(c("2001-01-01", "2020-01-01")),
                   breaks = "1 year",
                   minor_breaks = "6 months",
                   date_labels = "%Y") +
      scale_y_continuous(limits = c(0, 200), expand = c(0, NA)) +
      labs(x = "sample date",
           y = "number of samples") +
      theme_bw()

duo_plot <- cowplot::plot_grid(cumulative_btex_all, samples, ncol=1, nrow=2, align = "v")
duo_plot

ggsave("./../Manuscript/Figures/Figure_2_Cumulative_Occurrences.png",
       height = 8,
       width = 12,
       units = "in",
       dpi = 300)
