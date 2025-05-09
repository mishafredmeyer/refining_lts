### This script is created by Michael F Meyer (mfmeyere@usgs.gov) as part of 
### of the manuscript "Clarifying the trophic state concept to advance freshwater
### science, management, and interdisciplinary collaboration across spatial and 
### temporal scales" This script uses data from the US Environmental Protection
### Agency's National Lake Assessment (NLA) to produce depth profiles for 
### oxygen and temperature across several trophic state classifications. 
### The final product is a 16-panel figure of thermocline and oxycline profiles. 


# Step 1: Load necessary files and data -----------------------------------

library(tidyverse)
library(ggtext)
library(rMR)
library(envFunc)

profiles <- read_csv("../data/nla_all_years/nla_2017_profile-data.csv")



# Step 2: Aggregate data --------------------------------------------------

data = profiles %>%
  filter(SITE_ID %in% c(check_df_area %>%
           filter(stratified == "stratified") %>%
           select(SITE_ID) %>%
           .$SITE_ID)) %>%
  select(SITE_ID, DATE_COL, DEPTH, OXYGEN, PH, TEMPERATURE) %>%
  filter(!is.na(TEMPERATURE)) %>%
  left_join(x = .,
            y = nla_2017_formatted %>% ungroup() %>% 
              rowwise() %>%
              mutate(tsi_ptl_temp = (14.42 * log(PTL+0.0001) + 4.15),
                     tsi_chla_temp = (9.81 * log(CHLA+0.0001) + 30.6),
                     tsi_sdd_temp = (60 - 14.41*log(secchi+0.0001)),
                     tsi_ptl = mean(c(tsi_ptl_temp, tsi_chla_temp, tsi_sdd_temp), na.rm = TRUE),
                     tsi_redo = case_when(tsi_ptl < 40 ~ "oligo",
                                          tsi_ptl >= 40 & tsi_ptl < 50 ~ "meso", 
                                          tsi_ptl >= 50 & tsi_ptl < 70 ~ "eutro",
                                          tsi_ptl >= 70 ~ "hypereu")) %>%
              select(SITE_ID, ts_group, ts, tsi_redo) %>%
              unique() %>%
              mutate(ts_group = ifelse(ts_group != "ncp_ts", "tsi", ts_group),
                     ts = as.character(ts),
                     ts = ifelse(ts_group == "tsi", tsi_redo, ts)) %>%
              unique()) %>%
  filter(!is.na(ts), 
         DEPTH <= 50) %>%
  group_by(SITE_ID) %>%
  mutate(prop_depth = DEPTH / max(DEPTH),
         max_depth = max(DEPTH),
         prop_oxygen = OXYGEN / max(OXYGEN),
         prop_temperature = TEMPERATURE / max(TEMPERATURE),
         date = lubridate::month(lubridate::dmy(DATE_COL))) %>%
  ungroup() %>%
  group_by(ts_group, ts) %>%
  mutate(number = n_distinct(SITE_ID)) %>%
  inner_join(x = .,
             y = data_area %>%
               select(SITE_ID, INDEX_SITE_DEPTH, AREA_HA) %>%
              unique())

sink("skim.txt")
data %>%
  group_by(ts_group, ts) %>%
  skimr::skim()
sink()  # turn off diversion



# Add scaleFactor to ensure that oxygen evenly overlays temperature. 
scaleFactor <-  max(data$OXYGEN, na.rm = TRUE) / max(data$TEMPERATURE, na.rm = TRUE)

unique_ts_groups <- unique(data$ts_group)


# Step 3: Loop over all TS classifications and make plots --------


plot_list <- list()
plot_count <- 1

for(g in 1:length(unique_ts_groups)){
  data_for_plot <- data %>%
    filter(ts_group == unique_ts_groups[[g]]) %>%
    mutate(label = case_when(ts_group == "tsi" ~ "TSI",
                             ts_group == "ncp_ts" ~ "NCP"),
           ts = case_when(ts == "dys" ~ "Dystrophic",
                          ts == "oligo" ~ "Oligotrophic",
                          ts == "eutro" ~ "Eutrophic",
                          ts == "mixo" ~ "Mixotrophic",
                          ts == "meso" ~ "Mesotrophic",
                          ts == "hypereu" ~ "Hypereutrophic"),
           title = paste(label, ": ", ts, " (n = ", number, ")", sep = ""))
  
  unique_ts_label <- unique(data_for_plot$label)
  
  if(unique_ts_groups[[g]] == "ncp_ts"){
    unique_ts <- c("Oligotrophic", "Dystrophic", 
                   "Eutrophic", "Mixotrophic")
  } else {
    unique_ts <- c("Oligotrophic", "Mesotrophic", 
                   "Eutrophic", "Hypereutrophic")
  }
  
  for(t in 1:length(unique_ts)){
    if(unique_ts_label == "NCP"){
      
      title <- data_for_plot %>% ungroup() %>%
        filter(ts == unique_ts[[t]]) %>%
        select(title) %>%
        unique()
      
      plot_list[[plot_count]] <- ggplot(data = data_for_plot %>%
                                          filter(ts == unique_ts[[t]]), 
                                        aes(x = DEPTH)) +
        geom_smooth(aes(y = TEMPERATURE, 
                        color = viridis::mako(20)[c(9)]),
                    method = "loess",  lwd = 2) +
        geom_smooth(aes(y = OXYGEN * scaleFactor, 
                        color = viridis::mako(20)[c(15)]),
                    method = "loess", lwd = 2) +
        ggtitle(paste0(title[[1]])) +
        coord_flip() +
        scale_x_reverse() +
        xlab("Depth (m)") +
        scale_y_continuous(name = "Temperature (\u00B0C)", limits = c(0, 32),
                           sec.axis = sec_axis(~./scaleFactor, 
                                               name = "Oxygen (mg/L)")) +
        scale_color_manual(values = c(viridis::mako(20)[c(9)],
                                      viridis::mako(20)[c(15)]),
                           labels = c("Temperature", "Oxygen")) +
        guides(color = guide_legend(override.aes = list(fill=NA, size = 10, lwd = 10))) + 
        theme_bw() +
        theme(axis.text = element_text(size = 8),
              axis.title = element_text(size = 10),
              plot.title = element_markdown(size = 14),
              legend.text = element_text(size = 14),
              legend.position = "bottom",
              legend.title = element_blank())
        
    } else {
      
      title <- data_for_plot %>% ungroup() %>%
        filter(ts == unique_ts[[t]]) %>%
        select(title) %>%
        unique() 
      
      plot_list[[plot_count]] <- ggplot(data = data_for_plot %>%
                                          filter(ts == unique_ts[[t]]), 
                                        aes(x = DEPTH)) +
        geom_smooth(aes(y = TEMPERATURE, 
                        color = viridis::mako(20)[c(9)]),
                    method = "loess",  lwd = 2) +
        geom_smooth(aes(y = OXYGEN * scaleFactor, 
                        color = viridis::mako(20)[c(15)]),
                    method = "loess", lwd = 2) +
        ggtitle(paste0(title[[1]])) +
        coord_flip() +
        scale_x_reverse() +
        xlab("Depth (m)") +
        scale_y_continuous(name = "Temperature (\u00B0C)", limits = c(0, 32),
                           sec.axis = sec_axis(~./scaleFactor, 
                                               name = "Oxygen (mg/L)")) +
        scale_color_manual(values = c(viridis::mako(20)[c(9)],
                                      viridis::mako(20)[c(15)]),
                           labels = c("Temperature", "Oxygen")) +
        guides(color = guide_legend(override.aes = list(fill=NA, size = 10, lwd = 10))) + 
        theme_bw() +
        theme(axis.text = element_text(size = 8),
              axis.title = element_text(size = 10),
              plot.title = element_markdown(size = 14),
              legend.text = element_text(size = 14),
              legend.position = "bottom",
              legend.title = element_blank())
       
    } 
   
    
    plot_count <- plot_count + 1
  }
}

ggpubr::ggarrange(plotlist = plot_list[c(1, 5,
                                         2, 6,
                                         3, 7,
                                         4, 8)], 
                  align = "hv", nrow = 4, ncol = 2, 
                  common.legend = TRUE, legend = "bottom")

ggsave(filename = "../figures/depth_plots_deepest_lakes.png", 
       width = 6, height = 10, units = "in", bg = "white")

### Aggregate lake profile, depth, and area data

profiles <- read_csv("../data/nla_all_years/nla_2017_profile-data.csv")

max_depth <- read_csv("../data/nla_all_years/nla_2017_secchi-data.csv")

lake_area <- read_csv("../data/nla_all_years/nla_2017_site_information-data.csv")

combined_data <- left_join(x = profiles %>%
                             select(UID, SITE_ID, VISIT_NO, DEPTH, TEMPERATURE, OXYGEN, PH, CONDUCTIVITY),
                           y = max_depth %>%
                             select(UID, SITE_ID, VISIT_NO, INDEX_SITE_DEPTH)) %>%
  left_join(x = .,
            y = lake_area %>%
              select(UID, SITE_ID, VISIT_NO, AREA_HA, LAT_DD83, LON_DD83)) %>%
  mutate(INDEX_SITE_DEPTH = ifelse(DEPTH > INDEX_SITE_DEPTH, DEPTH, INDEX_SITE_DEPTH))

write_csv(x = combined_data, file = "../data/derived_products/combined_profiles.csv")

###### First Quantile

quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble(x = quantile(x, q), q = q)
}

quantiles <- data %>% 
  select(SITE_ID, max_depth) %>%
  unique() %>%
  summarize(quibble(max_depth, q = c(0.25,0.5, 0.75)))

plot_list <- list()
plot_count <- 1

for(g in 1:length(unique_ts_groups)){
  data_for_plot <- data %>%
    left_join(x = ., 
              y = quantiles) %>%
    filter(ts_group == unique_ts_groups[[g]],
           q == 0.75,
           max_depth >= x) %>%
    mutate(label = case_when(ts_group == "tsi" ~ "TSI",
                             ts_group == "ncp_ts" ~ "NCP"),
           number = length(unique(SITE_ID)), 
           ts = case_when(ts == "dys" ~ "Dystrophic",
                          ts == "oligo" ~ "Oligotrophic",
                          ts == "eutro" ~ "Eutrophic",
                          ts == "mixo" ~ "Mixotrophic",
                          ts == "meso" ~ "Mesotrophic",
                          ts == "hypereu" ~ "Hypereutrophic"),
           title = paste(label, ": ", ts, " (n = ", number, ")", sep = ""))
  
  unique_ts_label <- unique(data_for_plot$label)
  
  if(unique_ts_groups[[g]] == "ncp_ts"){
    unique_ts <- c("Oligotrophic", "Dystrophic", 
                   "Eutrophic", "Mixotrophic")
  } else {
    unique_ts <- c("Oligotrophic", "Mesotrophic", 
                   "Eutrophic", "Hypereutrophic")
  }
  
  for(t in 1:length(unique_ts)){
    if(unique_ts_label == "NCP"){
      
      title <- data_for_plot %>% ungroup() %>%
        filter(ts == unique_ts[[t]]) %>%
        select(title) %>%
        unique()
      
      plot_list[[plot_count]] <- ggplot(data = data_for_plot %>%
                                          filter(ts == unique_ts[[t]]), 
                                        aes(x = DEPTH)) +
        geom_smooth(aes(y = TEMPERATURE, 
                        color = viridis::mako(20)[c(9)]),
                    method = "loess",  lwd = 2) +
        geom_smooth(aes(y = OXYGEN * scaleFactor, 
                        color = viridis::mako(20)[c(15)]),
                    method = "loess", lwd = 2) +
        ggtitle(paste0(title[[1]])) +
        coord_flip() +
        scale_x_reverse() +
        xlab("Depth (m)") +
        scale_y_continuous(name = "Temperature (\u00B0C)", limits = c(0, 32),
                           sec.axis = sec_axis(~./scaleFactor, 
                                               name = "Oxygen (mg/L)")) +
        scale_color_manual(values = c(viridis::mako(20)[c(9)],
                                      viridis::mako(20)[c(15)]),
                           labels = c("Temperature", "Oxygen")) +
        guides(color = guide_legend(override.aes = list(fill=NA, size = 10, lwd = 10))) + 
        theme_bw() +
        theme(axis.text = element_text(size = 8),
              axis.title = element_text(size = 10),
              plot.title = element_markdown(size = 14),
              legend.text = element_text(size = 14),
              legend.position = "bottom",
              legend.title = element_blank())
      
    } else {
      
      title <- data_for_plot %>% ungroup() %>%
        filter(ts == unique_ts[[t]]) %>%
        select(title) %>%
        unique() 
      
      plot_list[[plot_count]] <- ggplot(data = data_for_plot %>%
                                          filter(ts == unique_ts[[t]]), 
                                        aes(x = DEPTH)) +
        geom_smooth(aes(y = TEMPERATURE, 
                        color = viridis::mako(20)[c(9)]),
                    method = "loess",  lwd = 2) +
        geom_smooth(aes(y = OXYGEN * scaleFactor, 
                        color = viridis::mako(20)[c(15)]),
                    method = "loess", lwd = 2) +
        ggtitle(paste0(title[[1]])) +
        coord_flip() +
        scale_x_reverse() +
        xlab("Depth (m)") +
        scale_y_continuous(name = "Temperature (\u00B0C)", limits = c(0, 32),
                           sec.axis = sec_axis(~./scaleFactor, 
                                               name = "Oxygen (mg/L)")) +
        scale_color_manual(values = c(viridis::mako(20)[c(9)],
                                      viridis::mako(20)[c(15)]),
                           labels = c("Temperature", "Oxygen")) +
        guides(color = guide_legend(override.aes = list(fill=NA, size = 10, lwd = 10))) + 
        theme_bw() +
        theme(axis.text = element_text(size = 8),
              axis.title = element_text(size = 10),
              plot.title = element_markdown(size = 14),
              legend.text = element_text(size = 14),
              legend.position = "bottom",
              legend.title = element_blank())
      
    } 
    
    
    plot_count <- plot_count + 1
  }
}
