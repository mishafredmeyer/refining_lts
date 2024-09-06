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

profiles <- read_csv("../data/nla_all_years/nla_2017_profile-data.csv")



# Step 2: Aggregate data --------------------------------------------------

data = profiles %>%
  select(SITE_ID, DATE_COL, DEPTH, OXYGEN, PH, TEMPERATURE) %>%
  filter(!is.na(TEMPERATURE)) %>%
  left_join(x = .,
            y = nla_2017_formatted %>% ungroup() %>% ## Note: nla_2017_formatted made in national map script
              select(SITE_ID, ts_group, ts) %>%
              unique()) %>%
  filter(!is.na(ts), 
         DEPTH <= 50) %>%
  group_by(SITE_ID) %>%
  mutate(prop_depth = DEPTH / max(DEPTH),
         date = lubridate::month(lubridate::dmy(DATE_COL))) 

# Add scaleFactor to ensure that oxygen evenly overlays temperature. 
scaleFactor <-  max(data$OXYGEN, na.rm = TRUE) / max(data$TEMPERATURE, na.rm = TRUE)

unique_ts_groups <- unique(data$ts_group)


# Step 3: Loop over all TS classifications and make plots --------


plot_list <- list()
plot_count <- 1

for(g in 1:length(unique_ts_groups)){
  data_for_plot <- data %>%
    filter(ts_group == unique_ts_groups[[g]]) %>%
    mutate(label = case_when(ts_group == "chla_ts" ~ "TSI<sub>CHLa</sub>",
                             ts_group == "ncp_ts" ~ "NCP",
                             ts_group == "ptl_ts" ~ "TSI<sub>TP</sub>",
                             ts_group == "sdd_ts" ~ "TSI<sub>SDD</sub>"),
           ts = case_when(ts == "dys" ~ "Dystrophic",
                          ts == "oligo" ~ "Oligotrophic",
                          ts == "eutro" ~ "Eutrophic",
                          ts == "mixo" ~ "Mixotrophic",
                          ts == "meso" ~ "Mesotrophic",
                          ts == "hypereu" ~ "Hypereutrophic"))
  
  unique_ts_label <- unique(data_for_plot$label)
  
  if(unique_ts_groups[[g]] == "ncp_ts"){
    unique_ts <- c("Oligotrophic", "Dystrophic", 
                   "Eutrophic", "Mixotrophic")
  } else {
    unique_ts <- c("Oligotrophic", "Mesotrophic", 
                   "Eutrophic", "Hypereutrophic")
  }
  
  for(t in 1:length(unique_ts)){
    if(plot_count %in% c(1, 5, 9, 13)){
      plot_list[[plot_count]] <- ggplot(data = data_for_plot %>%
                                          filter(ts == unique_ts[[t]]), 
                                        aes(x = DEPTH)) +
        geom_smooth(aes(y = TEMPERATURE, 
                        color = viridis::mako(20)[c(9)]),
                    method = "loess",  lwd = 2) +
        geom_smooth(aes(y = OXYGEN * scaleFactor, 
                        color = viridis::mako(20)[c(15)]),
                    method = "loess", lwd = 2) +
        ggtitle(paste0(unique_ts_label, ": ", unique_ts[[t]], sep = " ")) +
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
        
    } else if(plot_count %in% c(2)){
      plot_list[[plot_count]] <- ggplot(data = data_for_plot %>%
                                          filter(ts == unique_ts[[t]]), 
                                        aes(x = DEPTH)) +
        geom_smooth(aes(y = TEMPERATURE, 
                        color = viridis::mako(20)[c(9)]),
                    method = "loess",  lwd = 2) +
        geom_smooth(aes(y = OXYGEN * scaleFactor, 
                        color = viridis::mako(20)[c(15)]),
                    method = "loess", lwd = 2) +
        ggtitle(paste0(unique_ts_label, ": ", unique_ts[[t]], sep = " ")) +
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
       
    } else if(plot_count %in% c(6, 10, 14)){
      plot_list[[plot_count]] <- ggplot(data = data_for_plot %>%
                                          filter(ts == unique_ts[[t]]), 
                                        aes(x = DEPTH)) +
        geom_smooth(aes(y = TEMPERATURE, 
                        color = viridis::mako(20)[c(9)]),
                    method = "loess",  lwd = 2) +
        geom_smooth(aes(y = OXYGEN * scaleFactor, 
                        color = viridis::mako(20)[c(15)]),
                    method = "loess", lwd = 2) +
        ggtitle(paste0(unique_ts_label, ": ", unique_ts[[t]], sep = " ")) +
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
       
    } else if(plot_count %in% c(3, 7, 11, 15)){
      plot_list[[plot_count]] <- ggplot(data = data_for_plot %>%
                                          filter(ts == unique_ts[[t]]), 
                                        aes(x = DEPTH)) +
        geom_smooth(aes(y = TEMPERATURE, 
                        color = viridis::mako(20)[c(9)]),
                    method = "loess",  lwd = 2) +
        geom_smooth(aes(y = OXYGEN * scaleFactor, 
                        color = viridis::mako(20)[c(15)]),
                    method = "loess", lwd = 2) +
        ggtitle(paste0(unique_ts_label, ": ", unique_ts[[t]], sep = " ")) +
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
       
    } else if(plot_count %in% c(4)){
      plot_list[[plot_count]] <- ggplot(data = data_for_plot %>%
                                          filter(ts == unique_ts[[t]]), 
                                        aes(x = DEPTH)) +
        geom_smooth(aes(y = TEMPERATURE, 
                        color = viridis::mako(20)[c(9)]),
                    method = "loess",  lwd = 2) +
        geom_smooth(aes(y = OXYGEN * scaleFactor, 
                        color = viridis::mako(20)[c(15)]),
                    method = "loess", lwd = 2) +
        ggtitle(paste0(unique_ts_label, ": ", unique_ts[[t]], sep = " ")) +
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
      plot_list[[plot_count]] <- ggplot(data = data_for_plot %>%
                                          filter(ts == unique_ts[[t]]), 
                                        aes(x = DEPTH)) +
        geom_smooth(aes(y = TEMPERATURE, 
                        color = viridis::mako(20)[c(9)]),
                    method = "loess",  lwd = 2) +
        geom_smooth(aes(y = OXYGEN * scaleFactor, 
                        color = viridis::mako(20)[c(15)]),
                    method = "loess", lwd = 2) +
        ggtitle(paste0(unique_ts_label, ": ", unique_ts[[t]], sep = " ")) +
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

ggpubr::ggarrange(plotlist = plot_list[c(1, 5, 9, 13,
                                          2, 6, 10, 14,
                                          3, 7, 11, 15,
                                          4, 8, 12, 16)], 
                  align = "hv", nrow = 4, ncol = 4, 
                  common.legend = TRUE, legend = "bottom")

ggsave(filename = "../figures/depth_plots.png", 
       width = 12, height = 10, units = "in", bg = "white")
  