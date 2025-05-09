library(tidyverse)
library(ggpubr)

combined_sdd <- bind_rows(nla_2007_formatted %>%
                            select(SITE_ID, YEAR, SECCHI) %>%
                            unique(),
                          nla_2012_ptl_color %>%
                            select(SITE_ID, YEAR, SECCHI) %>%
                            unique(),
                          nla_2017_formatted %>% ungroup() %>%
                            select(SITE_ID, YEAR, "SECCHI" = secchi) %>%
                            unique())

merged_data <- read_csv("../1_aggregate/out/limnosat_redux_raw_rel_reflectance_ptl_color.csv") %>%
  inner_join(x = .,
             y = combined_sdd) %>%
  rowwise() %>%
  mutate(tsi_ptl_temp = (14.42 * log(PTL+0.0001) + 4.15),
         tsi_chla_temp = (9.81 * log(mean_chla+0.0001) + 30.6),
         tsi_sdd_temp = (60 - 14.41*log(SECCHI+0.0001)),
         tsi_ptl = mean(c(tsi_ptl_temp, tsi_chla_temp, tsi_sdd_temp), na.rm = TRUE),
         tsi_ts = case_when(tsi_ptl < 40 ~ "oligo",
                              tsi_ptl >= 40 & tsi_ptl < 50 ~ "meso", 
                              tsi_ptl >= 50 & tsi_ptl < 70 ~ "eutro",
                              tsi_ptl >= 70 ~ "hypereu"),
         ncp_ts = ifelse(PTL <= 30 & COLOR <= 20, "oligo", NA),
         ncp_ts = ifelse(PTL <= 30 & COLOR > 20, "dys", ncp_ts),
         ncp_ts = ifelse(PTL > 30 & COLOR <= 20, "eutro", ncp_ts),
         ncp_ts = ifelse(PTL > 30 & COLOR > 20, "mixo", ncp_ts)) %>%
  pivot_longer(cols = c(ncp_ts, tsi_ts), names_to = "ts", values_to = "ts_group") %>%
  unite(col = "ts_names_type", c(ts, ts_group), sep = "_", remove = FALSE)
      



ggplot(merged_data) +
  geom_violin(aes(x = fct_reorder(ts_names_type, dWL, median) , 
               y = dWL))

ncp_plot <- merged_data %>%
  group_by(ts, ts_names_type, ts_group) %>%
  summarise(mean_dwl = mean(dWL, na.rm = TRUE),
            mean_color = mean(COLOR, na.rm = TRUE),
            sd_dwl = sd(dWL, na.rm = TRUE),
            sd_color = sd(COLOR, na.rm = TRUE),
            n_sample = length(dWL)) %>%
  filter(ts == "ncp_ts") %>%
  ungroup() %>%
  mutate(ts_group = factor(ts_group, 
                           levels = c("oligo", "eutro", "dys", "mixo"),
                           labels = c("Oligotrophic", "Eutrophic", "Dystrophic", "Mixotrophic"))) %>%
  ggplot() +
  geom_point(aes(x = mean_dwl, 
                 y = mean_color,
                 color = ts_group),
             size = 5) +
  geom_errorbar(aes(x = mean_dwl,
                    ymin = mean_color - sd_color,
                    ymax = mean_color + sd_color, 
                    color = ts_group), 
                linewidth = 2, width = 0) +
  geom_errorbarh(aes(y = mean_color,
                      xmin = mean_dwl - sd_dwl,
                      xmax = mean_dwl + sd_dwl,
                     color = ts_group), 
                 linewidth = 2, height = 0) +
  xlab("Mean Dominant Wavelength (nm)") +
  ylab("Mean Color (PCU)") +
  scale_color_manual(values = c("#8da0cb", "#66a61e", 
                                "#a6761d", "#e6ab02"),
                     name = "Nutrient Color\nParadigm Groupings") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))
  

tsi_plot <- merged_data %>%
  group_by(ts, ts_names_type, ts_group) %>%
  summarise(mean_dwl = mean(dWL, na.rm = TRUE),
            mean_color = mean(COLOR, na.rm = TRUE),
            sd_dwl = sd(dWL, na.rm = TRUE),
            sd_color = sd(COLOR, na.rm = TRUE),
            n_sample = length(dWL)) %>%
  filter(ts == "tsi_ts") %>%
  ungroup() %>%
  mutate(ts_group = factor(ts_group, 
                           levels = c("oligo", "meso", "eutro", "hypereu"),
                           labels = c("Oligotrophic", "Mesotrophic", "Eutrophic", "Hypereutrophic"))) %>%
  ggplot() +
  geom_point(aes(x = mean_dwl, 
                 y = mean_color,
                 color = ts_group),
             size = 5) +
  geom_errorbar(aes(x = mean_dwl,
                    ymin = mean_color - sd_color,
                    ymax = mean_color + sd_color, 
                    color = ts_group), 
                linewidth = 2, width = 0) +
  geom_errorbarh(aes(y = mean_color,
                     xmin = mean_dwl - sd_dwl,
                     xmax = mean_dwl + sd_dwl,
                     color = ts_group), 
                 linewidth = 2, height = 0) +
  xlab("Mean Dominant Wavelength (nm)") +
  ylab("Mean Color (PCU)") +
  scale_color_manual(values = c("#8da0cb", "#4891AB", 
                                "#66a61e", "#006d2c"),
                     name = "Trophic State\nIndex Groupings") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


 box_violin <- merged_data %>%
  mutate(ts_names_type = ifelse(ts_names_type == "tsi_ts_oligo", "Oligo<sub>TSI</sub>", ts_names_type),
         ts_names_type = ifelse(ts_names_type == "tsi_ts_eutro", "Eu<sub>TSI</sub>", ts_names_type),
         ts_names_type = ifelse(ts_names_type == "tsi_ts_meso", "Meso<sub>TSI</sub>", ts_names_type),
         ts_names_type = ifelse(ts_names_type == "tsi_ts_hypereu", "Hypereu<sub>TSI</sub>", ts_names_type),
         ts_names_type = ifelse(ts_names_type == "ncp_ts_oligo", "Oligo<sub>NCP</sub>", ts_names_type),
         ts_names_type = ifelse(ts_names_type == "ncp_ts_eutro", "Eu<sub>NCP</sub>", ts_names_type),
         ts_names_type = ifelse(ts_names_type == "ncp_ts_dys", "Dys<sub>NCP</sub>", ts_names_type),
         ts_names_type = ifelse(ts_names_type == "ncp_ts_mixo", "Mixo<sub>NCP</sub>", ts_names_type),
         ts_names_type = factor(ts_names_type,
                                levels = c("Oligo<sub>TSI</sub>", "Dys<sub>NCP</sub>", "Oligo<sub>NCP</sub>",
                                           "Meso<sub>TSI</sub>", "Eu<sub>TSI</sub>", "Eu<sub>NCP</sub>",
                                           "Mixo<sub>NCP</sub>", "Hypereu<sub>TSI</sub>"))) %>% 
   filter(!is.na(ts_names_type)) %>%
ggplot() +
  geom_violin(aes(x = ts_names_type , 
                  y = dWL,
                  fill = ts_names_type),
              alpha = 0.2) +
  geom_boxplot(aes(x = ts_names_type , 
                  y = dWL,
                  fill = ts_names_type),
               width = 0.1, outlier.color = NA) +
  scale_fill_manual( values = c("Oligo<sub>TSI</sub>" = "#8da0cb", 
                           "Dys<sub>NCP</sub>" = "#a6761d", 
                           "Oligo<sub>NCP</sub>" = "#8da0cb",
                           "Meso<sub>TSI</sub>" = "#4891AB", 
                           "Eu<sub>TSI</sub>" = "#349454", 
                           "Eu<sub>NCP</sub>" = "#349454",
                           "Mixo<sub>NCP</sub>" = "#e6ab02", 
                           "Hypereu<sub>TSI</sub>" = "#006d2c")) +
  xlab("Trophic States") +
  ylab("Dominant Wavelength (nm)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16),
        axis.text.y = element_markdown(size = 14),
        axis.text.x = element_markdown(size = 14),
        legend.position = "none")

combined_plots <- ggarrange(plotlist = list(tsi_plot, ncp_plot, box_violin), ncol = 1, labels = "AUTO", font.label = list(size = 20)) 
   
ggsave(plot = combined_plots, filename = "../combined_box_violin.png", height = 15, width = 10, units = "in", bg = "white")
  
  
