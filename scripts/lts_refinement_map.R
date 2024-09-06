### This script is created by Michael F Meyer (mfmeyere@usgs.gov) as part of 
### of the manuscript "Clarifying the trophic state concept to advance freshwater
### science, management, and interdisciplinary collaboration across spatial and 
### temporal scales" This script uses data from the US Environmental Protection
### Agency's National Lake Assessment (NLA) to produce maps and statistical
### aggregations of Omernik Level III ecoregion data for different trophic states. 
### The final product is a two panel figure, where the top panel contains various
### US national maps and the lower panel contains stacked barcharts of percentages. 


## Load necessary packages

library(tidyverse)
library(sf)

### Load the data

nla_2007_tptnchla <- read_csv("../data/nla_all_years//NLA2007_WaterQuality_20091123.csv")

nla_2007_sdd <- read_csv("../data/nla_all_years/nla2007_secchi_20091008.csv")

nla_2007_zoop <- read_csv("../data/nla_all_years/nla2007_zooplankton_count_20091022.csv")

nla_2012_tptn <- read_csv("../data/nla_all_years/nla2012_waterchem_wide.csv")

nla_2012_chla <- read_csv("../data/nla_all_years/nla2012_chla_wide.csv")

nla_2012_sdd <- read_csv("../data/nla_all_years/nla2012_secchi_08232016.csv")

nla_2012_zoop <- read_csv("../data/nla_all_years/nla-2012-zooplankton-count-data-updated-12092021.csv")

nla_2012_metadata <- read_csv("../data/nla_all_years/nla2012_wide_siteinfo_08232016.csv")

nla_2017_tntp <- read_csv("../data/nla_all_years/nla_2017_water_chemistry_chla-data.csv")

nla_2017_metadata <- read_csv("../data/nla_all_years/nla_2017_site_information-data.csv")

nla_2017_sdd <- read_csv("../data/nla_all_years/nla_2017_secchi-data.csv")

nla_2017_zoop <- read_csv("../data/nla_all_years/nla-2017-zooplankton-count-data.csv")

epa_ecoregions <- st_read("../data/Aggr_Ecoregions_2015.shp") %>%
  st_transform(crs = 'NAD83')
  
### Clean 2007 Data

nla_2007_formatted <- nla_2007_tptnchla %>%
  select(SITE_ID, YEAR, VISIT_NO, PTL, NTL, CHLA, COLOR) %>%
  tibble() %>%
  group_by(SITE_ID, YEAR) %>%
  summarize(across(.cols = c(PTL, NTL, CHLA, COLOR), 
                   .fns = ~ mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  inner_join(x = .,
             y = nla_2007_sdd %>%
               select(SITE_ID, YEAR, VISIT_NO, SECMEAN) %>%
               tibble() %>%
               group_by(SITE_ID, YEAR) %>%
               summarize(across(.cols = c(SECMEAN), 
                                .fns = ~ mean(.x, na.rm = TRUE))) %>%
               ungroup())

### NLA 2012 Cleaning

nla_2012_ptl_color <- nla_2012_tptn %>%
  select(UID, contains(c("color", "ptl", "ntl"))) %>%
  select(UID, 
         "COLOR" = COLOR_RESULT, 
         "PTL" = PTL_RESULT,
         "NTL" = NTL_RESULT) %>%
  mutate(YEAR = 2012) %>%
  tibble() %>%
  inner_join(., nla_2012_metadata) %>%
  select(COLOR, NTL, PTL, YEAR, SITE_ID, -UID) %>%
  group_by(YEAR, SITE_ID) %>%
  summarize(across(.cols = c(COLOR, PTL, NTL), 
                   .fns = ~ mean(.x, na.rm = TRUE))) %>%
  inner_join(x = .,
             y = nla_2012_sdd %>%
               select(SITE_ID, YEAR, SECCHI) %>%
               group_by(YEAR, SITE_ID) %>%
               summarize(across(.cols = c(SECCHI), 
                                .fns = ~ mean(.x, na.rm = TRUE)))) %>%
  inner_join(x = .,
             y = nla_2012_chla %>%
               mutate(YEAR = 2012) %>%
               select(UID, YEAR, CHLX_RESULT) %>%
               rename("CHLA" = CHLX_RESULT) %>%
               tibble() %>%
               inner_join(., nla_2012_metadata) %>%
               select(CHLA, YEAR, SITE_ID, -UID) %>%
               group_by(YEAR, SITE_ID) %>%
               summarize(across(.cols = c(CHLA), 
                                .fns = ~ mean(.x, na.rm = TRUE)))) %>%
  inner_join(x = nla_2012_zoop %>%
               mutate(YEAR = 2012) %>%
               select(SITE_ID, YEAR, BIOMASS, ORDER) %>%
               group_by(SITE_ID, YEAR, ORDER) %>%
               summarize(across(.cols = c(BIOMASS), 
                                .fns = ~ mean(.x, na.rm = TRUE))) %>%
               pivot_wider(names_from = ORDER, values_from = BIOMASS),
             y = .)


### Clean 2017 data

nla_2017_formatted <- nla_2017_tntp %>%
  filter(ANALYTE %in% c("COLOR", "PTL", "NTL", "CHLA")) %>%
  mutate(RESULT = as.numeric(RESULT),
         YEAR = 2017) %>%
  select(UID, YEAR, ANALYTE, RESULT) %>%
  pivot_wider(names_from = "ANALYTE", values_from = "RESULT") %>%
  tibble() %>%
  inner_join(., nla_2017_metadata) %>%
  select(COLOR, PTL, YEAR, SITE_ID, NTL, CHLA, -UID, 
         LAT_DD83, LON_DD83, ELEVATION) %>%
  group_by(YEAR, SITE_ID, LAT_DD83, LON_DD83, ELEVATION) %>%
  summarize(across(.cols = c("COLOR", "NTL", "PTL", "CHLA"), 
                   .fns = ~ mean(.x, na.rm = TRUE))) %>%
  inner_join(x = .,
             y = nla_2017_sdd %>%
               mutate(YEAR = 2017,
                      secchi = (DISAPPEARS + REAPPEARS)/2) %>%
               group_by(SITE_ID, YEAR) %>%
               summarize(across(.cols = c(secchi),
                                .fns = ~ mean(.x, na.rm = TRUE)))) %>%
  inner_join(x = nla_2017_zoop %>%
               mutate(YEAR = 2017) %>%
               filter(PHYLUM == "ROTIFERA") %>%
               select(SITE_ID, YEAR, COUNT, BIOMASS) %>%
               group_by(SITE_ID, YEAR) %>%
               summarize(across(.cols = c(COUNT, BIOMASS), 
                                .fns = ~ sum(.x, na.rm = TRUE))) %>%
               ungroup() %>%
               mutate(tsi_rot = (5.38 * log(COUNT)) + 19.28) %>%
               select(-COUNT, -BIOMASS),
             y = .) %>%
  inner_join(x = nla_2017_zoop %>%
               mutate(YEAR = 2017) %>%
               filter(CLASS %in% c("MAXILLOPODA", "BRANCHIOPODA",
                                   "OSTRACODA", "MALACOSTRACA")) %>%
               select(SITE_ID, YEAR, COUNT, BIOMASS) %>%
               group_by(SITE_ID, YEAR) %>%
               summarize(across(.cols = c(COUNT, BIOMASS), 
                                .fns = ~ sum(.x, na.rm = TRUE))) %>%
               ungroup() %>%
               mutate(tsi_crust = (25.5 * (COUNT^0.142))) %>%
               select(-COUNT, -BIOMASS),
             y = .) %>%
  mutate(ncp_ts = ifelse(PTL <= 30 & COLOR <= 20, "oligo", NA),
         ncp_ts = ifelse(PTL <= 30 & COLOR > 20, "dys", ncp_ts),
         ncp_ts = ifelse(PTL > 30 & COLOR <= 20, "eutro", ncp_ts),
         ncp_ts = ifelse(PTL > 30 & COLOR > 20, "mixo", ncp_ts),
         chla_ts = ifelse(CHLA <= 2.6, "oligo", NA),
         chla_ts = ifelse(CHLA > 2.6 & CHLA <= 7.3, "meso", chla_ts),
         chla_ts = ifelse(CHLA > 7.3 & CHLA <= 56, "eutro", chla_ts),
         chla_ts = ifelse(CHLA > 56, "hypereu", chla_ts),
         ptl_ts = ifelse(PTL <= 12, "oligo", NA),
         ptl_ts = ifelse(PTL > 12 & PTL <= 24, "meso", ptl_ts),
         ptl_ts = ifelse(PTL > 24 & PTL <= 96, "eutro", ptl_ts),
         ptl_ts = ifelse(PTL > 96, "hypereu", ptl_ts),
         sdd_ts = ifelse(secchi >= 4, "oligo", NA),
         sdd_ts = ifelse(secchi >= 2 & secchi < 4, "meso", sdd_ts),
         sdd_ts = ifelse(secchi >= 0.5 & secchi < 2, "eutro", sdd_ts),
         sdd_ts = ifelse(secchi < 0.5, "hypereu", sdd_ts),
         rot_ts = ifelse(tsi_rot < 45, "oligo-meso", NA),
         rot_ts = ifelse(tsi_rot >= 45 & tsi_rot < 55, "meso-eutro", rot_ts),
         rot_ts = ifelse(tsi_rot >= 55 & tsi_rot < 65, "eutro", rot_ts),
         rot_ts = ifelse(tsi_rot > 65, "hypereu", rot_ts),
         crs_ts = ifelse(tsi_crust < 45, "oligo-meso", NA),
         crs_ts = ifelse(tsi_crust >= 45 & tsi_crust < 55, "meso-eutro", crs_ts),
         crs_ts = ifelse(tsi_crust >= 55 & tsi_crust < 65, "eutro", crs_ts),
         crs_ts = ifelse(tsi_crust > 65, "hypereu", crs_ts),
         ncp_ts = factor(ncp_ts, 
                         levels = c("dys", "oligo",
                                    "eutro", "mixo")),
         chla_ts = factor(chla_ts, 
                         levels = c("oligo", "meso", "eutro", "hypereu")),
         ptl_ts = factor(ptl_ts, 
                          levels = c("oligo", "meso", "eutro", "hypereu")),
         sdd_ts = factor(sdd_ts, 
                         levels = c("oligo", "meso", "eutro", "hypereu")),
         rot_ts = factor(rot_ts, 
                          levels = c("oligo-meso", "meso-eutro", 
                                     "eutro", "hypereu")),
         crs_ts = factor(crs_ts, 
                         levels = c("oligo-meso", "meso-eutro", 
                                    "eutro", "hypereu"))) %>%
  pivot_longer(cols = c(ncp_ts, chla_ts, ptl_ts, sdd_ts, rot_ts, crs_ts), names_to = "ts_group", values_to = "ts")

## Bring in USA map
usa <- sf::st_as_sf(map_data("usa"), 
                      coords = c("long", "lat"),
                      crs = 'WGS84') %>% 
  group_by(group) %>% 
  summarize(do_union=FALSE) %>%
  sf::st_cast("POLYGON") %>% 
  ungroup() 

## Generate map of US with different TS of lakes portrated by color of point. 
## Facets in the maps are different TS classification schemes.

map <- ggplot(data = nla_2017_formatted %>%
                drop_na() %>%
                mutate(ts_group = case_when(ts_group == "chla_ts" ~ "TSI<sub>CHLa</sub>",
                                            ts_group == "crs_ts" ~ "TSI<sub>CR1</sub>",
                                            ts_group == "ncp_ts" ~ "NCP",
                                            ts_group == "ptl_ts" ~ "TSI<sub>TP</sub>",
                                            ts_group == "rot_ts" ~ "TSI<sub>ROT</sub>",
                                            ts_group == "sdd_ts" ~ "TSI<sub>SDD</sub>"),
                       ts_group = factor(ts_group, 
                                         levels = c("TSI<sub>CHLa</sub>", "TSI<sub>TP</sub>",
                                                    "TSI<sub>SDD</sub>", "TSI<sub>ROT</sub>",
                                                    "TSI<sub>CR1</sub>", "NCP")),
                       ts = factor(ts, 
                                   levels = c("oligo", "oligo-meso", "meso", 
                                              "meso-eutro", "eutro", "hypereu",
                                              "dys", "mixo"),
                                   labels = c("Oligo", "Oligo-Meso", "Meso", 
                                              "Meso-Eutro", "Eutro", "Hypereutro",
                                              "Dys", "Mixo"))) %>%
                st_as_sf(coords = c("LON_DD83",
                                    "LAT_DD83"),
                         crs = 'WGS84')) + 
  geom_sf(data = usa, lwd = 0.5, color = "black", fill = "grey95") +
  geom_sf(aes(color = ts))+
  scale_color_manual(values = c("#8da0cb", "#4891AB","#02818a", 
                                "#349454", "#66a61e","#006d2c", 
                                "#a6761d", "#e6ab02")) + 
  xlab('') + ylab('') +
  coord_sf(crs = 'epsg:2163') +
  facet_wrap(~ts_group) +
  guides(color = guide_legend(override.aes = list(size = 6),
                              nrow = 3)) +
  theme_minimal() +
  theme(strip.text = element_markdown(size = 18),
        legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 18))

## National Barchat for each trophic state classification scheme. 
## This figure is NOT used in the manuscript. 
ggplot(data = nla_2017_formatted %>% select(SITE_ID, ts_group, ts, LON_DD83, LAT_DD83) %>% unique() %>%
                drop_na() %>%
                mutate(ts_group = case_when(ts_group == "chla_ts" ~ "TSI<sub>CHLa</sub>",
                                            ts_group == "crs_ts" ~ "TSI<sub>CR1</sub>",
                                            ts_group == "ncp_ts" ~ "NCP",
                                            ts_group == "ptl_ts" ~ "TSI<sub>TP</sub>",
                                            ts_group == "rot_ts" ~ "TSI<sub>ROT</sub>",
                                            ts_group == "sdd_ts" ~ "TSI<sub>SDD</sub>"),
                       ts_group = factor(ts_group, 
                                         levels = c("TSI<sub>CHLa</sub>", "TSI<sub>TP</sub>",
                                                    "TSI<sub>SDD</sub>", "TSI<sub>ROT</sub>",
                                                    "TSI<sub>CR1</sub>", "NCP")),
                       ts = factor(ts, 
                                   levels = c("oligo", "oligo-meso", "meso", 
                                              "meso-eutro", "eutro", "hypereu",
                                              "dys", "mixo"),
                                   labels = c("Oligo", "Oligo-Meso", "Meso", 
                                              "Meso-Eutro", "Eutro", "Hypereutro",
                                              "Dys", "Mixo"))) %>%
         st_as_sf(coords = c("LON_DD83",
                             "LAT_DD83"),
                  crs = st_crs(5071)) %>%
         group_by(ts_group, ts) %>%
         count() %>%
         ungroup() %>%
         group_by(ts_group) %>%
         mutate(prop = n / sum(n))) + 
  geom_bar(aes(x = ts_group,
               y = prop, 
               fill = ts),
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("blue", "skyblue", "skyblue2", 
                               "green", "green2", "forestgreen",
                               "brown", "gold")) + 
  xlab('') + ylab('') +
  guides(color = guide_legend(override.aes = list(size = 6),
                              nrow = 3)) +
  theme_void() +
  theme(legend.position = "bottom",
        axis.text.x = element_markdown(size = 18),
        axis.text.y = element_markdown(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 18))

ggsave(filename = "../figures/map_of_ts.png", plot = map, 
       device = "png", width = 15, height = 12, units = "in", bg = "white")


## Aggregate 2017 data 
nla_2017_sf <- nla_2017_formatted %>% 
  select(SITE_ID, ts_group, ts, LON_DD83, LAT_DD83) %>% unique() %>%
  drop_na() %>%
  mutate(ts_group = case_when(ts_group == "chla_ts" ~ "TSI<sub>CHLa</sub>",
                              ts_group == "crs_ts" ~ "TSI<sub>CR1</sub>",
                              ts_group == "ncp_ts" ~ "NCP",
                              ts_group == "ptl_ts" ~ "TSI<sub>TP</sub>",
                              ts_group == "rot_ts" ~ "TSI<sub>ROT</sub>",
                              ts_group == "sdd_ts" ~ "TSI<sub>SDD</sub>"),
         ts_group = factor(ts_group, 
                           levels = c("TSI<sub>CHLa</sub>", "TSI<sub>TP</sub>",
                                      "TSI<sub>SDD</sub>", "TSI<sub>ROT</sub>",
                                      "TSI<sub>CR1</sub>", "NCP")),
         ts = factor(ts, 
                     levels = c("oligo", "oligo-meso", "meso", 
                                "meso-eutro", "eutro", "hypereu",
                                "dys", "mixo"),
                     labels = c("Oligo", "Oligo-Meso", "Meso", 
                                "Meso-Eutro", "Eutro", "Hypereutro",
                                "Dys", "Mixo"))) %>%
  st_as_sf(coords = c("LON_DD83",
                      "LAT_DD83"),
           crs = st_crs('NAD83'))

## Generate barchart for each of the 9 Level III Aggregated Omernik Ecoregions
bar_plot <- map_df(.x = unique(epa_ecoregions$WSA9_NAME), 
       .f = ~  nla_2017_sf[epa_ecoregions[epa_ecoregions$WSA9_NAME == .x, ], ] %>%
         mutate(WSA9_NAME = paste(.x))) %>%
  st_drop_geometry() %>%
  group_by(WSA9_NAME, ts_group, ts) %>%
  count() %>%
  ungroup() %>%
  group_by(WSA9_NAME, ts_group) %>%
  mutate(prop = n / sum(n)) %>% View()
  ggplot() +
  geom_bar(aes(x = ts_group,
               y = prop, 
               fill = ts),
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#8da0cb", "#4891AB","#02818a", 
                               "#349454", "#66a61e","#006d2c", 
                               "#a6761d", "#e6ab02")) + 
  xlab('') + ylab('') +
  guides(color = guide_legend(override.aes = list(size = 6),
                              nrow = 3)) +
  theme_classic() +
  facet_wrap(~ WSA9_NAME) +
  theme(strip.text = element_text(size = 14), 
        legend.position = "bottom",
        axis.text.x = element_markdown(size = 14),
        axis.text.y = element_markdown(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 20))

aranged_plots <- plot_grid(
  map + theme(legend.position="none"),
  bar_plot + theme(legend.position="none"),
  labels = c("A", "B"), label_size = 20, align = 'v',
  nrow = 2)

legend <- get_legend(
  # create some space to the left of the legend
  bar_plot 
)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(aranged_plots, legend, rel_heights = c(1, .1), ncol = 1)

ggsave(filename = "../figures/ts_props_per_ecoregion.png", 
       height = 13, width = 13, units = "in", bg = "white")

