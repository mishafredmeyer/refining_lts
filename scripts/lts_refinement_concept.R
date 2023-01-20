library(tidyverse)

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
  select(COLOR, PTL, YEAR, SITE_ID, NTL, CHLA, -UID) %>%
  group_by(YEAR, SITE_ID) %>%
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
               select(SITE_ID, YEAR, BIOMASS, ORDER, FAMILY) %>%
               group_by(SITE_ID, YEAR, ORDER, FAMILY) %>%
               summarize(across(.cols = c(BIOMASS), 
                                .fns = ~ sum(.x, na.rm = TRUE))),
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
         ncp_ts = factor(ncp_ts, 
                         levels = c("dys", "oligo",
                                    "eutro", "mixo")),
         chla_ts = factor(chla_ts, 
                         levels = c("oligo", "meso", "eutro", "hypereu")),
         ptl_ts = factor(ptl_ts, 
                          levels = c("oligo", "meso", "eutro", "hypereu")),
         sdd_ts = factor(sdd_ts, 
                         levels = c("oligo", "meso", "eutro", "hypereu"))) %>%
  pivot_longer(cols = c(ncp_ts, chla_ts, ptl_ts, sdd_ts), names_to = "ts_group", values_to = "ts")

all_plots <- map(.x = c("ncp_ts", "sdd_ts", "chla_ts", "ptl_ts"),
                 .f = ~ nla_2017_formatted %>%
                   filter(ts_group == .x) %>%
                   filter(!is.na(ts),
                          !is.na(ORDER)) %>%
                   group_by(ts, ORDER) %>%
                   summarize(BIOMASS = mean(BIOMASS, na.rm = TRUE)) %>%
                   ungroup() %>%
                   group_by(ts) %>%
                   mutate(prop = BIOMASS / sum(BIOMASS, na.rm = TRUE)) %>%
                   mutate(ts = factor(ts, levels = c("dys", "oligo", "meso",
                                                        "eutro", "mixo", "hypereu"))) %>%
                   filter(!is.na(BIOMASS),
                          !ORDER %in% c("AMPPHIPODA", "ARGULOIDA", "MYSIDA",
                                        "TROMBIDIFORMES", "VENEROIDA")) %>%
                   ggplot(aes(x = ts, 
                              y = prop, 
                              fill = ORDER)) +
                   geom_bar(stat = "identity") +
                   #scale_y_continuous(limits = c(0,300)) +
                   ggtitle(if(.x == "ncp_ts"){paste("NCP")}
                           else if(.x == "chla_ts"){paste("TSI-CHLA")}
                           else if(.x == "ptl_ts"){paste("TSI-PTL")}
                           else if(.x == "sdd_ts"){paste("TSI-SDD")}) +
                   theme_bw())

combined_plots <- ggpubr::ggarrange(plotlist = all_plots, ncol = 2,
                                     nrow = 2, common.legend = TRUE, 
                                     legend = "right", labels = "AUTO")

ggsave(filename = "../figures/conceptual_ts_figure_proportions.png", 
       plot = combined_plots, device = "png", 
       height = 6, width = 8, units = "in", bg = "white")


all_plots <- map(.x = c("ncp_ts", "sdd_ts", "chla_ts", "ptl_ts"),
                 .f = ~ nla_2017_formatted %>%
                   filter(ts_group == .x) %>%
                   filter(!is.na(ts),
                          !is.na(ORDER)) %>%
                   group_by(ts) %>%
                   filter(!is.na(BIOMASS),
                          !ORDER %in% c("AMPPHIPODA", "ARGULOIDA", "MYSIDA",
                                        "TROMBIDIFORMES", "VENEROIDA")) %>%
                   summarize(tte = sum(BIOMASS, na.rm = TRUE)/CHLA)%>%
                   mutate(ts = factor(ts, levels = c("dys", "oligo", "meso",
                                                     "eutro", "mixo", "hypereu"))) %>%
                   ggplot(aes(x = ts, 
                              y = tte)) +
                   geom_boxplot() +
                   scale_y_log10() +
                   ggtitle(if(.x == "ncp_ts"){paste("NCP")}
                           else if(.x == "chla_ts"){paste("TSI-CHLA")}
                           else if(.x == "ptl_ts"){paste("TSI-PTL")}
                           else if(.x == "sdd_ts"){paste("TSI-SDD")}) +
                   theme_bw())

combined_plots <- ggpubr::ggarrange(plotlist = all_plots, ncol = 2,
                                     nrow = 2, common.legend = TRUE, 
                                     legend = "right", labels = "AUTO")

ggsave(filename = "../figures/tte_oper_lts.png", 
       plot = combined_plots, device = "png", 
       height = 6, width = 8, units = "in", bg = "white")
