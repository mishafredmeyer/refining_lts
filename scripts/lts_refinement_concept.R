library(tidyverse)
library(scales)

### Load the data

nla_2007_tptnchla <- read_csv("../data/nla_all_years/NLA2007_WaterQuality_20091123.csv")

nla_2007_sdd <- read_csv("../data/nla_all_years/nla2007_secchi_20091008.csv")

nla_2007_zoop <- read_csv("../data/nla_all_years/nla2007_zooplankton_count_20091022.csv")

nla_2012_tptn <- read_csv("../data/nla_all_years/nla2012_waterchem_wide.csv")

nla_2012_chla <- read_csv("../data/nla_all_years/nla2012_chla_wide.csv")

nla_2012_sdd <- read_csv("../data/nla_all_years/nla2012_secchi_08232016.csv")

nla_2012_zoop <- read_csv("../data/nla_all_years/nla-2012-zooplankton-count-data-updated-12092021.csv")

nla_2012_metadata <- read_csv("../data/nla_all_years/nla2012_wide_siteinfo_08232016.csv")

nla_2012_toxins <- read_csv("../data/nla_all_years/nla2012_algaltoxins_08192016.csv")

nla_2017_tntp <- read_csv("../data/nla_all_years/nla_2017_water_chemistry_chla-data.csv")

nla_2017_metadata <- read_csv("../data/nla_all_years/nla_2017_site_information-data.csv")

nla_2017_sdd <- read_csv("../data/nla_all_years/nla_2017_secchi-data.csv")

nla_2017_zoop <- read_csv("../data/nla_all_years/nla-2017-zooplankton-count-data.csv")

nla_2017_toxins <- read_csv("../data/nla_all_years/nla_2017_algal_toxin-data.csv")

nla_2017_ecoli <- read_csv("../data/nla_all_years/nla_2017_e.coli-data.csv")
  
### Clean 2007 Data

nla_2007_formatted <- nla_2007_tptnchla %>%
  select(SITE_ID, YEAR, VISIT_NO, PTL, NTL, CHLA, COLOR, DOC) %>%
  tibble() %>%
  group_by(SITE_ID, YEAR) %>%
  summarize(across(.cols = c(PTL, NTL, CHLA, COLOR, DOC), 
                   .fns = ~ mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  inner_join(x = .,
             y = nla_2007_sdd %>%
               select(SITE_ID, YEAR, VISIT_NO, SECMEAN) %>%
               tibble() %>%
               group_by(SITE_ID, YEAR) %>%
               summarize(across(.cols = c(SECMEAN), 
                                .fns = ~ mean(.x, na.rm = TRUE))) %>%
               ungroup()) %>%
  rename("SECCHI" = SECMEAN) %>%
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
         sdd_ts = ifelse(SECCHI >= 4, "oligo", NA),
         sdd_ts = ifelse(SECCHI >= 2 & SECCHI < 4, "meso", sdd_ts),
         sdd_ts = ifelse(SECCHI >= 0.5 & SECCHI < 2, "eutro", sdd_ts),
         sdd_ts = ifelse(SECCHI < 0.5, "hypereu", sdd_ts),
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

### NLA 2012 Cleaning

nla_2012_ptl_color <- nla_2012_tptn %>%
  select(UID, contains(c("color", "ptl", "ntl", "DOC"))) %>%
  select(UID, 
         "COLOR" = COLOR_RESULT, 
         "PTL" = PTL_RESULT,
         "NTL" = NTL_RESULT, 
         "DOC" = DOC_RESULT) %>%
  mutate(YEAR = 2012) %>%
  tibble() %>%
  inner_join(., nla_2012_metadata) %>%
  select(COLOR, NTL, PTL, DOC, YEAR, SITE_ID, -UID) %>%
  group_by(YEAR, SITE_ID) %>%
  summarize(across(.cols = c(COLOR, PTL, NTL, DOC), 
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
  inner_join(x = .,
             y = nla_2012_toxins %>%
               mutate(YEAR = 2012) %>%
               select(SITE_ID, YEAR, MICX_RESULT, MICL_RESULT) %>%
               group_by(SITE_ID, YEAR) %>%
               summarize(across(.cols = c(MICX_RESULT, MICL_RESULT), 
                                .fns = ~ mean(.x, na.rm = TRUE))) %>%
               rename("MICX" = MICX_RESULT,
                      "MICL" = MICL_RESULT)) %>%
  inner_join(x = nla_2012_zoop %>%
               mutate(YEAR = 2012) %>%
               select(SITE_ID, YEAR, BIOMASS, ORDER) %>%
               group_by(SITE_ID, YEAR, ORDER) %>%
               summarize(across(.cols = c(BIOMASS), 
                                .fns = ~ mean(.x, na.rm = TRUE))) %>%
               pivot_wider(names_from = ORDER, values_from = BIOMASS),
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
         sdd_ts = ifelse(SECCHI >= 4, "oligo", NA),
         sdd_ts = ifelse(SECCHI >= 2 & SECCHI < 4, "meso", sdd_ts),
         sdd_ts = ifelse(SECCHI >= 0.5 & SECCHI < 2, "eutro", sdd_ts),
         sdd_ts = ifelse(SECCHI < 0.5, "hypereu", sdd_ts),
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



### Clean 2017 data

nla_2017_formatted <- nla_2017_tntp %>%
  filter(ANALYTE %in% c("COLOR", "PTL", "NTL", "CHLA", "DOC", "NITRATE_N", "NITRITE_N", "AMMONIA_N")) %>%
  mutate(RESULT = as.numeric(RESULT),
         YEAR = 2017) %>%
  select(UID, YEAR, ANALYTE, RESULT) %>%
  pivot_wider(names_from = "ANALYTE", values_from = "RESULT") %>%
  tibble() %>%
  inner_join(., nla_2017_metadata) %>%
  select(COLOR, PTL, YEAR, SITE_ID, NTL, CHLA, DOC, NITRATE_N, NITRITE_N, AMMONIA_N, -UID) %>%
  group_by(YEAR, SITE_ID) %>%
  summarize(across(.cols = c("COLOR", "NTL", "PTL", "CHLA", "DOC", "NITRATE_N", "NITRITE_N", "AMMONIA_N"), 
                   .fns = ~ mean(.x, na.rm = TRUE))) %>%
  inner_join(x = .,
             y = nla_2017_sdd %>%
               mutate(YEAR = 2017,
                      secchi = (DISAPPEARS + REAPPEARS)/2) %>%
               group_by(SITE_ID, YEAR) %>%
               summarize(across(.cols = c(secchi),
                                .fns = ~ mean(.x, na.rm = TRUE)))) %>%
  inner_join(x = .,
             y = nla_2017_ecoli %>%
               mutate(YEAR = 2017) %>%
               group_by(SITE_ID, YEAR) %>%
               summarize(across(.cols = c(E_COLI_RESULT),
                                .fns = ~ mean(.x, na.rm = TRUE)))) %>%
  inner_join(x = .,
             y = nla_2017_toxins %>%
               filter(ANALYTE == "MICX") %>%
               mutate(YEAR = 2017) %>%
               group_by(SITE_ID, YEAR) %>%
               summarize(across(.cols = c(RESULT),
                                .fns = ~ mean(.x, na.rm = TRUE))) %>%
               rename("MICX" = RESULT)) %>%
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

### microcystin plot...

sdd_boxplots <- bind_rows(nla_2017_formatted %>% ungroup() %>%
            select(ts_group, ts, "SECCHI" = secchi, DOC, MICX, COLOR, CHLA) %>%
            unique(),
          nla_2012_ptl_color %>% ungroup() %>%
            select(ts_group, ts, SECCHI, DOC, MICX, COLOR, CHLA) %>%
            unique(),
          nla_2007_formatted %>% ungroup() %>%
            select(ts_group, ts, SECCHI, DOC, COLOR, CHLA) %>%
            unique()) %>% 
  filter(!is.na(ts),
         ts_group %in% c("sdd_ts", "ncp_ts")) %>%
  mutate(across(.cols = c(SECCHI, DOC, MICX, CHLA), 
                .fns = ~ ifelse(is.nan(.x), 0, .x)),
         ts_group = gsub(pattern = "_ts", replacement = "", x = ts_group),
         Category = case_when(ts == "dys" ~ "Dystrophic",
                              ts == "eutro" ~ "Eutrophic",
                              ts == "meso" ~ "Mesotrophic",
                              ts == "oligo" ~ "Oligotrophic",
                              ts == "hypereu" ~ "Hypereutrophic",
                              ts == "mixo" ~ "Mixotrophic"),
        ts_group_condesed = case_when(ts_group == "ncp" ~ "NCP",
                                      ts_group == "sdd" ~ "TSI[SDD]"),
        x_axis = as.factor(paste(Category, ts_group_condesed, sep = ",\n"))) %>% 
  group_by(x_axis) %>%
  mutate(x_axis = fct_reorder(x_axis, SECCHI, .fun = 'mean')) %>%
  ggplot() +
  geom_boxplot_pattern(aes(x=reorder(x_axis, SECCHI, decreasing = TRUE), 
                           y=log1p(CHLA),
                           fill=Category,
                           pattern=ts_group_condesed),
                       pattern="stripe",
                       pattern_fill="black",
                       pattern_spacing=0.02,
                       pattern_density=0.1) +
  geom_boxplot(aes(x=reorder(x_axis, SECCHI, decreasing = TRUE),
                   y=log1p(CHLA),
                   fill=Category,
                   alpha=ts_group_condesed)) +
  scale_fill_manual(values=c("#a6761d","#66a61e","#006d2c", "#02818a", "#e6ab02", "#8da0cb"),
                    name="Trophic Category:") +
  scale_alpha_manual(values=c(0,1),name="Classification\nSystem:",labels=c("NCP","TSI[CHLA]")) +
  scale_y_continuous(breaks=log1p(c(0,10,30, 100, 300, 1000, 3000)),
                     labels=c(0,10,30, 100, 300, 1000, 3000)) +
  labs(x="Trophic State/Status",y="Chlorophyll a (\U03BCg/L)") +
  #guides(fill=guide_legend(ncol=2)) +
  theme_bw() +
  theme(axis.text=element_text(size=15,color="black"),
         axis.title=element_text(size=15,color="black"),
         legend.text=element_text(size=15,color="black"),
         legend.title=element_text(size=15,color="black"),
         legend.position="bottom")

micx_boxplots <- bind_rows(nla_2017_formatted %>% ungroup() %>%
            select(ts_group, ts, "SECCHI" = secchi, DOC, MICX, COLOR) %>%
            unique(),
          nla_2012_ptl_color %>% ungroup() %>%
            select(ts_group, ts, SECCHI, DOC, MICX, COLOR) %>%
            unique(),
          nla_2007_formatted %>% ungroup() %>%
            select(ts_group, ts, SECCHI, DOC, COLOR) %>%
            unique()) %>% 
  filter(!is.na(ts),
         ts_group %in% c("sdd_ts", "ncp_ts")) %>%
  mutate(across(.cols = c(SECCHI, DOC, MICX), 
                .fns = ~ ifelse(is.nan(.x), 0, .x)),
         ts_group = gsub(pattern = "_ts", replacement = "", x = ts_group),
         Category = case_when(ts == "dys" ~ "Dystrophic",
                              ts == "eutro" ~ "Eutrophic",
                              ts == "meso" ~ "Mesotrophic",
                              ts == "oligo" ~ "Oligotrophic",
                              ts == "hypereu" ~ "Hypereutrophic",
                              ts == "mixo" ~ "Mixotrophic"),
         ts_group_condesed = case_when(ts_group == "ncp" ~ "NCP",
                                       ts_group == "sdd" ~ "TSI[SDD]"),
         x_axis = as.factor(paste(Category, ts_group_condesed, sep = ",\n"))) %>% 
  group_by(x_axis) %>%
  mutate(x_axis = fct_reorder(x_axis, SECCHI, .fun = 'mean')) %>%
  ggplot() +
  geom_boxplot_pattern(aes(x=reorder(x_axis, SECCHI, decreasing = TRUE), 
                           y=log1p(MICX),
                           fill=Category,
                           pattern=ts_group_condesed),
                       pattern="stripe",
                       pattern_fill="black",
                       pattern_spacing=0.02,
                       pattern_density=0.1) +
  geom_boxplot(aes(x=reorder(x_axis, SECCHI, decreasing = TRUE),
                   y=log1p(MICX),
                   fill=Category,
                   alpha=ts_group_condesed)) +
  scale_fill_manual(values=c("#a6761d","#66a61e","#006d2c", "#02818a", "#e6ab02", "#8da0cb"),
                    name="Trophic Category:") +
  scale_alpha_manual(values=c(0,1),name="Classification\nSystem:",labels=c("NCP","TSI[CHLA]")) +
  scale_y_continuous(breaks=log1p(c(0,1, 3, 10, 30, 100)),labels=c(0,1, 3, 10, 30, 100)) +
  labs(x="Trophic State/Status",y="Microcystin (\U03BCg/L)") +
  guides(fill=guide_legend(ncol=1)) +
  theme_bw() +
  theme(axis.text=element_text(size=15,color="black"),
        axis.title=element_text(size=15,color="black"),
        legend.text=element_text(size=15,color="black"),
        legend.title=element_text(size=15,color="black"),
        legend.background=element_rect(color="black"),
        legend.position= "none")


doc_boxplots <- bind_rows(nla_2017_formatted %>% ungroup() %>%
            select(ts_group, ts, "SECCHI" = secchi, DOC, MICX, COLOR) %>%
            unique(),
          nla_2012_ptl_color %>% ungroup() %>%
            select(ts_group, ts, SECCHI, DOC, MICX, COLOR) %>%
            unique(),
          nla_2007_formatted %>% ungroup() %>%
            select(ts_group, ts, SECCHI, DOC, COLOR) %>%
            unique()) %>% 
  filter(!is.na(ts),
         ts_group %in% c("sdd_ts", "ncp_ts")) %>%
  mutate(across(.cols = c(SECCHI, DOC, MICX), 
                .fns = ~ ifelse(is.nan(.x), 0, .x)),
         ts_group = gsub(pattern = "_ts", replacement = "", x = ts_group),
         Category = case_when(ts == "dys" ~ "Dystrophic",
                              ts == "eutro" ~ "Eutrophic",
                              ts == "meso" ~ "Mesotrophic",
                              ts == "oligo" ~ "Oligotrophic",
                              ts == "hypereu" ~ "Hypereutrophic",
                              ts == "mixo" ~ "Mixotrophic"),
         ts_group_condesed = case_when(ts_group == "ncp" ~ "NCP",
                                       ts_group == "sdd" ~ "TSI[SDD]"),
         x_axis = as.factor(paste(Category, ts_group_condesed, sep = ",\n"))) %>% 
  group_by(x_axis) %>%
  mutate(x_axis = fct_reorder(x_axis, SECCHI, .fun = 'mean')) %>%
  ggplot() +
  geom_boxplot_pattern(aes(x=reorder(x_axis, SECCHI, decreasing = TRUE), 
                           y=log1p(DOC),
                           fill=Category,
                           pattern=ts_group_condesed),
                       pattern="stripe",
                       pattern_fill="black",
                       pattern_spacing=0.02,
                       pattern_density=0.1) +
  geom_boxplot(aes(x=reorder(x_axis, SECCHI, decreasing = TRUE),
                   y=log1p(DOC),
                   fill=Category,
                   alpha=ts_group_condesed)) +
  scale_fill_manual(values=c("#a6761d","#66a61e","#006d2c", "#02818a", "#e6ab02", "#8da0cb"),
                    name="Trophic Category:") +
  scale_alpha_manual(values=c(0,1),name="Classification\nSystem:",labels=c("NCP","TSI[CHLA]")) +
  scale_y_continuous(breaks=log1p(c(0,3,10,30,100, 300, 800)),labels=c(0,3,10,30,100, 300, 800)) +
  labs(x="Trophic State/Status",y="DOC (mg/L)") +
  guides(fill=guide_legend(nrow=1)) +
  theme_bw() +
  theme(axis.text=element_text(size=15,color="black"),
        axis.title=element_text(size=15,color="black"),
        legend.text=element_text(size=15,color="black"),
        legend.title=element_text(size=15,color="black"),
        legend.background=element_rect(color="black"),
        legend.position= "none")
#legend.box = "horizontal")

pcu_boxplots <- bind_rows(nla_2017_formatted %>% ungroup() %>%
            select(ts_group, ts, "SECCHI" = secchi, DOC, MICX, COLOR) %>%
            unique(),
          nla_2012_ptl_color %>% ungroup() %>%
            select(ts_group, ts, SECCHI, DOC, MICX, COLOR) %>%
            unique(),
          nla_2007_formatted %>% ungroup() %>%
            select(ts_group, ts, SECCHI, DOC, COLOR) %>%
            unique()) %>% 
  filter(!is.na(ts),
         ts_group %in% c("chla_ts", "ncp_ts")) %>%
  mutate(across(.cols = c(SECCHI, DOC, MICX), 
                .fns = ~ ifelse(is.nan(.x), 0, .x)),
         ts_group = gsub(pattern = "_ts", replacement = "", x = ts_group),
         Category = case_when(ts == "dys" ~ "Dystrophic",
                              ts == "eutro" ~ "Eutrophic",
                              ts == "meso" ~ "Mesotrophic",
                              ts == "oligo" ~ "Oligotrophic",
                              ts == "hypereu" ~ "Hypereutrophic",
                              ts == "mixo" ~ "Mixotrophic"),
         ts_group_condesed = case_when(ts_group == "ncp" ~ "NCP",
                                       ts_group == "chla" ~ "TSI[CHLA]"),
         x_axis = as.factor(paste(Category, ts_group_condesed, sep = ",\n"))) %>% 
  group_by(x_axis) %>%
  mutate(x_axis = fct_reorder(x_axis, SECCHI, .fun = 'mean')) %>%
  ggplot() +
  geom_boxplot_pattern(aes(x=reorder(x_axis, SECCHI, decreasing = TRUE), 
                           y=log1p(COLOR),
                           fill=Category,
                           pattern=ts_group_condesed),
                       pattern="stripe",
                       pattern_fill="black",
                       pattern_spacing=0.02,
                       pattern_density=0.1) +
  geom_boxplot(aes(x=reorder(x_axis, SECCHI, decreasing = TRUE),
                   y=log1p(COLOR),
                   fill=Category,
                   alpha=ts_group_condesed)) +
  scale_fill_manual(values=c("#a6761d","#66a61e","#006d2c", "#02818a", "#e6ab02", "#8da0cb"),
                    name="Trophic Category:") +
  scale_alpha_manual(values=c(0,1),name="Classification\nSystem:",labels=c("NCP","TSI[CHLA]")) +
  scale_y_continuous(breaks=log1p(c(0,3,10,30,100, 300, 800)),labels=c(0,3,10,30,100, 300, 800)) +
  labs(x="Trophic State/Status",y="Color (PCU)") +
  guides(fill=guide_legend(ncol=1)) +
  theme_bw() +
  theme(axis.text=element_text(size=15,color="black"),
        axis.title=element_text(size=15,color="black"),
        legend.text=element_text(size=15,color="black"),
        legend.title=element_text(size=15,color="black"),
        legend.background=element_rect(color="black"),
        legend.position= "none")
#legend.box = "horizontal")

arranged_boxplots <- plot_grid(
  sdd_boxplots + theme(axis.text.x = element_blank(),
                       axis.title.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       legend.position = "none"),
  micx_boxplots + theme(axis.text.x = element_blank(),
                        axis.title.x = element_blank(),
                        axis.ticks.x = element_blank()),
  doc_boxplots + theme(axis.text.x = element_blank(),
                       axis.title.x = element_blank(),
                       axis.ticks.x = element_blank()),
  pcu_boxplots + theme(axis.text.x = element_blank(),
                       axis.title.x = element_blank(),
                       axis.ticks.x = element_blank()),
  labels = c("A", "B", "C", "D"), label_size = 20,
  nrow = 2, ncol = 2)

legend_b <- get_legend(
  sdd_boxplots
)

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
plot_grid(arranged_boxplots, legend_b, ncol = 1, rel_heights = c(1, .1))

ggsave(filename = "../figures/water_quality_boxplots.png", 
       #plot = arranged_boxplots, 
       device = "png", width = 12, height = 12, units = "in", bg = "white")

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
