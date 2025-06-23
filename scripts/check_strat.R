### This script is created by Michael F Meyer (mfmeyere@usgs.gov) and Robert Ladwig (rladwig@ecos.au.dk) as part of 
### of the manuscript "Clarifying the trophic state concept to advance freshwater
### science, management, and interdisciplinary collaboration across spatial and 
### temporal scales" This script uses data from the US Environmental Protection
### Agency's National Lake Assessment (NLA) to produce depth profiles for 
### oxygen and temperature across several trophic state classifications. 
### This script takes depth profiles from the NLA and generates a list of lakes
### that are either stratified or mixed. These lakes are then passed to 
### "depth_profile_condensed.R". 

## Clean working environment
Sys.setenv(LANG = "en")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Sys.setlocale("LC_TIME", "English")

## Load necessary packages
library(tidyverse) ## 2.0.0
library(rLakeAnalyzer) ## 1.11.4.1
library(ggalluvial) ##0.12.5

### perform stratification check that does NOT include lake area information

data <- read.csv('../data/nla_2017_profile-data.csv')

str(data)
head(data)

df <- data %>%
  mutate(DATE = as.Date(DATE_COL, format = '%d-%b-%y')) %>% 
  mutate(ARRIVAL_TIME = ifelse(ARRIVAL_TIME != "", ARRIVAL_TIME, NA)) %>%
  fill(ARRIVAL_TIME) %>%
  mutate(DATETIME = (paste0(DATE,' ', ARRIVAL_TIME,':00'))) %>%
  select(SITE_ID, VISIT_NO, DATETIME, DEPTH, TEMPERATURE)

check_df <- data.frame('SITE_ID' = NULL, 'VISIT_NO' = NULL, 'DATETIME' = NULL,
                       'DENSITY_DIFF' = NULL, 'TEMP_DIFF' = NULL, 'AVG_TEMP' = NULL)
for (j in unique(df$SITE_ID)){
  tt_df <- df %>% filter(SITE_ID == j)
  for (n in unique(tt_df$DATETIME)){
    t_df <- tt_df %>% filter(DATETIME == n) %>%
      group_by(DEPTH) %>%
      summarise(wtmp = mean(TEMPERATURE),
                visit = mean(VISIT_NO))
    depths = t_df$DEPTH
    temps = t_df$wtmp
    
    if (any(is.na(depths))){
      idx = !is.na(depths)
      depths = depths[!is.na(depths)]
      
      temps = temps[idx]
    }
    
    if (any(is.na(temps))){
      idx = !is.na(temps)
      temps = temps[idx]
      
      if (length(temps) > 1){
        depths_orig = depths
        depths = depths[idx]
        temps <- approx(x = depths, y = temps, xout = depths_orig, rule = 2)$y
        depths = depths_orig
      } else {
        temps = NA
      }
      
      
    }
    
    if (length(temps)>1){
      dens_diff = water.density(temps[length(temps)]) - 
        water.density(temps[1])
      temp_dff =  (temps[length(temps)]) - 
        (temps[1])
      avg_temp = mean((temps))
    } else {
      dens_diff = NA
      temp_dff =  NA
      avg_temp = NA
    }
    
    
    check_df <- rbind(check_df, 
                      data.frame('SITE_ID' = j, 'VISIT_NO' = t_df$visit[1], 'DATETIME' = n,
                                 'DENSITY_DIFF' = dens_diff, 'TEMP_DIFF' = temp_dff, 'AVG_TEMP' = avg_temp))
  }  
}


### perform stratification check that does NOT include lake area information
data_area <- read.csv('../data/combined_profiles.csv')

str(data_area)
head(data_area)

df_area <- data_area %>%
  select(SITE_ID, VISIT_NO, DEPTH, TEMPERATURE, AREA_HA)

check_df_area <- data.frame('SITE_ID' = NULL, 'VISIT_NO' = NULL,
                       'DENSITY_DIFF' = NULL, 'TEMP_DIFF' = NULL, 'AVG_TEMP' = NULL, 'STRAT_FLAG' = NULL)
for (j in unique(df_area$SITE_ID)){
  tt_df <- df_area %>% filter(SITE_ID == j)
  for (n in unique(tt_df$VISIT_NO)){
    t_df <- tt_df %>% filter(VISIT_NO == n) %>%
      group_by(DEPTH) %>%
      summarise(wtmp = mean(TEMPERATURE),
                visit = mean(VISIT_NO),
                area_m2 = AREA_HA * 1000)
    

    
    
    if (all(is.na(t_df$wtmp)) |  length(na.omit(t_df$wtmp)) <= 1){
      bath = data.frame(Area.at.z = NA, depths = NA)
      depths = NA
      temps = NA
    } else {
      bath = approx.bathy(Zmax = max(t_df$DEPTH, na.rm =T),
                          lkeArea = max(t_df$area_m2, na.rm =T),
                          depths = seq(0, max(t_df$DEPTH, na.rm =T), 0.1))
      bath$Area.at.z[nrow(bath)] = bath$Area.at.z[nrow(bath)-1]
      temps_interp <- approx(x = t_df$DEPTH, y = t_df$wtmp, xout = bath$depths, rule = 2)
      
      depths = temps_interp$x
      temps = temps_interp$y
    }
    

   
    
    if (any(is.na(depths))){
      idx = !is.na(depths)
      depths = depths[!is.na(depths)]
      
      temps = temps[idx]
    }
    
    if (any(is.na(temps))){
      idx = !is.na(temps)
      temps = temps[idx]
      
      if (length(temps) > 1){
        depths_orig = depths
        depths = depths[idx]
        temps <- approx(x = depths, y = temps, xout = depths_orig, rule = 2)$y
        depths = depths_orig
      } else {
        temps = NA
      }
      
      
    }
    
    if (length(temps)>1){
      td = thermo.depth(wtr = temps, depths = depths)
      mdep = meta.depths(wtr = temps, depths = depths)
      
      if (!is.na(td)){
        STRAT_FLAG = T
        
        upper_layer = layer.temperature(top = 0, bottom = mdep[1],
                                        wtr = temps, depths = depths, bthA =  bath$Area.at.z, bthD = bath$depths)
        lower_layer = layer.temperature(top = mdep[2], bottom = max(depths),
                                        wtr = temps, depths = depths, bthA =  bath$Area.at.z, bthD = bath$depths)
        dens_diff = water.density(lower_layer) - 
          water.density(upper_layer)
        temp_dff =  (lower_layer) - 
          (upper_layer)
        
      } else {
        dens_diff = water.density(temps[length(temps)]) - 
          water.density(temps[1])
        temp_dff =  (temps[length(temps)]) - 
          (temps[1])
        
        STRAT_FLAG = F
      }

      avg_temp = layer.temperature(top = 0, bottom = max(depths), wtr = temps, depths = depths, bthA =  bath$Area.at.z, bthD = bath$depths)
    } else {
      dens_diff = NA
      temp_dff =  NA
      avg_temp = NA
      STRAT_FLAG = F
    }
    
    
    check_df_area <- rbind(check_df_area, 
                      data.frame('SITE_ID' = j, 'VISIT_NO' = t_df$visit[1],
                                 'DENSITY_DIFF' = dens_diff, 'TEMP_DIFF' = temp_dff, 'AVG_TEMP' = avg_temp,
                                 'STRAT_FLAG' = STRAT_FLAG))
  }  
}

## Compare results from stratification check that includes lake area with those that do not include lake area

# this one does not consider lake hypsography (we assume a simplified cone shape)
check_df <- check_df %>% 
  mutate(stratified = ifelse(DENSITY_DIFF >= 0.1 & AVG_TEMP >= 4, 'stratified', 'mixed'))

# here we consider hypsography assuming every lake is a cone (simplified!)
check_df_area <- check_df_area %>% 
  mutate(stratified = ifelse(DENSITY_DIFF >= 0.1 & AVG_TEMP >= 4 & STRAT_FLAG == T, 'stratified', 'mixed'))

check_all = merge(check_df, check_df_area, by = c('SITE_ID', 'VISIT_NO'))

check_all= check_all %>% mutate(SITE_VISIT = paste0(SITE_ID,'_',VISIT_NO))

ggplot()+
  geom_point(data = check_df,
             aes(SITE_ID, TEMP_DIFF, col = stratified))+
  geom_point(data = check_df_area,
             aes(SITE_ID, TEMP_DIFF, col = stratified))


# Create a summary table
df_summary <- check_all %>%
  filter(!is.na(stratified.x) & !is.na(stratified.y)) %>%
  count(stratified.x, stratified.y)

# Plot as heatmap
ggplot(df_summary, aes(x = stratified.x, y = stratified.y, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "wo area", y = "w area and thermocline", title = "Changes Between Both Methods") +
  theme_minimal()

ggplot(df_summary, aes(axis1 = stratified.x, axis2 = stratified.y, y = n)) +
  geom_alluvium(aes(fill = stratified.x)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  labs(x = "State", y = "Count", title = "Transition of Stratification States") +
  theme_minimal()

check_df %>% count(stratified)
check_df_area %>% count(stratified)

write_csv(x = check_df_area, "../data/derived_products/chexk_strat_area.csv")
