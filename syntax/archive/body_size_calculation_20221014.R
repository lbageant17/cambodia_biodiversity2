## Liz Bageant
## October 11, 2022

#------------------------------------------------------------------------------# 
#
#  CONSTRUCTING BODY SIZE FIGURES
#
#------------------------------------------------------------------------------#


# Starting from scratch. Process: 
# Calculate system level mean total length
# Calculate CFR level mean total length
# Household-level mean total length (this can be one file)
#   Catch
#   Consumption
#   Sold

### System level mean total length
  # first get total biomass for calculating proportional abundance
  total_biomass_biom <- b %>% 
    summarise(total = sum(totalweight_biom)) %>% 
    as.numeric()
  # calculate proportional abundance at system level
  pa_sys <- b %>% 
    ungroup() %>% 
    group_by(scode_biom) %>% 
    summarize(biomass = sum(totalweight_biom)) %>% 
    mutate(pa_sys = biomass/total_biomass_biom)
  # calculate mean total length at system level
  tl_system <- pa_sys %>% 
    left_join(., select(ccm_traits_specieslevel, c(tl, scode_biom)), by = "scode_biom") %>% 
    ungroup() %>% 
    mutate(tl_weighted = tl*pa_sys) %>% 
    summarise(tl_sys = mean(tl_weighted, na.rm = TRUE)) %>% 
    as.numeric()


### CFR level mean total length

  # calculate proportional abundance at CFR level 
  pa_cfr <- b %>%
    ungroup() %>% 
    group_by(cfrid, scode_biom) %>% 
    summarize(biomass = sum(totalweight_biom)) %>% 
    mutate(pa_cfr = biomass/total_biomass_biom)
  # calculate mean total length at CFR level weighted by biomass
  tl_cfr <- pa_cfr %>% 
    left_join(., select(ccm_traits_specieslevel, c(tl, scode_biom)), by = "scode_biom") %>% 
    ungroup() %>% 
    mutate(tl_weighted = tl*pa_cfr) %>% 
    group_by(cfrid) %>% 
    summarise(tl_cfr = mean(tl_weighted, na.rm = TRUE)) %>% 
    mutate(type = "cfr",
           tl = tl_cfr) %>% 
    select(type, tl)

### HOUSEHOLD LEVEL mean total length--catch, consumption, sold
  
  ## CATCH
  # total catch biomass for calculating proportional abundance
  total_biomass_catch <- c %>%
    summarise(biomass = sum(catch_iweight)) %>% 
    as.numeric()
  # calculate proportional abundance of caught species
  pa_catch <- c %>% 
    ungroup() %>% 
    group_by(hhid, scode_ccm) %>% 
    summarise(biomass = sum(catch_iweight)) %>% 
    filter(biomass != 0) %>%  # if there are species that are never caught, this removes them (but there aren't)
    mutate(pa_catch = biomass/total_biomass_catch)
  # calculate mean total length of caught spcies weighted by biomass
  tl_catch <- pa_catch %>% 
    left_join(., select(ccm_traits_specieslevel, c(tl, scode_ccm)), by = "scode_ccm") %>% 
    ungroup() %>% 
    mutate(tl_weighted = tl*pa_catch) %>% 
    group_by(hhid) %>% 
    summarise(tl_catch = mean(tl_weighted, na.rm = TRUE)) %>% 
    mutate(type = "catch",
           tl = tl_catch) %>% 
    select(type, tl)
  
  ## CONSUMPTION
  # total consumption biomass
  total_biomass_cons <- c %>% 
    summarise(biomass = sum(catch_iweight)) %>% 
    as.numeric()
  # proportional abundance of consumed species
  pa_cons <- c %>% 
    ungroup() %>% 
    group_by(hhid, scode_ccm) %>% 
    summarise(biomass = sum(atefresh_iweight)) %>%
    filter(biomass != 0) %>%  # remove species that are not eaten (655 cases)
    mutate(pa_cons = biomass/total_biomass_cons) 
  # calculate mean total length of consumed species weighted by biomass
  tl_cons <- pa_cons %>% 
    left_join(., select(ccm_traits_specieslevel, c(tl, scode_ccm)), by = "scode_ccm") %>% 
    ungroup() %>% 
    mutate(tl_weighted = tl*pa_cons) %>% 
    group_by(hhid) %>% 
    summarise(tl_cons = mean(tl_weighted, na.rm = TRUE)) %>% 
    mutate(type = "cons",
           tl = tl_cons) %>% 
    select(type, tl) 
  
  ## SOLD
  # total biomass sold
  total_biomass_sold <- c %>% 
    summarise(biomass = sum(soldfresh_iweight)) %>% 
    as.numeric()
  # proportional abundance of sold species
  pa_sold <- c %>% 
    ungroup() %>% 
    group_by(hhid, scode_ccm) %>% 
    summarise(biomass = sum(soldfresh_iweight)) %>% 
    filter(biomass != 0) %>%  # removing all cases where certain species were never sold. 
    mutate(pa_sold = biomass/total_biomass_sold)
  # calculate mean total length of consumed species weighted by biomass
  tl_sold <- pa_sold %>% 
    left_join(., select(ccm_traits_specieslevel, c(tl, scode_ccm)), by = "scode_ccm") %>% 
    ungroup() %>% 
    mutate(tl_weighted = tl*pa_sold) %>% 
    group_by(hhid) %>% 
    summarise(tl_sold = mean(tl_weighted, na.rm = TRUE)) %>% 
    mutate(type = "sold",
           tl = tl_sold) %>% 
    select(type, tl) 
  
  
### COMBINE FILES
  body_size <- tl_catch %>% 
    rbind(tl_cons) %>% 
    rbind(tl_sold) %>% 
    rbind(tl_cfr)
  
  
### BOXPLOT
  
  sys <- tl_system
  
  body_size %>% 
    mutate(type = factor(type, levels = c("cfr", "catch", "cons", "sold" ))) %>% 
    ggplot(aes(x = type, y = tl, fill = type)) +
    geom_boxplot(outlier.shape = NA) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.7, name = "Portfolio type", labels = c("CFR", "Household Catch", "Household Consumption", "Household Sold")) +
    geom_jitter(size = 0.7, alpha = 0.4, width = 0.2, color = "black") +
    geom_hline(yintercept = sys, color = "#00CCCC", size = 1) + # horizontal line corresponding to value in minbio_sys_biom
    #scale_y_continuous(trans = "log1p") + # THIS WORKS IN CODE BELOW
    #coord_trans(y = "log1p") +
    #scale_y_log10() +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.caption = element_text(hjust = 0)) +
    ylab("Mean total length") +
    labs(title = "Mean (biomass-weighted) total length by portfolio type")
  

view(body_size)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

BELOW HERE IS OLD CODE

#------------------------------------------------------------------------------# 
#### Body size (total length) boxplot
  # goal is to calculate mean body size at different levels: system (line), CFR, household catch, household consumption, household sold
#------------------------------------------------------------------------------# 

# system-level mean total length, weighted by biomass at system level (via relative abundance)
mbs_system <- b %>% 
  left_join(ccm_traits_specieslevel, by = "scode_biom") %>% 
  drop_na(tl) %>% 
  mutate(abundance_weighted_tl = tl*rel_abundance) %>% 
  select(abundance_weighted_tl, tl, rel_abundance) %>% 
  summarise(mbs_system = mean(abundance_weighted_tl))

### Mean total length, weighted by biomass at CFR level (using BIOM data) 
  # calculate CFR-level proportional abundance by species
  pa_cfr <- b %>% 
    select(cfrid, scode_biom, totalweight_biom) %>% 
    # get total CFR-level by species
    group_by(cfrid, scode_biom) %>% 
    summarise(cfr_kg_species = sum(totalweight_biom)) %>% 
    # bring in total biom by CFR 
    left_join(total_cfr_biom, by = "cfrid") %>% 
    # calculate proportional abundance
    mutate(pa = cfr_kg_species/total_cfr_catch) %>% 
    mutate_at(vars(pa), ~replace(., is.nan(.), 0)) %>% # change all NaN values to zero
    select(cfrid, scode_biom, pa)
  
    mbs_cfr <- b %>% 
      left_join(ccm_traits_specieslevel, by = "scode_biom") %>% 
      left_join(pa_cfr, by = c("cfrid", "scode_biom")) %>% 
      select(cfrid, tl, pa) %>% 
      drop_na(tl) %>% 
      mutate(abundance_weighted_tl = tl*pa) %>% 
      select(cfrid, abundance_weighted_tl, tl, pa) %>% 
      group_by(cfrid) %>% 
      summarise(mean_tl = mean(tl)) %>% 
      mutate(type = "cfr") %>% 
      select(-cfrid)
    

### Mean total length, weighted by biomass at HOUSEHOLD CATCH level
  # household catch proportional abundance by species
    pa_hh_catch <- c %>% 
      select(hhid, scode_ccm, catch_iweight) %>% 
      # get total catch by species
      group_by(hhid, scode_ccm) %>% 
      summarise(catch_kg_species = sum(catch_iweight)) %>% 
      # bring in total catch by household
      left_join(total_hh, by = "hhid") %>% 
      # calculate proportional abundance
      mutate(pa = catch_kg_species/total_catch_kg) %>% 
      select(hhid, scode_ccm, pa)
  
    mbs_hh_catch <- c %>% 
      left_join(ccm_traits_specieslevel, by = "scode_ccm") %>% 
      left_join(pa_hh_catch, by = c("hhid", "scode_ccm")) %>% 
      #drop_na(tl) %>%
      mutate(abundance_weighted_tl = tl*pa) %>% 
      select(hhid, tl, pa, abundance_weighted_tl) %>% 
      group_by(hhid) %>% 
      summarise(mean_tl = mean(tl), na.rm = TRUE) %>% 
      mutate(type = "catch") 
    
### Mean total length, weighted by biomass at HOUSEHOLD CONSUMPTION level
  # household consumption proportional abundance by species
  pa_hh_cons <- c %>% 
    select(hhid, scode_ccm, atefresh_iweight) %>% 
    # get total consumption by species
    group_by(hhid, scode_ccm) %>% 
    summarise(cons_kg_species = sum(atefresh_iweight)) %>% 
    # bring in total consumption by household
    left_join(total_hh, by = "hhid") %>% 
    # calculate proportional abundance
    mutate(pa = cons_kg_species/total_cons_kg)  %>% 
    select(hhid, scode_ccm, pa)  
    
  mbs_hh_cons <- c %>% 
    left_join(ccm_traits_specieslevel, by = "scode_ccm") %>% 
    left_join(pa_hh_cons, by = c("hhid", "scode_ccm")) %>% 
    select(hhid, tl, pa) %>% 
    #drop_na(tl) %>% 
    mutate(abundance_weighted_tl = tl*pa) %>% 
    drop_na(abundance_weighted_tl) %>% 
    select(hhid, tl, pa, abundance_weighted_tl) %>% 
    group_by(hhid) %>% 
    summarise(mean_tl = mean(tl), na.rm = TRUE) %>% 
    mutate(type = "cons") 
  
  
  
## testing: 
  x <- mbs_hh_catch %>% 
    left_join(mbs_hh_cons, by = c("hhid")) %>% 
    left_join(ccm_traits_specieslevel, by = "scode_ccm") %>% 
    select(hhid, scode_ccm, pa.x, pa.y, tl) %>% 
    mutate(tlx = tl*pa.x,
           tly = tl*pa.y) %>% 
    drop_na(tl) %>% 
  group_by(hhid) %>% 
    summarise(tlx = mean(tlx),
              tly = mean(tly))
    
### Mean total length, weighted by biomass at HOUSEHOLD SOLD level
    
  # household SOLD proportional abundance by species
  pa_hh_sold <- c %>% 
    select(hhid, scode_ccm, soldfresh_iweight) %>% 
    # get total SOLD by species
    group_by(hhid, scode_ccm) %>% 
    summarise(sold_kg_species = sum(soldfresh_iweight)) %>% 
    # bring in total SOLD by household
    left_join(total_hh, by = "hhid") %>% 
    # calculate proportional abundance
    mutate(pa = sold_kg_species/total_sold_kg) %>% 
    mutate_at(vars(pa), ~replace(., is.nan(.), 0)) %>% # change all NaN values to zero
    select(hhid, scode_ccm, pa)

  mbs_hh_sold <- c %>% 
    left_join(ccm_traits_specieslevel, by = "scode_ccm") %>% 
    left_join(pa_hh_sold, by = c("hhid", "scode_ccm")) %>% 
    drop_na(tl) %>% 
    mutate(abundance_weighted_tl = tl*pa) %>% 
    select(hhid, tl, pa, abundance_weighted_tl) %>% 
    group_by(hhid) %>% 
    summarise(mean_tl = mean(tl), na.rm = TRUE) %>% 
    mutate(type = "sold") %>% 
    select(-hhid)
  
  
#### Combine files into long form for plotting
  tl <- mbs_cfr %>% 
    rbind(mbs_hh_catch) %>% 
    rbind(mbs_hh_sold) %>% 
    rbind(mbs_hh_cons) %>% 
    arrange(mean_tl)
  
  table(tl$type)
  
  
### Boxplot 
  sys <- mbs_system$mbs_system[1]
  
  
  tl %>% 
    mutate(type = factor(type, levels = c("cfr", "catch", "cons", "sold" ))) %>% 
    ggplot(aes(x = type, y = mean_tl, fill = type)) +
    geom_hline(yintercept = sys, color = "#00CCCC", size = 1) + # horizontal line corresponding to value in minbio_sys_biom
    geom_boxplot(outlier.shape = NA) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.7, name = "Portfolio type", labels = c("CFR", "Household Catch", "Household Consumption", "Household Sold")) +
    #scale_color_viridis(discrete = TRUE) +
    geom_jitter(size = 0.7, alpha = 0.4, width = 0.2, color = "black") +
    scale_y_continuous(trans = "log1p") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.caption = element_text(hjust = 0)) +
    ylab("Mean total length") +
    labs(title = "Mean (biomass-weighted) total length by portfolio type")
  
  
  
  
  
  
  
  
#------------------------------------------------------------------------------# 
#### Body size (total length) plots @ household level ####
#------------------------------------------------------------------------------# 


# eat x sell
ccm_traits_specieslevel %>% 
  ggplot(aes(x = eat, y = sell, size = tl, color = tl)) +
  geom_point() +
  scale_x_continuous(trans = "log1p") +
  scale_y_continuous(trans = "log1p") +
  scale_color_viridis() +
  theme_bw() +
  #geom_smooth(method = "lm", color = "black", show.legend = FALSE) +
  labs(size = "", colour = "Total length") +
  geom_abline(intercept = 0, slope = 1, color = "gray") +
  ggtitle("Quantity eaten X Quantity sold X Total length")

ggsave(path = "output/", "eat_x_sell_x_length_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)


# eat x sell disaggregated to household level
ccm_traits_specieslevel %>% 
  select(scode_ccm, tl, rel_abundance) %>% 
  full_join(hh_ccm, by = "scode_ccm") %>% 
  arrange(hhid) %>% # dim() 9694 x 11
  filter(sold <200) %>% #dim() 9684 x 11
  ggplot(aes(x = eat, y = sold, color = tl, size = tl)) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(trans = "log1p") +
  scale_y_continuous(trans = "log1p") +
  geom_abline(intercept = 0, slope = 1, color = "gray") +
  scale_color_viridis() +
  theme_bw() +
  labs(size = "", colour = "Total length") +
  ggtitle("Eaten X Sold X Total length (household level)") +
  xlab("Quantity eaten") +
  ylab("Quantity sold") 

ggsave(path = "output/", "eat_x_sell_x_length_hhlevel.png", width = 16, height =  12, units = "cm", dpi = 320)



# catch x eat
ccm_traits_specieslevel %>% 
  ggplot(aes(x = catch, y = eat, size = tl, color = tl)) +
  geom_point() +
  scale_x_continuous(trans = "log1p") +
  scale_y_continuous(trans = "log1p") +
  theme_bw() +
  #geom_smooth(method = "lm", color = "black", show.legend = FALSE) +
  geom_abline(intercept = 0, slope = 1, color = "gray") +
  #geom_abline(intercept = 0, slope = log1p(0.5), color = "#99FFFF") +
  labs(size = "", colour = "Total length") +
  ggtitle("Quantity caught X Quantity eaten X Total length")

ggsave(path = "output/", "catch_x_eat_x_length_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)


# catch x sell
size_catchsell <- ccm_traits_specieslevel %>% 
  ggplot(aes(x = catch, y = sell, size = tl, color = tl)) +
  geom_point() +
  scale_x_continuous(trans = "log1p") +
  scale_y_continuous(trans = "log1p") +
  theme_bw() +
  #geom_smooth(method = "lm", color = "black", show.legend = FALSE) +
  geom_abline(intercept = 0, slope = 1, color = "gray")+
  #geom_abline(intercept = 0, slope = 0.5, color = "#99FFFF") +
  labs(size = "", colour = "Total length") +
  ggtitle("Quantity caught X Quantity sold X Total length")

ggsave(path = "output/", "catch_x_sell_x_length_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)

## Body size and nutrition

size_rda <- ccm_traits_specieslevel %>% 
  ggplot(aes(x = num_rda100, y = tl)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  ggtitle("Body size X RDAs met")

# Check correlation stats
size_rda_corr <- ccm_traits_specieslevel %>% 
  ungroup() %>% 
  select(num_rda100, tl) %>% 
  drop_na() %>% 
  as.matrix() %>% 
  Hmisc::rcorr(type = "spearman") %>% 
  broom::tidy()%>% 
  knitr::kable()



