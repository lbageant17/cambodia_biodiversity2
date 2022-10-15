## Liz Bageant
## October 11, 2022

#------------------------------------------------------------------------------# 
#
#  CONSTRUCTING BODY SIZE FIGURES
#

# Need to think more about species that don't appear in portfolios. 
# There are some species that are caught by households, but never eaten. 
# There are some species that are caught but never sold.
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
    group_by(scode_biom) %>% 
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
    group_by(scode_ccm) %>% 
    summarise(biomass = sum(catch_iweight)) %>% 
    filter(biomass != 0) %>%  # if there are species that are never caught, this removes them (but there aren't)
    mutate(pa_catch = biomass/total_biomass_catch)
  # calculate mean total length of caught spcies weighted by biomass
  tl_catch <- pa_catch %>% 
    left_join(., select(ccm_traits_specieslevel, c(tl, scode_ccm)), by = "scode_ccm") %>% 
    ungroup() %>% 
    mutate(tl_weighted = tl*pa_catch) %>% 
    group_by(scode_ccm) %>% 
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
    group_by(scode_ccm) %>% 
    summarise(biomass = sum(atefresh_iweight)) %>%
    filter(biomass != 0) %>%  # remove species that are not eaten (655 cases)
    mutate(pa_cons = biomass/total_biomass_cons) 
  # calculate mean total length of consumed species weighted by biomass
  tl_cons <- pa_cons %>% 
    left_join(., select(ccm_traits_specieslevel, c(tl, scode_ccm)), by = "scode_ccm") %>% 
    ungroup() %>% 
    mutate(tl_weighted = tl*pa_cons) %>% 
    group_by(scode_ccm) %>% 
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
    group_by(scode_ccm) %>% 
    summarise(biomass = sum(soldfresh_iweight)) %>% 
    filter(biomass != 0) %>%  # removing all cases where certain species were never sold. 
    mutate(pa_sold = biomass/total_biomass_sold)
  # calculate mean total length of consumed species weighted by biomass
  tl_sold <- pa_sold %>% 
    left_join(., select(ccm_traits_specieslevel, c(tl, scode_ccm)), by = "scode_ccm") %>% 
    ungroup() %>% 
    mutate(tl_weighted = tl*pa_sold) %>% 
    group_by(scode_ccm) %>% 
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
    #scale_y_continuous(trans = "log1p") +
    #coord_trans(y = "log1p") +
    scale_y_log10() +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.caption = element_text(hjust = 0)) +
    ylab("Mean total length") +
    labs(title = "Mean (biomass-weighted) total length by portfolio type")
  
