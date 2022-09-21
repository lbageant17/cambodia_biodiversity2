## Liz Bageant
## September 21, 2022

#------------------------------------------------------------------------------# 
#
#  CONSTRUCTING DIVERSITY INDICES
#
#------------------------------------------------------------------------------#


#### ----- Set up data files for diversity index calculations ------ #### 

# We need a data file with vectors of species and biomass. 
# We need this for biomonitoring, catch, and consumption data. 

# biomonitoring @ cfr level
biom_div_data <- b %>% 
  select(cfrid, scode_biom, totalweight_biom) %>% 
  # collapse to CFR level (dropping time dimension)
  group_by(cfrid, scode_biom) %>% 
  summarise(biomass = sum(totalweight_biom)) %>% 
  # reshape wide by species
  pivot_wider(names_from = "scode_biom", values_from = "biomass") %>% 
  # replace NA values with zero--these are species that weren't present in a given CFR's biomonitoring
  mutate_all(~replace(., is.na(.), 0))


# catch @ cfr level
catch_div_data_cfr <- c %>% 
  select(hhid, cfrid, scode_ccm, catch_iweight) %>% 
  # collapse to CFR level 
  group_by(cfrid, scode_ccm) %>% 
  summarise(biomass = sum(catch_iweight)) %>% 
  # reshape
  pivot_wider(names_from = "scode_ccm", values_from = "biomass") %>% 
  # replace NA values with zero
  mutate_all(~replace(., is.na(.), 0))

# catch @ household level
catch_div_data_hh <- c %>% 
  select(hhid, hhid, scode_ccm, catch_iweight) %>% 
  # collapse to CFR level 
  group_by(hhid, scode_ccm) %>% 
  summarise(biomass = sum(catch_iweight)) %>% 
  # reshape
  pivot_wider(names_from = "scode_ccm", values_from = "biomass") %>% 
  # replace NA values with zero
  mutate_all(~replace(., is.na(.), 0)) 

# consumption @ cfr level--not sure if this is necessary
cons_div_data_cfr <- c %>% 
  select(cfrid, scode_ccm, atefresh_iweight) %>% 
  # collapse to CFR level 
  group_by(cfrid, scode_ccm) %>% 
  summarise(biomass = sum(atefresh_iweight)) %>% 
  # reshape
  pivot_wider(names_from = "scode_ccm", values_from = "biomass") %>% 
  # replace NA values with zero
  mutate_all(~replace(., is.na(.), 0)) 

# consumption @ hh level
cons_div_data_hh <- c %>% 
  select(hhid, hhid, scode_ccm, atefresh_iweight) %>% 
  # collapse to CFR level 
  group_by(hhid, scode_ccm) %>% 
  summarise(biomass = sum(atefresh_iweight)) %>% 
  # reshape
  pivot_wider(names_from = "scode_ccm", values_from = "biomass") %>% 
  # replace NA values with zero
  mutate_all(~replace(., is.na(.), 0)) 

# sold @ cfr level--not sure if this is necessary
sold_div_data_cfr <- c %>% 
  select(cfrid, scode_ccm, soldfresh_iweight) %>% 
  # collapse to CFR level 
  group_by(cfrid, scode_ccm) %>% 
  summarise(biomass = sum(soldfresh_iweight)) %>% 
  # reshape
  pivot_wider(names_from = "scode_ccm", values_from = "biomass") %>% 
  # replace NA values with zero
  mutate_all(~replace(., is.na(.), 0)) 

# sold @ hh level
sold_div_data_hh <- c %>% 
  select(hhid, cfrid, scode_ccm, soldfresh_iweight) %>% 
  # collapse to CFR level 
  group_by(hhid, scode_ccm) %>% 
  summarise(biomass = sum(soldfresh_iweight)) %>% 
  # reshape
  pivot_wider(names_from = "scode_ccm", values_from = "biomass") %>% 
  # replace NA values with zero
  mutate_all(~replace(., is.na(.), 0)) 



#### ----- Calculate diversity indices ------ #### 

# Biomonitoring (system) diversity @ CFR level
    
    # Shannon Index (H)
    temp <- biom_div_data %>% 
      ungroup() %>% 
      select(-cfrid) %>% 
      diversity() %>% 
      # adding back CFR IDs
      cbind(biom_div_data$cfrid) %>% 
      as.data.frame()
    
    h_biom_cfr <- temp %>% 
      mutate(h_biom = as.numeric(.),
             cfrid = V2) %>% 
      select(h_biom, cfrid)
    
    # Simpson index (D1)
    temp <- biom_div_data %>% 
      ungroup() %>% 
      select(-cfrid) %>% 
      diversity("simpson") %>% 
      # adding back CFR IDs
      cbind(biom_div_data$cfrid) %>% 
      as.data.frame()
    
    d1_biom_cfr <- temp %>% 
      mutate(d1_biom = as.numeric(.),
             cfrid = V2) %>% 
      select(d1_biom, cfrid)
    
    
    # Inverse simpson index (D2)
    temp <- biom_div_data %>% 
      ungroup() %>% 
      select(-cfrid) %>% 
      diversity("invsimpson") %>% 
      # adding back CFR IDs
      cbind(biom_div_data$cfrid) %>% 
      as.data.frame()
    
    d2_biom_cfr <- temp %>% 
      mutate(d2_biom = as.numeric(.),
             cfrid = V2) %>% 
      select(d2_biom, cfrid)
    
    # combine files
    indices_biom_cfr <- h_biom_cfr %>% 
      left_join(d1_biom_cfr) %>% 
      left_join(d2_biom_cfr) 


# Catch diversity @ CFR level

    # Shannon Index (H)
    temp <- catch_div_data_cfr %>% 
      ungroup() %>% 
      select(-cfrid) %>% 
      diversity() %>% 
      # adding back CFR IDs
      cbind(catch_div_data_cfr$cfrid) %>% 
      as.data.frame()
    
    h_catch_cfr <- temp %>% 
      mutate(h_catch = as.numeric(.),
             cfrid = V2) %>% 
      select(h_catch, cfrid)
    
    # Simpson index (D1)
    temp <- catch_div_data_cfr %>% 
      ungroup() %>% 
      select(-cfrid) %>% 
      diversity("simpson") %>% 
      # adding back CFR IDs
      cbind(catch_div_data_cfr$cfrid) %>% 
      as.data.frame()
    
    d1_catch_cfr <- temp %>% 
      mutate(d1_catch = as.numeric(.),
             cfrid = V2) %>% 
      select(d1_catch, cfrid)
    
    # Inverse simpson index (D2)
    temp <- catch_div_data_cfr %>% 
      ungroup() %>% 
      select(-cfrid) %>% 
      diversity("invsimpson") %>% 
      # adding back CFR IDs
      cbind(catch_div_data_cfr$cfrid) %>% 
      as.data.frame()
    
    d2_catch_cfr <- temp %>% 
      mutate(d2_catch = as.numeric(.),
             cfrid = V2) %>% 
      select(d2_catch, cfrid)
    
    # combine files
    indices_catch_cfr <- h_catch_cfr %>% 
      left_join(d1_catch_cfr, by = "cfrid") %>% 
      left_join(d2_catch_cfr, by = "cfrid")
 
       
# Catch diversity @ HH level
    
    # Shannon Index (H)
    temp <- catch_div_data_hh %>% 
      ungroup() %>% 
      select(-hhid) %>% 
      diversity() %>% 
      # adding back CFR IDs
      cbind(catch_div_data_hh$hhid) %>% 
      as.data.frame()
    
    h_catch_hh <- temp %>% 
      mutate(h_catch = as.numeric(.),
             hhid = V2) %>% 
      select(h_catch, hhid)
    
    # Simpson index (D1)
    temp <- catch_div_data_hh %>% 
      ungroup() %>% 
      select(-hhid) %>% 
      diversity("simpson") %>% 
      # adding back CFR IDs
      cbind(catch_div_data_hh$hhid) %>% 
      as.data.frame()
    
    d1_catch_hh <- temp %>% 
      mutate(d1_catch = as.numeric(.),
             hhid = V2) %>% 
      select(d1_catch, hhid)
    
    # Inverse simpson index (D2)
    temp <- catch_div_data_hh %>% 
      ungroup() %>% 
      select(-hhid) %>% 
      diversity("invsimpson") %>% 
      # adding back CFR IDs
      cbind(catch_div_data_hh$hhid) %>% 
      as.data.frame()
    
    d2_catch_hh <- temp %>% 
      mutate(d2_catch = as.numeric(.),
             hhid = V2) %>% 
      select(d2_catch, hhid)
    
    # combine files
    indices_catch_hh <- h_catch_hh %>% 
      left_join(d1_catch_hh, by = "hhid") %>% 
      left_join(d2_catch_hh, by = "hhid")
    
    
## Consumption diversity at the HH level
    
    # Shannon Index (H)
    temp <- cons_div_data_hh %>% 
      ungroup() %>% 
      select(-hhid) %>% 
      diversity() %>% 
      # adding back CFR IDs
      cbind(cons_div_data_hh$hhid) %>% 
      as.data.frame()
    
    h_cons_hh <- temp %>% 
      mutate(h_cons = as.numeric(.),
             hhid = V2) %>% 
      select(h_cons, hhid)
    
    # Simpson index (D1)
    temp <- cons_div_data_hh %>% 
      ungroup() %>% 
      select(-hhid) %>% 
      diversity("simpson") %>% 
      # adding back CFR IDs
      cbind(cons_div_data_hh$hhid) %>% 
      as.data.frame()
    
    d1_cons_hh <- temp %>% 
      mutate(d1_cons = as.numeric(.),
             hhid = V2) %>% 
      select(d1_cons, hhid)
    
    # Inverse simpson index (D2)
    temp <- cons_div_data_hh %>% 
      ungroup() %>% 
      select(-hhid) %>% 
      diversity("invsimpson") %>% 
      # adding back CFR IDs
      cbind(cons_div_data_hh$hhid) %>% 
      as.data.frame()
    
    d2_cons_hh <- temp %>% 
      mutate(d2_cons = as.numeric(.),
             hhid = V2) %>% 
      select(d2_cons, hhid)
    
    # combine files
    indices_cons_hh <- h_cons_hh %>% 
      left_join(d1_cons_hh, by = "hhid") %>% 
      left_join(d2_cons_hh, by = "hhid")
    

## Consumption diversity at the CFR level
    
    # Shannon Index (H)
    temp <- cons_div_data_cfr %>% 
      ungroup() %>% 
      select(-cfrid) %>% 
      diversity() %>% 
      # adding back CFR IDs
      cbind(cons_div_data_cfr$cfrid) %>% 
      as.data.frame()
    
    h_cons_cfr <- temp %>% 
      mutate(h_cons = as.numeric(.),
             cfrid = V2) %>% 
      select(h_cons, cfrid)
    
    # Simpson index (D1)
    temp <- cons_div_data_cfr %>% 
      ungroup() %>% 
      select(-cfrid) %>% 
      diversity("simpson") %>% 
      # adding back CFR IDs
      cbind(cons_div_data_cfr$cfrid) %>% 
      as.data.frame()
    
    d1_cons_cfr <- temp %>% 
      mutate(d1_cons = as.numeric(.),
             cfrid = V2) %>% 
      select(d1_cons, cfrid)
    
    # Inverse simpson index (D2)
    temp <- cons_div_data_cfr %>% 
      ungroup() %>% 
      select(-cfrid) %>% 
      diversity("invsimpson") %>% 
      # adding back CFR IDs
      cbind(cons_div_data_cfr$cfrid) %>% 
      as.data.frame()
    
    d2_cons_cfr <- temp %>% 
      mutate(d2_cons = as.numeric(.),
             cfrid = V2) %>% 
      select(d2_cons, cfrid)
    
    # combine files
    indices_cons_cfr <- h_cons_cfr %>% 
      left_join(d1_cons_cfr, by = "cfrid") %>% 
      left_join(d2_cons_cfr, by = "cfrid")   
    
## Sold diversity at the HH level
    
    # Shannon Index (H)
    temp <- sold_div_data_hh %>% 
      ungroup() %>% 
      select(-hhid) %>% 
      diversity() %>% 
      # adding back CFR IDs
      cbind(sold_div_data_hh$hhid) %>% 
      as.data.frame()
    
    h_sold_hh <- temp %>% 
      mutate(h_sold = as.numeric(.),
             hhid = V2) %>% 
      select(h_sold, hhid)
    
    # Simpson index (D1)
    temp <- sold_div_data_hh %>% 
      ungroup() %>% 
      select(-hhid) %>% 
      diversity("simpson") %>% 
      # adding back CFR IDs
      cbind(sold_div_data_hh$hhid) %>% 
      as.data.frame()
    
    d1_sold_hh <- temp %>% 
      mutate(d1_sold = as.numeric(.),
             hhid = V2) %>% 
      select(d1_sold, hhid)
    
    # Inverse simpson index (D2)
    temp <- sold_div_data_hh %>% 
      ungroup() %>% 
      select(-hhid) %>% 
      diversity("invsimpson") %>% 
      # adding back CFR IDs
      cbind(sold_div_data_hh$hhid) %>% 
      as.data.frame()
    
    d2_sold_hh <- temp %>% 
      mutate(d2_sold = as.numeric(.),
             hhid = V2) %>% 
      select(d2_sold, hhid)
    
    # combine files
    indices_sold_hh <- h_sold_hh %>% 
      left_join(d1_sold_hh, by = "hhid") %>% 
      left_join(d2_sold_hh, by = "hhid")

## Sold diversity at the CFR level
    
    # Shannon Index (H)
    temp <- sold_div_data_cfr %>% 
      ungroup() %>% 
      select(-cfrid) %>% 
      diversity() %>% 
      # adding back CFR IDs
      cbind(sold_div_data_cfr$cfrid) %>% 
      as.data.frame()
    
    h_sold_cfr <- temp %>% 
      mutate(h_sold = as.numeric(.),
             cfrid = V2) %>% 
      select(h_sold, cfrid)
    
    # Simpson index (D1)
    temp <- sold_div_data_cfr %>% 
      ungroup() %>% 
      select(-cfrid) %>% 
      diversity("simpson") %>% 
      # adding back CFR IDs
      cbind(sold_div_data_cfr$cfrid) %>% 
      as.data.frame()
    
    d1_sold_cfr <- temp %>% 
      mutate(d1_sold = as.numeric(.),
             cfrid = V2) %>% 
      select(d1_sold, cfrid)
    
    # Inverse simpson index (D2)
    temp <- sold_div_data_cfr %>% 
      ungroup() %>% 
      select(-cfrid) %>% 
      diversity("invsimpson") %>% 
      # adding back CFR IDs
      cbind(sold_div_data_cfr$cfrid) %>% 
      as.data.frame()
    
    d2_sold_cfr <- temp %>% 
      mutate(d2_sold = as.numeric(.),
             cfrid = V2) %>% 
      select(d2_sold, cfrid)
    
    # combine files
    indices_sold_cfr <- h_sold_cfr %>% 
      left_join(d1_sold_cfr, by = "cfrid") %>% 
      left_join(d2_sold_cfr, by = "cfrid")
    
    
#### ----- Create CFR and HH level combined data files ------ ####     
    
  ## CFR-level file
  diversity_indices_cfr_level <- indices_biom_cfr %>% 
        left_join(indices_catch_cfr, by = "cfrid") %>% 
        left_join(indices_cons_cfr, by = "cfrid") %>% 
        left_join(indices_sold_cfr, by = "cfrid") %>% 
        relocate(cfrid)
      
  
  ## HH-level file
      diversity_indices_hh_level <- indices_catch_hh %>% 
        left_join(indices_cons_hh, by = "hhid") %>% 
      left_join(indices_sold_hh, by = "hhid") %>% 
      relocate(hhid)


#### ----- Plot some things! ------ ####     

#### System diversity vs catch diversity ####
      
  ## @ CFR level
      diversity_indices_cfr_level %>% 
        ggplot(aes(x = h_catch, y = h_biom)) +
        geom_point(size = 2, alpha = 0.6) +
        geom_smooth(method = "lm", color = "#008B8B") +
        theme_bw() +
        ggtitle("Shannon Index: System diversity X Catch diversity") +
        xlab("Biomonitoring Shannon index (by CFR)") +
        ylab("Catch Shannon index (by CFR)")
      
      ggsave(path = "output/", "shannon_biom_x_catch_cfr.png", width = 16, height =  12, units = "cm", dpi = 320)
      
      # Check correlation stats
      diversity_indices_cfr_level %>% 
        select(h_catch, h_biom) %>% 
        as.matrix() %>% 
        Hmisc::rcorr(type = "spearman") %>% 
        broom::tidy()
      
  ## @ HH level
    data <- diversity_indices_hh_level %>% 
      select(hhid, contains(c("biom", "catch"))) %>% 
      left_join(cfrid_to_hhid, by = "hhid") %>% 
      left_join(diversity_indices_cfr_level, by = "cfrid") %>% 
      select(hhid, cfrid, h_biom, h_catch.x) 
      
    data %>% 
      ggplot(aes(x = h_catch.x, y = h_biom)) +
      geom_point(size = 2, alpha = 0.6) +
      geom_smooth(method = "lm", color = "#008B8B") +
      theme_bw() +
      ggtitle("Shannon Index: System diversity X Catch diversity (HH level") +
      xlab("Catch Shannon index (by household)") +
      ylab("Biomonitoring Shannon index (by CFR)")
      
    ggsave(path = "output/", "shannon_biom_x_catch_hh.png", width = 16, height =  12, units = "cm", dpi = 320)
    
    # Check correlation stats
    data %>% 
      select(h_catch.x, h_biom) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") %>% 
      broom::tidy()

#### Catch diversity vs consumption diversity ####
    
  ## @ HH level
    diversity_indices_hh_level %>% 
      ggplot(aes(x = h_catch, y = h_cons)) +
      geom_point(size = 2, alpha = 0.6) +
      geom_smooth(method = "lm", color = "#008B8B") +
      theme_bw() +
      ggtitle("Shannon Index: Catch diversity X Consumption diversity") +
      xlab("Catch Shannon index (by HH)") +
      ylab("Consumption Shannon index (by HH)")
    
    ggsave(path = "output/", "shannon_catch_x_cons_hh.png", width = 16, height =  12, units = "cm", dpi = 320)
  
    # Check correlation stats
    diversity_indices_hh_level %>% 
      select(h_catch, h_cons) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") %>% 
      broom::tidy() 
    
#### Catch diversity vs sold diversity #### 
    
  ## @ HH level among all households
    diversity_indices_hh_level %>% 
      ggplot(aes(x = h_catch, y = h_sold)) +
      geom_point(size = 2, alpha = 0.6) +
      geom_smooth(method = "lm", color = "#008B8B") +
      theme_bw() +
      ggtitle("Shannon Index: Catch diversity X Sold diversity (all households)") +
      xlab("Catch Shannon index (by HH)") +
      ylab("Sold Shannon index (by HH)")
    
    ggsave(path = "output/", "shannon_catch_x_sold_hh.png", width = 16, height =  12, units = "cm", dpi = 320)
    
    # Check correlation stats
    diversity_indices_hh_level %>% 
      select(h_catch, h_sold) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") %>% 
      broom::tidy() 
    
  ## @ HH level among only households that sold fish
    diversity_indices_hh_level %>% 
      filter(h_sold > 0) %>% 
      ggplot(aes(x = h_catch, y = h_sold)) +
      geom_point(size = 2, alpha = 0.6) +
      geom_smooth(method = "lm", color = "#008B8B") +
      theme_bw() +
      ggtitle("Shannon Index: Catch diversity X Sold diversity (households with non-zero fish sales)") +
      xlab("Catch Shannon index (by HH)") +
      ylab("Sold Shannon index (by HH)")
    
    ggsave(path = "output/", "shannon_catch_x_sold_hh2.png", width = 16, height =  12, units = "cm", dpi = 320)
    
    # Check correlation stats
    diversity_indices_hh_level %>% 
      select(h_catch, h_sold) %>% 
      filter(h_sold > 0) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") %>% 
      broom::tidy() 
    