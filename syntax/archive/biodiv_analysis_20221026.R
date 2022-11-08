## Liz Bageant
## September 19, 2022


#------------------------------------------------------------------------------# 
#
#  CAMBODIA BIODIVERSITY PAPER ANALYSIS
#
#------------------------------------------------------------------------------# 

## NOTE: All libraries and data prep can be found in _MASTER.R file.


#------------------------------------------------------------------------------# 
#### Number-of-species plots ####
#------------------------------------------------------------------------------# 

### Biomonitoring X Catch ####

# Plot system diversity and catch diversity
  cfr_biodiv %>% 
    ggplot(aes(x = biom_species, y = catch_species)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "#008B8B") +
    theme_bw() +
    ggtitle("Number of species: Biomonitoring X Catch") +
    xlab("Number of species in biomonitoring (CFR level") +
    ylab("Number of species caught (HH level)")
  
  ggsave(path = "output/", "numspecies_biom_x_catch.png", width = 16, height =  12, units = "cm", dpi = 320)
  
  
  # Check correlation stats
  cfr_biodiv %>% 
    select(biom_species, catch_species) %>% 
    as.matrix() %>% 
    Hmisc::rcorr(type = "spearman") %>% 
    broom::tidy() 


  # Add effort as an additional dimension
  cfr_biodiv %>% 
    full_join(effort, by = "hhid") %>% 
    ggplot(aes(x = biom_species, y = catch_species, color = effort, size = effort)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", formula = y ~poly(x, 4), color = "#008B8B", show.legend = FALSE) +
    #geom_smooth(method = "lm", formula = y ~poly(x, 4), color = "#008B8B", show.legend = FALSE) +
    scale_color_viridis() + 
    theme_bw() +
    ggtitle("Number of species: Biomonitoring X Catch X Effort") +
    xlab("Number of species in biomonitoring") +
    ylab("Number of species caught")
  
  ggsave(path = "output/", "numspecies_biom_x_catch_x_effort.png", width = 16, height =  12, units = "cm", dpi = 320)


    # Sidenote: What is the relationship between effort and diversity of species caught
    effort2 <- cfr_biodiv %>% 
      full_join(effort, by = "hhid") %>% 
      filter(effort < 150) %>%  # filter out person-days > 150
      ggplot(aes(x = catch_species, y = effort)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", color = "#008B8B") +
      #geom_smooth(method = "lm", formula = y ~ poly(x, 5), se = TRUE) +
      theme_bw() +
      ggtitle("Relationship between effort and number of species caught") +
      ylab("Effort (person-days in past 7 days)") +
      xlab("Number of species caught in past 7 days")

    # look at correlation
    cfr_biodiv %>% 
      full_join(effort, by = "hhid") %>% 
      # filter out person-days > 150
      filter(effort < 150) %>% 
      select(effort, catch_species) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") 

    
  # Biomonitoring and sold
    cfr_biodiv %>% 
      ggplot(aes(x = biom_species, y = sold_species)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", color = "#008B8B") +
      theme_bw() +
      ggtitle("Number of species: Biomonitoring X Sold") +
      xlab("Number of species in biomonitoring (CFR level") +
      ylab("Number of species sold (HH level)")
    
    ggsave(path = "output/", "numspecies_biom_x_sold.png", width = 16, height =  12, units = "cm", dpi = 320)
    
    
    # Check correlation stats
    cfr_biodiv %>% 
      select(biom_species, sold_species) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") %>% 
      broom::tidy() 
  
    
#### Catch X Consumption ####
    
    # Plot
    cfr_biodiv %>% 
      ggplot(aes(x = catch_species, y = consumption_species)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", color = "#008B8B") + 
      #geom_smooth(method = "lm", formula = y ~ poly(x, 5), se = TRUE) +
      #geom_abline(intercept = 0, slope = 1, color = "gray") +
      theme_bw() +
      ggtitle("Number of species: Catch X Consumption") +
      xlab("Number of species caught") +
      ylab("Number of species consumed")
    
    ggsave(path = "output/", "numspecies_catch_x_cons.png", width = 16, height =  12, units = "cm", dpi = 320)

    
    # Check correlation stats
    cfr_biodiv %>% 
      select(catch_species, consumption_species) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") %>% 
      broom::tidy() 
    
  
    
    # Add effort as a third dimension
    cfr_biodiv %>% 
      full_join(effort, by = "hhid") %>% 
      ggplot(aes(x = catch_species, y = consumption_species, color = effort, size = effort)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", color = "#008B8B", show.legend = FALSE) +
      scale_color_viridis() + 
      theme_bw() +
      geom_abline(intercept = 0, slope = 1, color = "gray") +
      ggtitle("Number of species: Catch X Consumption X Effort") +
      xlab("Number of species caught") +
      ylab("Number of species consumed")
    
    ggsave(path = "output/", "numspecies_catch_x_cons_x_effort.png", width = 16, height =  12, units = "cm", dpi = 320)
    
    
    #### PLACEHOLDER ####
    # [PLACEHOLDER FOR SOME DIVERSITY-INDEX BASED LOOK AT THE RELATIONSHIP BETWEEN CATCH AND CONSUMPTION --expecting this to be a very strong relationship]
    
    
    
#### Catch X Sold ####
    
    # Plot
    cfr_biodiv %>% 
      ggplot(aes(x = catch_species, y = sold_species)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", color = "#008B8B") +  
      #geom_abline(intercept = 0, slope = 1, color = "gray") +
      theme_bw() +
      ggtitle("Number of species: Catch X Sold") +
      xlab("Number of species caught") +
      ylab("Number of species sold")
    
    ggsave(path = "output/", "numspecies_catch_x_sold.png", width = 16, height =  12, units = "cm", dpi = 320)
    
    # Check correlation stats
    cfr_biodiv %>% 
      select(catch_species, sold_species) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") %>% 
      broom::tidy() 
    
    #### PLACEHOLDER ####
    # [PLACEHOLDER FOR SOME DIVERSITY-INDEX BASED LOOK AT THE RELATIONSHIP BETWEEN CATCH AND SOLD]   
    
    
    
#### Consumption X RDAs ####   
    
    eat_rda <- hh_ccm %>% 
      select(hhid, scode_ccm) %>% 
      full_join(ccm_traits_specieslevel, by = "scode_ccm") %>% 
      drop_na(pr_rda100) %>% 
      filter(eat > 0) %>%  # drops one observation where household caught something but did not eat it
      group_by(hhid) %>% 
      # calculate whether 100g of any of a household's species caught provides the RDA for each nutrient--THIS WILL NEED TO BE WEIGHTED BY ABUNDANCE
      summarise(species_count = n(),
                pr_rda100 = max(pr_rda100),
                vita_rda100 = max(vita_rda100),
                fe_rda100 = max(fe_rda100),
                o3_rda100 = max(o3_rda100),
                zn_rda100 = max(zn_rda100),
                ca_rda100 = max(ca_rda100)) %>% 
      # calculate RDAs met by 100g
      mutate(rda_met100 = pr_rda100 + vita_rda100 + fe_rda100 + o3_rda100 + zn_rda100 + ca_rda100) %>% 
      drop_na(rda_met100)
    
    # Check correlation stats
    eat_rda %>% 
      select(species_count, rda_met100) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") %>% 
      broom::tidy() 
    
    # Plot
    
    # scatter
    eat_rda_scatter <- eat_rda %>% 
      ggplot(aes(x = rda_met100, y = species_count)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", color = "#008B8B") +
      theme_bw() +
      ggtitle("Relationship between number of species consumed and RDAs met")
    
    # boxplot
    eat_rda %>% 
      mutate(RDAs = as.factor(rda_met100)) %>% 
      ggplot(aes(y = RDAs, x = species_count, fill = RDAs)) +
      geom_boxplot() +
      geom_jitter(size = 1, alpha = 0.5, height = 0.1) +
      scale_fill_viridis(discrete = TRUE, alpha = 0.6) + 
      theme_bw() +
      ggtitle("Number of species consumed X RDAs met") +
      ylab("Number of species consumed") +
      xlab("RDAs met by species portfolio (not weighted)")
    ggsave(path = "output/", "cons_x_rda_boxplot_UNWEIGHTED.png", width = 16, height =  12, units = "cm", dpi = 320)

    
#### Sold X RDAs ####   
    
    sold_rda <- hh_ccm %>% 
      select(hhid, scode_ccm) %>% 
      full_join(ccm_traits_specieslevel, by = "scode_ccm") %>% #dim() 9694 x 19
      drop_na(pr_rda100) %>% # dim() 7677 x 19
      filter(sell > 0) %>% # dim() 7537 x 19
      group_by(hhid) %>% 
      # calculate whether 100g of any of a household's species caught provides the RDA for each nutrient--THIS WILL NEED TO BE WEIGHTED BY ABUNDANCE
      summarise(species_count = n(),
                pr_rda100 = max(pr_rda100),
                vita_rda100 = max(vita_rda100),
                fe_rda100 = max(fe_rda100),
                o3_rda100 = max(o3_rda100),
                zn_rda100 = max(zn_rda100),
                ca_rda100 = max(ca_rda100)) %>% 
      mutate(rda_met100 = pr_rda100 + vita_rda100 + fe_rda100 + o3_rda100 + zn_rda100 + ca_rda100) %>% 
      drop_na(rda_met100)
    
    # scatter
    sold_rda_scatter <- sold_rda %>% 
      ggplot(aes(x = rda_met100, y = species_count)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", color = "#008B8B") +
      theme_bw() +
      ggtitle("Relationship between number of species sold and RDAs met")
    
    # Check correlation stats
    sold_rda %>% 
      select(species_count, rda_met100) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") %>% 
      broom::tidy() 
    
    # boxplot
     sold_rda %>% 
      mutate(RDAs = as.factor(rda_met100)) %>% 
      ggplot(aes(y = RDAs, x = species_count, fill = RDAs)) +
      geom_boxplot() +
      geom_jitter(size = 1, alpha = 0.5, height = 0.1) +
      scale_fill_viridis(discrete = TRUE, alpha = 0.6) + 
      theme_bw() +
      ggtitle("Number of species sold and RDAs met") +
      ylab("Number of species sold") +
      xlab("RDAs met by species portfolio (not weighted)")
    
    ggsave(path = "output/", "sold_x_rda_boxplot_UNWEIGHTED.png", width = 16, height =  12, units = "cm", dpi = 320)
  
    
#------------------------------------------------------------------------------# 
#### Relative abundance plots ####
    
    # Need to confirm the level at which we should be calculating relative abundance
    # For example, we could calculate it at system level, CFR level or household catch level.
#------------------------------------------------------------------------------# 
 
    
#### EATEN X SOLD X RELATIVE ABUNDANCE #### 
    
  # eat x sell
  # This is at the species level
  # Relative abundance is calculated at the system level using biomonitoring data.
  ccm_traits_specieslevel %>% 
    ggplot(aes(x = eat, y = sell, color = rel_abundance, size = rel_abundance)) +
    geom_point(alpha = 0.7) +
    scale_color_viridis() + theme_bw() +
    scale_x_continuous(trans = "log1p") +
    scale_y_continuous(trans = "log1p") +
    #geom_smooth(method = "lm", color = "black", show.legend = FALSE) +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    ggtitle("Quantity eaten X Quantity sold X Relative abundance (species level)") +
      ylab("Quantity sold") +
      xlab("Quantity eaten") +
      labs(size = " ", colour = " Relative abundance \nat system level ")
    
  ggsave(path = "output/", "cons_x_sold_x_abund_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)
 
  # Check correlation stats
  ccm_traits_specieslevel %>% 
    ungroup() %>% 
    mutate(total = eat + sell,
           share_eaten = eat/total) %>% 
    select(rel_abundance, share_eaten) %>% 
    drop_na(rel_abundance) %>% 
    as.matrix() %>% 
    Hmisc::rcorr(type = "spearman") %>% 
    broom::tidy() 
  
  ccm_traits_specieslevel %>% 
    ungroup() %>% 
    mutate(total = eat + sell,
           share_eaten = eat/total) %>% 
    select(rel_abundance, share_eaten) %>%
    ggplot(aes(x = rel_abundance, y = share_eaten)) +
    geom_smooth()
  
  # eat x sell 
  # This is at the household level
  # Relative abundance is calculated at the system level using biomonitoring data
  ccm_traits_specieslevel %>% 
    select(scode_ccm, rel_abundance) %>% 
    full_join(hh_ccm, by = "scode_ccm") %>% 
    filter(sold < 200) %>% 
    ggplot(aes(x = eat, y = sold, color = log(rel_abundance+1), size = log(rel_abundance+1))) +
    geom_point(alpha = 0.6) +
    scale_x_continuous(trans = "log1p") +
    scale_y_continuous(trans = "log1p") +
    scale_color_viridis() +
    theme_bw() +
    ggtitle("Eaten X Sold X Relative abundance (household level)") +
    xlab("Quantity eaten") +
    ylab("Quantity sold") +
    labs(size = "", color = " Relative abundance \nat system level")
  
  # check correlation stats
  ccm_traits_specieslevel %>% 
    select(scode_ccm, rel_abundance) %>% 
    full_join(hh_ccm, by = "scode_ccm") %>% 
    filter(sold < 200) %>%   
    ungroup() %>% 
    mutate(total = eat + sold,
           share_eaten = eat/total) %>% 
    select(rel_abundance, share_eaten) %>% 
    drop_na(rel_abundance) %>% 
    as.matrix() %>% 
    Hmisc::rcorr(type = "spearman") %>% 
    broom::tidy()  
  
  ccm_traits_specieslevel %>% 
    select(scode_ccm, rel_abundance) %>% 
    full_join(hh_ccm, by = "scode_ccm") %>% 
    filter(sold < 200) %>%   
    ungroup() %>% 
    mutate(total = eat + sold,
           share_eaten = eat/total) %>% 
    select(rel_abundance, share_eaten) %>% 
    ggplot(aes(x = rel_abundance, y = share_eaten)) +
    geom_smooth() +
    ggtitle("System level relative abundance and proportion of catch that is eaten") +
    xlab("System level relative abundance") +
    ylab("Share eaten")
  
  
  # eat x sell
  # This is at the household level. 
  # Relative abundance is calculated at the CFR level using biom data
  
    # cfr total biomass
    total_cfr_biom <- b %>% 
      group_by(cfrid) %>% 
      summarise(total_cfr_catch = sum(totalweight_biom))
  
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
      # Confirm that proportional abundances total to 1 for each CFR
      # group_by(cfrid) %>% 
      # summarise(should_equal_1 = sum(pa)) 
      #table(pa_hh_sold$should_equal_1) 
      select(cfrid, scode_biom, pa)
    
    # plot
    hh_ccm %>% 
      left_join(cfrid_to_hhid, by = "hhid") %>% # to facilitate join with pa_cfr
      mutate(scode_biom = scode_ccm) %>% # to facilitate join with pa_cfr
      left_join(pa_cfr, by = c("cfrid", "scode_biom")) %>% 
      arrange(pa) %>% # dim() #9694 x 12
      filter(sold < 200) %>% 
      ggplot(aes(x = eat, y = sold, color = log(pa+1), size = log(pa+1))) +
      geom_point(alpha = 0.7) +
      scale_x_continuous(trans = "log1p") +
      scale_y_continuous(trans = "log1p") +
      scale_color_viridis() +
      theme_bw() +
      theme(plot.caption = element_text(hjust = 0)) +
      ggtitle("Eaten X Sold X Relative abundance (Household level)") +
      xlab("Quantity eaten") +
      ylab("Quantity sold") + 
      labs(size = "", colour = "Relative abundance \nat CFR level")
   
    # Check correlation stats
    hh_ccm %>% 
      left_join(cfrid_to_hhid, by = "hhid") %>% # to facilitate join with pa_cfr
      mutate(scode_biom = scode_ccm) %>% # to facilitate join with pa_cfr
      left_join(pa_cfr, by = c("cfrid", "scode_biom"))  %>% 
      filter(sold < 200) %>% 
      ungroup() %>% 
      mutate(total = eat + sold,
             share_eaten = eat/total) %>% 
      select(pa, share_eaten) %>% 
      drop_na(pa) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") %>% 
      broom::tidy()  
        # Relationship is negative but driven by the lower end of the pa distribution (see plot below)
    
    hh_ccm %>% 
      left_join(cfrid_to_hhid, by = "hhid") %>% # to facilitate join with pa_cfr
      mutate(scode_biom = scode_ccm) %>% # to facilitate join with pa_cfr
      left_join(pa_cfr, by = c("cfrid", "scode_biom"))  %>% 
      filter(sold < 200) %>% 
      ungroup() %>% 
      mutate(total = eat + sold,
             share_eaten = eat/total) %>% 
      select(pa, share_eaten) %>% 
      ggplot(aes(x = pa, y = share_eaten)) +
      geom_smooth() +
      ggtitle("CFR level relative abundance and proportion of catch that is eaten") +
      xlab("CFR level relative abundance") +
      ylab("Share eaten")

   
  # eat x sell
  # this is at the household level
  # Relative abundance is calculated at household catch level
    
    # hh level total biomass
    total_hh <- c %>% 
      group_by(hhid) %>% 
      summarise(total_catch_kg = sum(catch_iweight))
  
    # Calculate household catch proportional abundance by species
    pa_hh_catch <- c %>% 
      select(hhid, scode_ccm, catch_iweight) %>% 
      # get total catch by species
      group_by(hhid, scode_ccm) %>% 
      summarise(catch_kg_species = sum(catch_iweight)) %>% 
      # bring in total catch by household
      left_join(total_hh, by = "hhid") %>% 
      # calculate proportional abundance
      mutate(pa = catch_kg_species/total_catch_kg) %>% 
      # Confirm that proportional abundances total to 1 for each household
      ## group_by(hhid) %>% 
      ## summarise(should_equal_1 = sum(pa)) ## all values = 1
      select(hhid, scode_ccm, pa)
    
    hh_ccm %>% 
      left_join(pa_hh_catch, by = c("hhid", "scode_ccm")) %>% 
      arrange(pa) %>% 
      filter(sold < 200) %>% 
      ggplot(aes(x = eat, y = sold, color = pa, size = pa)) +
      geom_point(alpha = 0.6) +
      scale_x_continuous(trans = "log1p") +
      scale_y_continuous(trans = "log1p") +
      scale_color_viridis() +
      theme_bw() +
      ggtitle("Eaten X Sold X Relative abundance (household level)") +
      xlab("Quantity eaten") +
      ylab("Quantity sold") +
      labs(size = "", color = "Relative abundance \nat household \ncatch level")
    
    # check correlation stats
    hh_ccm %>% 
      left_join(pa_hh_catch, by = c("hhid", "scode_ccm")) %>% 
      arrange(pa) %>% 
      filter(sold < 200) %>% 
      ungroup() %>% 
      mutate(total = eat + sold,
             share_eaten = eat/total) %>% 
      select(pa, share_eaten) %>% 
      drop_na(pa) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") %>% 
      broom::tidy() 
    
    hh_ccm %>% 
      left_join(pa_hh_catch, by = c("hhid", "scode_ccm")) %>% 
      arrange(pa) %>% 
      filter(sold < 200) %>% 
      ungroup() %>% 
      mutate(total = eat + sold,
             share_eaten = eat/total) %>% 
      select(pa, share_eaten) %>% 
      ggplot(aes(x = pa, y = share_eaten)) +
      geom_smooth() +
      ggtitle("Household level relative abundance and proportion of catch that is eaten") +
      xlab("Household level relative abundance") +
      ylab("Share eaten")
    
      

#### CAUGHT X EATEN X RELATIVE ABUNDANCE #### 

  # catch x eat
  # This is at the species level
  # Relative abundance is calculated at the system level
  ccm_traits_specieslevel %>% 
    ggplot(aes(x = catch, y = eat, size = rel_abundance, color = rel_abundance)) +
    geom_point() +
    scale_color_viridis() + theme_bw() +
    scale_x_continuous(trans = "log1p") +
    scale_y_continuous(trans = "log1p") +
    #geom_smooth(method = "lm", color = "black", show.legend = FALSE) +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    #geom_abline(intercept = 0, slope = 0.5, color = "#99FFFF") +
    labs(size = "", colour = "Relative abundance") +
    ggtitle("Quantity caught X Quantity eaten X Relative abundance")

  ggsave(path = "output/", "catch_x_cons_x_abund_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)
  

#### CAUGHT X SOLD X RELATIVE ABUNDANCE #### 
  
  # catch x sell
  # This is at the species level
  # Relative abundance is calculated at the system level
  ccm_traits_specieslevel %>% 
    ggplot(aes(x = catch, y = sell, size = rel_abundance, color = rel_abundance)) +
    geom_point() +
    scale_color_viridis() + theme_bw() +
    scale_x_continuous(trans = "log1p") +
    scale_y_continuous(trans = "log1p") +
    #geom_smooth(method = "lm", color = "black", show.legend = FALSE) +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    #geom_abline(intercept = 0, slope = 0.5, color = "#99FFFF") +
    labs(size = "", colour = "Relative abundance") +
    ggtitle("Quantity caught X Quantity sold X Relative abundance (species level)")
  
  ggsave(path = "output/", "catch_x_sell_x_abund_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)
  

