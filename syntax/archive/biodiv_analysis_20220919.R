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
      geom_abline(intercept = 0, slope = 1, color = "gray") +
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
      geom_abline(intercept = 0, slope = 1, color = "gray") +
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
#------------------------------------------------------------------------------# 
    
  # eat x sell
  ccm_traits_specieslevel %>% 
    ggplot(aes(x = eat, y = sell, size = rel_abundance, color = rel_abundance)) +
    geom_point() +
    scale_color_viridis() + theme_bw() +
    scale_x_continuous(trans = "log1p") +
    scale_y_continuous(trans = "log1p") +
    #geom_smooth(method = "lm", color = "black", show.legend = FALSE) +
    labs(size = "", colour = "Relative abundance") +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    ggtitle("Quantity eaten X Quantity sold X Relative abundance (system level)")
  
  ggsave(path = "output/", "cons_x_sold_x_abund_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)
  
  # eat x sell at HH level with CFR-level relative abundance
  
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
    ccm_traits_specieslevel %>% 
      select(scode_ccm, num_rda100, tl, rel_abundance) %>% 
      mutate(scode_biom = scode_ccm) %>% # facilitate merge with pa_cfr
      full_join(hh_ccm, by = "scode_ccm") %>%  # dim(x) 9594 x 12
      left_join(cfrid_to_hhid, by = "hhid") %>% # dim(x) 9594 x 13
      left_join(pa_cfr, by = c("cfrid", "scode_biom")) %>% 
      arrange(hhid) %>% # dim() #9694 x 12
      filter(sold < 200) %>% 
      ggplot(aes(x = eat, y = sold, color = pa, size = pa)) +
      geom_point(alpha = 0.6) +
      scale_x_continuous(trans = "log1p") +
      scale_y_continuous(trans = "log1p") +
      scale_color_viridis() +
      theme_bw() +
      ggtitle("Eaten X Sold X Relative abundance (CFR level)") +
      xlab("Quantity eaten") +
      ylab("Quantity sold")
      
  
  # eat x sell at household level
  ccm_traits_specieslevel %>% 
    select(scode_ccm, num_rda100, tl, rel_abundance) %>% 
    full_join(hh_ccm, by = "scode_ccm") %>% 
    arrange(hhid) %>% # dim() #9694 x 12
    filter(sold < 200) %>% 
    ggplot(aes(x = eat, y = sold, color = rel_abundance, size = rel_abundance)) +
    geom_point(alpha = 0.6) +
    scale_x_continuous(trans = "log1p") +
    scale_y_continuous(trans = "log1p") +
    scale_color_viridis() +
    theme_bw() +
    ggtitle("Eaten X Sold X Relative abundance (household level)") +
    xlab("Quantity eaten") +
    ylab("Quantity sold")

  ggsave(path = "output/", "cons_x_sold_x_abund_hhlevel.png", width = 16, height =  12, units = "cm", dpi = 320)
  
  
  # catch x eat
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
  
  
  # catch x sell
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
  

  
  
  
  
#------------------------------------------------------------------------------# 
#### Nutrition plots ####
#------------------------------------------------------------------------------# 

## EVERYTHING IN THIS SECTION IS UNWEIGHTED RDAS AND NEEDS TO BE FIXED
  
  # eat x sell 
  ccm_traits_specieslevel %>% 
    ggplot(aes(x = eat, y = sell, size = num_rda100, color = num_rda100)) +
    geom_point() +
    scale_color_viridis() + theme_bw() +
    scale_x_continuous(trans = "log1p") +
    scale_y_continuous(trans = "log1p") +
    #geom_smooth(method = "lm", color = "black", show.legend = FALSE) +
    labs(size = "", colour = "RDAs Met") +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    ggtitle("Eaten X Sold X RDAs met") +
    xlab("Quantity eaten") +
    ylab("Quantity sold")
  
  ggsave(path = "output/", "eat_x_sell_x_rda_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)
  
  
  # eat x sell at household level
  ccm_traits_specieslevel %>% 
    select(scode_ccm, num_rda100, tl, rel_abundance) %>% 
    full_join(hh_ccm, by = "scode_ccm") %>% 
    arrange(hhid) %>% # dim() 9694 x 12
    filter(sold < 200) %>% 
    ggplot(aes(x = eat, y = sold, color = num_rda100, size = num_rda100)) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    scale_x_continuous(trans = "log1p") +
    scale_y_continuous(trans = "log1p") +
    scale_color_viridis() +
    theme_bw() +
    labs(size = "", colour = "RDAs Met") +
    ggtitle("Eaten X Sold X RDAs met (household level)") +
    xlab("Quantity eaten") +
    ylab("Quantity sold")
  
  ggsave(path = "output/", "eat_x_sell_x_rda_hhlevel.png", width = 16, height =  12, units = "cm", dpi = 320)
  
  

  # catch x eat
  ccm_traits_specieslevel %>% 
    ggplot(aes(x = catch, y = eat, size = num_rda100, color = num_rda100)) +
    geom_point() +
    scale_color_viridis() + theme_bw() +
    scale_x_continuous(trans = "log1p") +
    scale_y_continuous(trans = "log1p") +
    #geom_smooth(method = "lm", color = "black", show.legend = FALSE) +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    #geom_abline(intercept = 0, slope = 0.5, color = "#99FFFF") +
    labs(size = "", colour = "RDAs Met") +
    ggtitle("Quantity caught X Quantity eaten X RDAs met")
  
  ggsave(path = "output/", "catch_x_eat_x_rda_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)
  
  
  
  # catch x sell
  ccm_traits_specieslevel %>% 
    ggplot(aes(x = catch, y = sell, size = num_rda100, color = num_rda100)) +
    geom_point() +
    scale_color_viridis() + theme_bw() +
    scale_x_continuous(trans = "log1p") +
    scale_y_continuous(trans = "log1p") +
    #geom_smooth(method = "lm", color = "black", show.legend = FALSE) +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    #geom_abline(intercept = 0, slope = 0.5, color = "#99FFFF") +
    labs(size = "", colour = "RDAs Met") +
    ggtitle("Quantity caught X Quantity sold X RDAs met")
  
  ggsave(path = "output/", "catch_x_sell_x_rda_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)
 
  
#------------------------------------------------------------------------------# 
#### Body size (total length) plots ####
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
    