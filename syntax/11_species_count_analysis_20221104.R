## Liz Bageant
## November 15, 2022


#------------------------------------------------------------------------------# 
#
#  CAMBODIA BIODIVERSITY PAPER ANALYSIS--Species counts
#
#  This file does the following: 
#    Uses species count data constructed in _MASTER.R
#    Plots species count boxplots
#    Conducts means tests across CFR, catch, consumed and sold: species counts
# 
# This file also contains prior analyses that are not currently part of the main paper:
#    Species count scatter plots
#    Catch vs. sold scatter plots
#
#------------------------------------------------------------------------------# 

## NOTE: All libraries and data prep can be found in _MASTER.R file.

cfr_file <- cfr_biodiv %>% 
  select(cfrid, biom_species) %>% 
  rename(species = biom_species) %>% 
  mutate(type = "cfr") %>% 
  distinct() %>% 
  mutate(hhid = 0)

# combine files for plotting
cfr_biodiv_long <- cfr_biodiv %>% 
  select(-cfrid, -biom_species) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "species") %>% 
  arrange(type) %>% 
  mutate(cfrid = 0) %>% 
  rbind(cfr_file)


# plot
order <-c("cfr", "catch_species", "consumption_species", "sold_species" )
cfr_biodiv_long %>% 
  filter(species != 0) %>% 
  ggplot(aes(x = type, y = species, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 1.3, alpha = 0.5, width = 0.2, color = "black") +  
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        plot.caption = element_text(hjust = 0)) +
  ylab("Number of species") +
  labs(title = "Number of species in CFR and household portfolios", 
       caption = "
       Sold plot excludes excludes households that sold no fish. White diamonds depict means. 
       Means differences are significant between all levels (paired t-test with Bonferroni correction)")

#ggsave(path = "output/20221109/figures", "species_count_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)
path <- paste("output/",output_date,"/figures/",sep="")
ggsave(path = path, "species_count_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)


#--------- Test mean differences in number of species ------------------------*/

# convert CFR biom data to household level
cfrdata <- cfrid_to_hhid %>% 
  left_join(cfr_file, by = "cfrid") %>% 
  mutate(hhid = hhid.x) %>% 
  select(hhid, species, type)

# # generate a sold data file that contains zero values for commonness for households that didn't sell fish
# sold413 <- commonness_sold %>% 
#   full_join(cfrid_to_hhid, by = "hhid") %>% 
#   mutate(commonness = replace_na(commonness, 0),
#          type = "sold") %>% 
#   select(-cfrid)


# combine data files
data <- cfr_biodiv %>% 
  select(-cfrid, -biom_species) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "species") %>% 
  filter(type != "biom_species")   %>% 
  rbind(cfrdata) %>% 
  as_tibble() 

  ## Export for use in stata
  data %>% 
    pivot_wider(names_from = "type", values_from = "species") %>% 
    write.csv(., file = "data/processed/species_hh_level.csv")

# compare differences with n = 413, replacing nd_score = 0 for households that did not sell fish.
t413 <- data %>% 
  # filter(type != "sold") %>% 
  # rbind(sold413) %>% 
  pairwise_t_test(species ~ type, paired = TRUE, p.adjust.method = "bonferroni")

# compare differences with n = 266, dropping all households that did not sell fish
t266 <- data %>% 
  pivot_wider(names_from = "type", values_from = "species") %>% 
  filter(sold_species > 0) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "species") %>% 
  pairwise_t_test(species ~ type, paired = TRUE, p.adjust.method = "bonferroni")

# export
csv_name <- paste("output/",output_date,"/tables/ttests/species_ttest.csv",sep="")
t413 %>% rbind(t266) %>% 
  write.csv(., file = csv_name)



#--------- Look at number of species at each level ---------------------------*/

# number of unique species in the biomonitoring data
biom_species_no <- b %>% 
  select(scode_biom) %>% 
  mutate(code = scode_biom, 
         biom = "biom") %>% 
  distinct() # 132

# number of unique species at catch level
catch_species_no <- c %>% 
  select(scode_ccm) %>% 
  mutate(code = scode_ccm,
         catch_code = scode_ccm,
         catch = "catch") %>% 
  select(-scode_ccm) %>% 
  distinct() # 130

# number of unique species at consumption level
cons_species_no <- c %>% 
  filter(atefresh == 1) %>% 
  select(scode_ccm) %>% 
  mutate(code = scode_ccm,
         scode_cons = scode_ccm, 
         cons = "cons") %>% 
  select(-scode_ccm) %>% 
  distinct() # 126

# number of unique species at sold level
sold_species_no <- c %>% 
  filter(soldfresh == 1) %>% 
  select(scode_ccm) %>% 
  mutate(code = scode_ccm, 
         scode_sold = scode_ccm,
         sold = "sold") %>% 
  select(-scode_ccm) %>% 
  distinct() # 110

# combine all data files to see what species are present in each level.
data <- biom_species_no %>% 
  full_join(catch_species_no, by = "code") %>% 
  full_join(cons_species_no, by = "code") %>% 
  full_join(sold_species_no, by = "code") %>% 
  arrange(code) %>% 
  select(code, biom, catch, cons, sold)

# total number of species in data set
nrow(data) # 137

# species that are present in all levels
all_levels <- data %>% 
  drop_na(biom, catch, cons, sold) # 104

# species that are only in biom and nowhere else
biom_only <- data %>% 
  mutate(count =rowSums(is.na(.))) %>% 
  filter(!is.na(biom),
         count == 3) # 7

# species that are only in catch and nowhere else
catch_only <- data %>% 
  drop_na(catch) %>% 
  mutate(count = rowSums(is.na(.))) %>% 
  filter(count == 3) # 0

# species that are in system and catch but nowhere else
sys_catch_only <- data %>% 
  drop_na(biom, catch) %>% 
  mutate(count = rowSums(is.na(.))) %>% 
  filter(count == 2) # 2

# species that are caught and consumed, but not sold
biom_catch_cons_only <- data %>% 
  drop_na(biom, catch, cons) %>% 
  mutate(count = rowSums(is.na(.))) %>% 
  filter(count == 1) #18

# species that are caught and consumed, but not sold
biom_catch_sold_only <- data %>% 
  drop_na(biom, catch, sold) %>%  
  mutate(count = rowSums(is.na(.))) %>% 
  filter(count == 1) # 1

# species that are not in biomonitoring but are caught and consumed
catch_cons_only <- data %>% 
  filter(is.na(biom)) %>% 
  filter(is.na(cons)) %>% 
  mutate(count = rowSums(is.na(.))) %>%  # 1
  filter(count == 2)

# species that are not in biomonitoring but are caught, sold and consumed
catch_sold_cons_only <- data %>% 
  filter(is.na(biom)) %>% 
  mutate(count = rowSums(is.na(.))) %>% 
  filter(count == 1) # 4


check <- all_levels %>% 
  mutate(count = 0) %>% 
  rbind(biom_only, catch_only, sys_catch_only, biom_catch_cons_only, biom_catch_sold_only, catch_cons_only, catch_sold_cons_only) %>% 
  arrange(code) %>% 
  select(code) %>%
  mutate(x = "x") %>% 
  full_join(data, by = "code")
  




#------------------------------------------------------------------------------# 
# Analysis below this point that is not currently included in the main paper:
#
#   Number of species scatter plots
#   Catch vs. sold scatter plots (including RDAs, abundance dimensions)
#
# 11/8/2022: These analyses still run despite changes upstream. 
#
#------------------------------------------------------------------------------# 



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
  
  #ggsave(path = "output/20221109/figures/secondary", "numspecies_biom_x_catch.png", width = 16, height =  12, units = "cm", dpi = 320)
  
  
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
  
  #ggsave(path = "output/20221109/figures/secondary", "numspecies_biom_x_catch_x_effort.png", width = 16, height =  12, units = "cm", dpi = 320)


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
    
    #ggsave(path = "output/20221109/figures/secondary", "numspecies_biom_x_sold.png", width = 16, height =  12, units = "cm", dpi = 320)
    
    
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
    
    #ggsave(path = "output/20221109/figures/secondary", "numspecies_catch_x_cons.png", width = 16, height =  12, units = "cm", dpi = 320)

    
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
    
    #ggsave(path = "output/20221109/figures/secondary", "numspecies_catch_x_cons_x_effort.png", width = 16, height =  12, units = "cm", dpi = 320)
    
    

    
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
    
    #ggsave(path = "output/20221109/figures/secondary", "numspecies_catch_x_sold.png", width = 16, height =  12, units = "cm", dpi = 320)
    
    # Check correlation stats
    cfr_biodiv %>% 
      select(catch_species, sold_species) %>% 
      as.matrix() %>% 
      Hmisc::rcorr(type = "spearman") %>% 
      broom::tidy() 
    

    
    
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
    #ggsave(path = "output/20221109/figures/secondary", "cons_x_rda_boxplot_UNWEIGHTED.png", width = 16, height =  12, units = "cm", dpi = 320)

    
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
    
     #ggsave(path = "output/20221109/figures/secondary", "sold_x_rda_boxplot_UNWEIGHTED.png", width = 16, height =  12, units = "cm", dpi = 320)
  
    
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
    
     #ggsave(path = "output/20221109/figures/secondary", "cons_x_sold_x_abund_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)
 
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

    #ggsave(path = "output/20221109/figures/secondary", "catch_x_cons_x_abund_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)
  

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
  
    #ggsave(path = "output/20221109/figures/secondary", "catch_x_sell_x_abund_specieslevel.png", width = 16, height =  12, units = "cm", dpi = 320)
  

