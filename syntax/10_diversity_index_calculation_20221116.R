## Liz Bageant
## November 8, 2022

#------------------------------------------------------------------------------# 
#
#  CAMBODIA BIODIVERSITY PAPER ANALYSIS--Diversity indices
#  This file does the following: 
#    Uses data constructed in _MASTER.R
#    Constructs shannon, simpson and inverse simpson indices
#    Creates scatter plots of shannon indices across CFR, catch, consumed and sold
# 
#
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------# 
# Set up data files for diversity index calculations
#------------------------------------------------------------------------------# 

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


# # catch @ cfr level
# catch_div_data_cfr <- c %>% 
#   select(hhid, cfrid, scode_ccm, catch_iweight) %>% 
#   # collapse to CFR level 
#   group_by(cfrid, scode_ccm) %>% 
#   summarise(biomass = sum(catch_iweight)) %>% 
#   # reshape
#   pivot_wider(names_from = "scode_ccm", values_from = "biomass") %>% 
#   # replace NA values with zero
#   mutate_all(~replace(., is.na(.), 0))

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

# # consumption @ cfr level--not sure if this is necessary
# cons_div_data_cfr <- c %>% 
#   select(cfrid, scode_ccm, atefresh_iweight) %>% 
#   # collapse to CFR level 
#   group_by(cfrid, scode_ccm) %>% 
#   summarise(biomass = sum(atefresh_iweight)) %>% 
#   # reshape
#   pivot_wider(names_from = "scode_ccm", values_from = "biomass") %>% 
#   # replace NA values with zero
#   mutate_all(~replace(., is.na(.), 0)) 

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

# # sold @ cfr level--not sure if this is necessary
# sold_div_data_cfr <- c %>% 
#   select(cfrid, scode_ccm, soldfresh_iweight) %>% 
#   # collapse to CFR level 
#   group_by(cfrid, scode_ccm) %>% 
#   summarise(biomass = sum(soldfresh_iweight)) %>% 
#   # reshape
#   pivot_wider(names_from = "scode_ccm", values_from = "biomass") %>% 
#   # replace NA values with zero
#   mutate_all(~replace(., is.na(.), 0)) 

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


#------------------------------------------------------------------------------# 
# Calculate diversity indices
#------------------------------------------------------------------------------# 

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


# # Catch diversity @ CFR level
# 
#     # Shannon Index (H)
#     temp <- catch_div_data_cfr %>% 
#       ungroup() %>% 
#       select(-cfrid) %>% 
#       diversity() %>% 
#       # adding back CFR IDs
#       cbind(catch_div_data_cfr$cfrid) %>% 
#       as.data.frame()
#     
#     h_catch_cfr <- temp %>% 
#       mutate(h_catch = as.numeric(.),
#              cfrid = V2) %>% 
#       select(h_catch, cfrid)
#     
#     # Simpson index (D1)
#     temp <- catch_div_data_cfr %>% 
#       ungroup() %>% 
#       select(-cfrid) %>% 
#       diversity("simpson") %>% 
#       # adding back CFR IDs
#       cbind(catch_div_data_cfr$cfrid) %>% 
#       as.data.frame()
#     
#     d1_catch_cfr <- temp %>% 
#       mutate(d1_catch = as.numeric(.),
#              cfrid = V2) %>% 
#       select(d1_catch, cfrid)
#     
#     # Inverse simpson index (D2)
#     temp <- catch_div_data_cfr %>% 
#       ungroup() %>% 
#       select(-cfrid) %>% 
#       diversity("invsimpson") %>% 
#       # adding back CFR IDs
#       cbind(catch_div_data_cfr$cfrid) %>% 
#       as.data.frame()
#     
#     d2_catch_cfr <- temp %>% 
#       mutate(d2_catch = as.numeric(.),
#              cfrid = V2) %>% 
#       select(d2_catch, cfrid)
#     
#     # combine files
#     indices_catch_cfr <- h_catch_cfr %>% 
#       left_join(d1_catch_cfr, by = "cfrid") %>% 
#       left_join(d2_catch_cfr, by = "cfrid")
 
       
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
    

# ## Consumption diversity at the CFR level
#     
#     # Shannon Index (H)
#     temp <- cons_div_data_cfr %>% 
#       ungroup() %>% 
#       select(-cfrid) %>% 
#       diversity() %>% 
#       # adding back CFR IDs
#       cbind(cons_div_data_cfr$cfrid) %>% 
#       as.data.frame()
#     
#     h_cons_cfr <- temp %>% 
#       mutate(h_cons = as.numeric(.),
#              cfrid = V2) %>% 
#       select(h_cons, cfrid)
#     
#     # Simpson index (D1)
#     temp <- cons_div_data_cfr %>% 
#       ungroup() %>% 
#       select(-cfrid) %>% 
#       diversity("simpson") %>% 
#       # adding back CFR IDs
#       cbind(cons_div_data_cfr$cfrid) %>% 
#       as.data.frame()
#     
#     d1_cons_cfr <- temp %>% 
#       mutate(d1_cons = as.numeric(.),
#              cfrid = V2) %>% 
#       select(d1_cons, cfrid)
#     
#     # Inverse simpson index (D2)
#     temp <- cons_div_data_cfr %>% 
#       ungroup() %>% 
#       select(-cfrid) %>% 
#       diversity("invsimpson") %>% 
#       # adding back CFR IDs
#       cbind(cons_div_data_cfr$cfrid) %>% 
#       as.data.frame()
#     
#     d2_cons_cfr <- temp %>% 
#       mutate(d2_cons = as.numeric(.),
#              cfrid = V2) %>% 
#       select(d2_cons, cfrid)
#     
#     # combine files
#     indices_cons_cfr <- h_cons_cfr %>% 
#       left_join(d1_cons_cfr, by = "cfrid") %>% 
#       left_join(d2_cons_cfr, by = "cfrid")   
    
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

# ## Sold diversity at the CFR level
#     
#     # Shannon Index (H)
#     temp <- sold_div_data_cfr %>% 
#       ungroup() %>% 
#       select(-cfrid) %>% 
#       diversity() %>% 
#       # adding back CFR IDs
#       cbind(sold_div_data_cfr$cfrid) %>% 
#       as.data.frame()
#     
#     h_sold_cfr <- temp %>% 
#       mutate(h_sold = as.numeric(.),
#              cfrid = V2) %>% 
#       select(h_sold, cfrid)
#     
#     # Simpson index (D1)
#     temp <- sold_div_data_cfr %>% 
#       ungroup() %>% 
#       select(-cfrid) %>% 
#       diversity("simpson") %>% 
#       # adding back CFR IDs
#       cbind(sold_div_data_cfr$cfrid) %>% 
#       as.data.frame()
#     
#     d1_sold_cfr <- temp %>% 
#       mutate(d1_sold = as.numeric(.),
#              cfrid = V2) %>% 
#       select(d1_sold, cfrid)
#     
#     # Inverse simpson index (D2)
#     temp <- sold_div_data_cfr %>% 
#       ungroup() %>% 
#       select(-cfrid) %>% 
#       diversity("invsimpson") %>% 
#       # adding back CFR IDs
#       cbind(sold_div_data_cfr$cfrid) %>% 
#       as.data.frame()
#     
#     d2_sold_cfr <- temp %>% 
#       mutate(d2_sold = as.numeric(.),
#              cfrid = V2) %>% 
#       select(d2_sold, cfrid)
#     
#     # combine files
#     indices_sold_cfr <- h_sold_cfr %>% 
#       left_join(d1_sold_cfr, by = "cfrid") %>% 
#       left_join(d2_sold_cfr, by = "cfrid")
#     
    
#### ----- Create CFR and HH level combined data files ------ ####     
    
  ## CFR-level file
  # diversity_indices_cfr_level <- indices_biom_cfr %>% 
  #       left_join(indices_catch_cfr, by = "cfrid") %>% 
  #       left_join(indices_cons_cfr, by = "cfrid") %>% 
  #       left_join(indices_sold_cfr, by = "cfrid") %>% 
  #       relocate(cfrid)
   
  ## Expand CFR-level file to HH level 
    cfrfile <- indices_biom_cfr %>% 
      left_join(cfrid_to_hhid, by = "cfrid")
  
  ## HH-level file
      diversity_indices_hh_level <- indices_catch_hh %>% 
        left_join(indices_cons_hh, by = "hhid") %>% 
      left_join(indices_sold_hh, by = "hhid") %>% 
        left_join(cfrfile, by = "hhid") %>% 
      relocate(hhid)
      
  
    
  

  ## Export for use in stata
      #write.csv(diversity_indices_cfr_level, file = "data/processed/diversity_indices_cfr_level.csv")
      write.csv(diversity_indices_hh_level, file = "data/processed/diversity_indices_hh_level.csv")

      
#------------------------------------------------------------------------------# 
# Create diversity index boxplots
#------------------------------------------------------------------------------#       

      
# Shannon index ----------------------------------------------------------------
      
  shannon <- diversity_indices_hh_level %>% 
    select(hhid, h_biom, h_catch, h_sold, h_cons) %>% 
    rename(cfr = h_biom, 
           catch = h_catch,
           sold = h_sold, 
           cons = h_cons) %>% 
    pivot_longer(!hhid, names_to = "type", values_to = "shannon")
    
      
order <-c("cfr", "catch", "cons", "sold" ) 
shannon %>% 
  ggplot(aes(x = type, y = shannon, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.7, alpha = 0.4, width = 0.2, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        plot.caption = element_text(hjust = 0)) +
  ylab("Mean Shannon index") +
  labs(title = "Mean Shannon index by portfolio type",
       caption = "
      White diamonds depict means. Means differences are significant between all groups 
      (Paired t-tests with Bonferroni correction)")

path <- paste("output/",output_date,"/figures/",sep="")
ggsave(path = path, "shannon_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)

# t-tests

# compare differences with n = 413, replacing shannon = 0 for households that did not sell fish.
t413 <- shannon %>% 
  pairwise_t_test(shannon ~type, paired = TRUE, p.adjust.method = "bonferroni")
  
# compare differences with n = 267, dropping all households that did not sell fish
t267 <- shannon %>% 
  pivot_wider(names_from = "type", values_from = "shannon") %>% 
  # to filter households that did not sell fish we need to identify those households and distinguish them from households that sold only a single species (which makes their shannon index = 0)
  left_join(hh_sold, by = "hhid") %>% 
  drop_na(sold_species) %>% 
  select(-sold_species, -cfrid) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "shannon") %>% 
  pairwise_t_test(shannon ~type, paired = TRUE, p.adjust.method = "bonferroni")

# export
csv_name <- paste("output/",output_date,"/tables/ttests/simpson_ttest.csv",sep="")
t413 %>% rbind(t267) %>% 
  write.csv(., file = csv_name)
      
# Simpson index ----------------------------------------------------------------

simpson <- diversity_indices_hh_level %>% 
  select(hhid, d1_biom, d1_catch, d1_sold, d1_cons) %>% 
  rename(cfr = d1_biom, 
         catch = d1_catch,
         sold = d1_sold, 
         cons = d1_cons) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "simpson")


order <-c("cfr", "catch", "cons", "sold" ) 
simpson %>% 
  ggplot(aes(x = type, y = simpson, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.7, alpha = 0.4, width = 0.2, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        plot.caption = element_text(hjust = 0)) +
  ylab("Mean Simpson index") +
  labs(title = "Mean Simpson index by portfolio type",
       caption = "
      White diamonds depict means. Means differences are significant between all groups 
      (Paired t-tests with Bonferroni correction)")

path <- paste("output/",output_date,"/figures/",sep="")
ggsave(path = path, "simpson_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)

# t-tests

# compare differences with n = 413. Simpson = 1 for households that sold no fish
t413 <- simpson %>% 
  pairwise_t_test(simpson ~type, paired = TRUE, p.adjust.method = "bonferroni")

# compare differences with n = 237, dropping all households that did not sell fish
t267 <- simpson %>% 
  pivot_wider(names_from = "type", values_from = "simpson") %>% 
  filter(sold != 1) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "simpson") %>% 
  pairwise_t_test(simpson ~type, paired = TRUE, p.adjust.method = "bonferroni")

# export
csv_name <- paste("output/",output_date,"/tables/ttests/simpson_ttest.csv",sep="")
t413 %>% rbind(t267) %>% 
  write.csv(., file = csv_name)


# Inverse Simpson index --------------------------------------------------------

inv_simpson <- diversity_indices_hh_level %>% 
  select(hhid, d2_biom, d2_catch, d2_sold, d2_cons) %>% 
  rename(cfr = d2_biom, 
         catch = d2_catch,
         sold = d2_sold, 
         cons = d2_cons) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "inv_simpson")

order <-c("cfr", "catch", "cons", "sold" ) 
inv_simpson %>% 
  filter(!is.infinite(inv_simpson)) %>% 
  ggplot(aes(x = type, y = inv_simpson, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.7, alpha = 0.4, width = 0.2, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        plot.caption = element_text(hjust = 0)) +
  ylab("Mean Inverse Simpson index") +
  labs(title = "Mean Inverse Simpson index by portfolio type",
       caption = "
      White diamonds depict means. Means differences are significant between all groups 
      (Paired t-tests with Bonferroni correction)")

path <- paste("output/",output_date,"/figures/",sep="")
ggsave(path = path, "invsimpson_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)

# t-tests

# compare differences with n = 413. We do not do this for the inverse simpson index.

# compare differences with n = 237, dropping all households that did not sell fish
t267 <- inv_simpson %>% 
  pivot_wider(names_from = "type", values_from = "inv_simpson") %>% 
  filter(!is.infinite(sold)) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "inv_simpson") %>% 
  pairwise_t_test(inv_simpson ~type, paired = TRUE, p.adjust.method = "bonferroni")

# export
csv_name <- paste("output/",output_date,"/tables/ttests/inv_simpson_ttest.csv",sep="")
t267 %>% 
  write.csv(., file = csv_name)

