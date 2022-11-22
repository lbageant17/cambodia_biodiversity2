## Liz Bageant
## November 21, 2022

#------------------------------------------------------------------------------# 
#
#  CAMBODIA BIODIVERSITY PAPER ANALYSIS--Diversity indices
#  This file does the following: 
#    Uses data constructed in _MASTER.R
#    Constructs shannon, simpson and inverse simpson indices
#    Creates box plots of shannon and simpson indices across CFR, catch, consumed and sold
#    Creates scatter plots of shannon and simpson indices across CFR, catch, consumed and sold
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
  
    
#### ----- Create CFR and HH level combined data files ------ ####     

   
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

  # create a long version of biodiversity index file that contains 40 CFR observations (not 413)
      
  # set up shannon CFR file
      shannon_cfr <- h_biom_cfr %>% 
        mutate(type = "h_cfr") %>% 
        rename(index = h_biom)
  # set up simpson CFR file
      simpson_cfr <- d1_biom_cfr %>% 
        mutate(type = "d1_cfr") %>% 
        rename(index = d1_biom)
  # set up inverse simpson CFR file
      invsimpson_cfr <- d2_biom_cfr %>% 
        mutate(type = "d2_cfr") %>% 
        rename(index = d2_biom)
        
  index_long <- diversity_indices_hh_level %>% 
    select(-contains("biom"), -cfrid) %>% 
    pivot_longer(!hhid, names_to = "type", values_to = "index") %>%
    left_join(cfrid_to_hhid, by = "hhid") %>% 
    select(-hhid) %>% 
    # add shannon CFR file
    rbind(shannon_cfr) %>% 
    # add simpson CFR file
    rbind(simpson_cfr) %>% 
    # add inverse simpson
    rbind(invsimpson_cfr)
#------------------------------------------------------------------------------# 
# Create diversity index boxplots--household level
#------------------------------------------------------------------------------#       

      
# Shannon index ----------------------------------------------------------------

plotshannon <- index_long %>% 
    filter(type %in% c("h_cfr", "h_catch", "h_cons", "h_sold"))

  order <- c("h_cfr", "h_catch", "h_cons", "h_sold" )
  
# Original boxplot   
  plotshannon %>%
    filter(index != 0) %>%  # removing cases where people caught no fish
    ggplot(aes(x = type, y = index, fill = type)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(size = 0.7, alpha = 0.4, width = 0.2, color = "black") +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
    scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.7) +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          plot.caption = element_text(hjust = 0)) +
    ylab("Mean Shannon index") +
    labs(title = "Mean Shannon index by portfolio type",
         caption = "
        White diamonds depict means. Means differences are significant between all groups
        (Paired t-tests with Bonferroni correction)")
  path <- paste("output/",output_date,"/figures/",sep="")
  ggsave(path = path, "shannon_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)
  
# Boxplot with colors mapped to CFR-level shannon values
  cfr_colors <- cfrfile %>% 
    select(h_biom, cfrid) %>% 
    rename(cfr_colors = h_biom) %>% 
    distinct()

    order <- c("h_cfr", "h_catch", "h_cons", "h_sold" )
  
  plotshannon %>% 
    filter(index != 0) %>% 
    left_join(cfr_colors, by = "cfrid") %>% 
    ggplot(aes(x = type, y = index)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(color = cfr_colors), alpha = 0.7, width = 0.2) +
    scale_color_viridis(option = "C", name = "CFR Shannon \nindex") +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
    scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
    theme_bw() +
    ylab("Shannon index") +
    labs(title = "Shannon index by portfolio type")
  
  path <- paste("output/",output_date,"/figures/",sep="")
  ggsave(path = path, "shannon_boxplot_cfr.png", width = 16, height =  12, units = "cm", dpi = 320)
  
 
# t-tests @ hh level ----------------------------------------------------------

shannon <- diversity_indices_hh_level %>% 
  select(hhid, h_biom, h_catch, h_sold, h_cons) %>% 
  rename(cfr = h_biom, 
         catch = h_catch,
         sold = h_sold, 
         cons = h_cons) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "shannon")
  
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



plotsimpson <- index_long %>% 
  filter(type %in% c("d1_cfr", "d1_catch", "d1_cons", "d1_sold"))

order <- c("d1_cfr", "d1_catch", "d1_cons", "d1_sold")

# Original boxplot   
plotsimpson %>% 
  filter(index != 1) %>%  # removing cases where people caught no fish
  ggplot(aes(x = type, y = index, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.7, alpha = 0.4, width = 0.2, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.7) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        plot.caption = element_text(hjust = 0)) +
  ylab("Mean Simpson index") +
  labs(title = "Mean Simpson index by portfolio type",
       caption = "
        White diamonds depict means.")
       
       path <- paste("output/",output_date,"/figures/",sep="")
       ggsave(path = path, "simpson_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)
       
 # Boxplot with colors mapped to CFR-level simpson values
 cfr_colors <- cfrfile %>% 
   select(h_biom, cfrid) %>% 
   rename(cfr_colors = h_biom) %>% 
   distinct()
 
 plotsimpson %>% 
   filter(index != 1) %>% 
   left_join(cfr_colors, by = "cfrid") %>% 
   ggplot(aes(x = type, y = index)) +
   geom_boxplot(outlier.shape = NA) +
   geom_jitter(aes(color = cfr_colors), alpha = 0.7, width = 0.2) +
   scale_color_viridis(option = "C", name = "CFR Simpson \nindex") +
   stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
   scale_x_discrete(limits = order, labels = c("CFR","Catch","Consumed", "Sold")) +
   theme_bw() +
   ylab("Simpson index") +
   labs(title = "Simpson index by portfolio type")
 
 path <- paste("output/",output_date,"/figures/",sep="")
 ggsave(path = path, "simpson_boxplot_cfr.png", width = 16, height =  12, units = "cm", dpi = 320)
# simpson <- diversity_indices_hh_level %>% 

# t-tests @ hh level ----------------------------------------------------------

  simpson <- diversity_indices_hh_level %>% 
   select(hhid, d1_biom, d1_catch, d1_sold, d1_cons) %>% 
   rename(cfr = d1_biom, 
          catch = d1_catch,
          sold = d1_sold, 
          cons = d1_cons) %>% 
   pivot_longer(!hhid, names_to = "type", values_to = "simpson") 
 
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

#------------------------------------------------------------------------------# 
# Scatter plots--Shannon
#------------------------------------------------------------------------------# 

path <- paste("output/",output_date,"/figures/scatter_plots",sep="")

# system X catch
diversity_indices_hh_level %>% 
  ggplot(aes(x = h_biom, y = h_catch)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Shannon: Biomonitoring X Catch") +
  xlab("Shannon index of biomonitoring (CFR level)") +
  ylab("Shannon index of species caught (HH level)")

ggsave(path = path, "shannon_systemXcatch.png", width = 16, height =  12, units = "cm", dpi = 320)

# catch X consumption
diversity_indices_hh_level %>% 
  ggplot(aes(x = h_catch, y = h_cons)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Shannon: Catch X Consumption") +
  xlab("Shannon index of catch (HH level)") +
  ylab("Shannon index of consumed (HH level)")

ggsave(path = path, "shannon_catchXcons.png", width = 16, height =  12, units = "cm", dpi = 320)


# catch X sold
diversity_indices_hh_level %>% 
  ggplot(aes(x = h_catch, y = h_sold)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Shannon: Catch X Sold") +
  xlab("Shannon index of catch (HH level)") +
  ylab("Shannon index of sold (HH level)")

ggsave(path = path, "shannon_catchXsold.png", width = 16, height =  12, units = "cm", dpi = 320)


# system X consumption
diversity_indices_hh_level %>% 
  ggplot(aes(x = h_biom, y = h_cons)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Number of species: Biomonitoring X Consumption") +
  xlab("Shannon index of biomonitoring (CFR level)") +
  ylab("Shannon index of consumed (HH level)")

ggsave(path = path, "shannon_systemXcons.png", width = 16, height =  12, units = "cm", dpi = 320)

# system X sold

diversity_indices_hh_level %>% 
  ggplot(aes(x = h_biom, y = h_sold)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Number of species: Biomonitoring X Sold") +
  xlab("Shannon index of biomonitoring (CFR level)") +
  ylab("Shannon index of sold (HH level)")

ggsave(path = path, "shannon_systemXsold.png", width = 16, height =  12, units = "cm", dpi = 320)


 
#------------------------------------------------------------------------------# 
# Scatter plots--Simpson
#------------------------------------------------------------------------------# 

# system X catch
diversity_indices_hh_level %>% 
  ggplot(aes(x = d1_biom, y = d1_catch)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Simpson: Biomonitoring X Catch") +
  xlab("Simpson index of biomonitoring (CFR level)") +
  ylab("Simpson index of species caught (HH level)")

ggsave(path = path, "simpson_systemXcatch.png", width = 16, height =  12, units = "cm", dpi = 320)

# catch X consumption
diversity_indices_hh_level %>% 
  ggplot(aes(x = d1_catch, y = d1_cons)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Simpson: Catch X Consumption") +
  xlab("Simpson index of catch (HH level)") +
  ylab("Simpson index of consumed (HH level)")

ggsave(path = path, "simpson_catchXcons.png", width = 16, height =  12, units = "cm", dpi = 320)


# catch X sold
diversity_indices_hh_level %>% 
  ggplot(aes(x = d1_catch, y = d1_sold)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Simpson: Catch X Sold") +
  xlab("Simpson index of catch (HH level)") +
  ylab("Simpson index of sold (HH level)")

ggsave(path = path, "simpson_catchXsold.png", width = 16, height =  12, units = "cm", dpi = 320)


# system X consumption
diversity_indices_hh_level %>% 
  ggplot(aes(x = d1_biom, y = d1_cons)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Number of species: Biomonitoring X Consumption") +
  xlab("Simpson index of biomonitoring (CFR level)") +
  ylab("Simpson index of consumed (HH level)")

ggsave(path = path, "simpson_systemXcons.png", width = 16, height =  12, units = "cm", dpi = 320)

# system X sold

diversity_indices_hh_level %>% 
  ggplot(aes(x = d1_biom, y = d1_sold)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Number of species: Biomonitoring X Sold") +
  xlab("Simpson index of biomonitoring (CFR level)") +
  ylab("Simpson index of sold (HH level)")

ggsave(path = path, "simpson_systemXsold.png", width = 16, height =  12, units = "cm", dpi = 320)

