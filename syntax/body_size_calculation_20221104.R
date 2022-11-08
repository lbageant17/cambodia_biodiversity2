## Liz Bageant
## November 8, 2022

#------------------------------------------------------------------------------# 
#
#  CONSTRUCTING BODY SIZE FIGURES
#
# General steps at each portfolio level:
#   1. Calculate the total biomass at the portfolio level
#   2. Remove cases where total biomass = 0. These are cases where a given species was not present in the portfolio in question. 
#   3. Calculate the proportional abundance of each species within each portfolio
#   4. Multiply portfolio-level proportional abundance by total length
#   5. Take the portfolio level average of the proportional-abundance-weighted total length
#
# This file also:
#   Plots the distribution of body size across portfolios
#   Conducts means tests across CFR, catch, consumed and sold: Body size
#
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------# 
# SYSTEM LEVEL
#------------------------------------------------------------------------------# 

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
# tl_system <- pa_sys %>% 
#   left_join(., select(ccm_traits_specieslevel, c(tl, scode_biom)), by = "scode_biom") %>% 
#   ungroup() %>% 
#   mutate(tl_weighted = tl*pa_sys) %>% 
#   summarise(tl_sys = mean(tl_weighted, na.rm = TRUE)) %>% 
#   as.numeric()



#------------------------------------------------------------------------------# 
# CFR LEVEL
#------------------------------------------------------------------------------# 

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
         tl = tl_cfr, 
         hhid = 0) %>% 
  select(type, tl, hhid, cfrid)



#------------------------------------------------------------------------------# 
# HOUSEHOLD LEVEL CATCH
#------------------------------------------------------------------------------# 

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
  select(type, tl, hhid)


#------------------------------------------------------------------------------# 
# HOUSEHOLD LEVEL CONSUMPTION
#------------------------------------------------------------------------------# 

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
  select(type, tl, hhid) 


#------------------------------------------------------------------------------# 
# HOUSEHOLD LEVEL SOLD
#------------------------------------------------------------------------------# 


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
  select(type, tl, hhid) 



#------------------------------------------------------------------------------# 
# COMBINE FILES
#------------------------------------------------------------------------------# 

body_size <- tl_catch %>% 
  rbind(tl_cons) %>% 
  rbind(tl_sold) %>% 
  mutate(cfrid = 0) %>% 
  rbind(tl_cfr) %>% 
  drop_na(tl) # drops a single NaN value



#------------------------------------------------------------------------------# 
# BOXPLOT
#------------------------------------------------------------------------------# 

# sys <- log(tl_system +1) # system total length

order <-c("cfr", "catch", "cons", "sold" )

body_size %>% 
  # mutate(type = factor(type, levels = c("cfr", "catch", "cons", "sold" ))) %>% 
  ggplot(aes(x = type, y = tl, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.7, alpha = 0.4, width = 0.2, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.7) +
  scale_y_continuous(trans = "log1p") +
  #scale_y_log10() +
  coord_cartesian(ylim = c(0, 0.1)) + # zooming the plot in
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        plot.caption = element_text(hjust = 0)) +
  ylab("Mean total length") +
  labs(title = "Mean (biomass-weighted) total length by portfolio type",
       caption = "Figure is zoomed to show detail, truncating max values of household sold mean total length.White diamonds depict means. \nMeans differences are significant between: CFR and all other levels; Sold and all other levels. (Tukey HSD 95% conf.)")

ggsave(path = "output/20221104/figures", "bodysize_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)



#--------- Test mean differences in mean body size ---------------------------*/

# convert CFR biom data to household level
cfrdata <- cfrid_to_hhid %>% 
  left_join(tl_cfr, by = "cfrid") %>% 
  mutate(hhid = hhid.x) %>% 
  select(hhid, tl, type)

# generate a sold data file that contains zero values for total length for households that didn't sell fish
sold413 <- tl_sold %>%  
  full_join(cfrid_to_hhid, by = "hhid") %>% 
  mutate(tl = replace_na(tl, 0),
         type = "sold") %>% 
  select(-cfrid)


# combine data files
data <- body_size %>% 
  select(tl, type, hhid) %>% 
  filter(type != "cfr")   %>% 
  rbind(cfrdata) %>% 
  as_tibble() 

# compare differences with n = 413, replacing nd_score = 0 for households that did not sell fish.
t413 <- data %>% 
  filter(type != "sold") %>% 
  rbind(sold413) %>% 
  pairwise_t_test(tl ~ type, paired = TRUE, p.adjust.method = "bonferroni")

# compare differences with n = 266, dropping all households that did not sell fish
t266 <- data %>% 
  pivot_wider(names_from = "type", values_from = "tl") %>% 
  filter(sold > 0) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "tl") %>% 
  pairwise_t_test(tl ~ type, paired = TRUE, p.adjust.method = "bonferroni")

# export
t413 %>% rbind(t266) %>% 
  write.csv(., file = "output/20221104/tables/body_size_ttest.csv")

