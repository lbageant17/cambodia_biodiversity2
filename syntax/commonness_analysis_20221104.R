# Liz Bageant
## November 8, 2022


#------------------------------------------------------------------------------# 
#
#  CAMBODIA BIODIVERSITY PAPER ANALYSIS--Commonness (aka relative abundance)
#
# General steps at each portfolio level:
#   1. Calculate the total biomass at the portfolio level
#   2. Remove cases where total biomass = 0. These are cases where a given species was not present in the portfolio in question. 
#   3. Calculate the proportional abundance of each species within each portfolio
#   4. Multiply portfolio-level proportional abundance by SYSTEM LEVEL PROPORTIONAL ABUNDANCE
#   5. Take the portfolio level average of the portfolio-proportional-abundance-weighted proportional abundance
#
# This file also:
#   Plots the distribution of "commonness" across portfolios
#   Conducts means tests across CFR, catch, consumed and sold: commonness
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
  mutate(pa_sys = biomass/total_biomass_biom) %>% 
  left_join(scode, by = "scode_biom") %>% 
  select(scode_ccm, scode_biom, pa_sys)

#------------------------------------------------------------------------------# 
# CFR LEVEL
#------------------------------------------------------------------------------# 

# calculate proportional abundance at CFR level 
pa_cfr <- b %>%
  ungroup() %>% 
  group_by(cfrid, scode_biom) %>% 
  summarize(biomass = sum(totalweight_biom)) %>% 
  mutate(pa_cfr = biomass/total_biomass_biom)

commonness_cfr <- pa_cfr %>% 
  left_join(pa_sys, by = "scode_biom") %>% 
  mutate(commonness = pa_sys * pa_cfr,
         type = "cfr") %>% 
  drop_na(pa_sys) %>% 
  group_by(cfrid, type) %>% 
  summarise(commonness = mean(commonness, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(hhid = 0)

commonness_cfr %>% 
  ggplot(aes(x = log(commonness))) + 
  geom_histogram()


#------------------------------------------------------------------------------# 
# CATCH LEVEL
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

commonness_catch <- pa_catch %>% 
  left_join (pa_sys, by = "scode_ccm") %>% 
  mutate(commonness = pa_sys * pa_catch,
         type = "catch") %>% 
  drop_na(pa_sys) %>% 
  group_by(hhid, type) %>% 
  summarise(commonness = mean(commonness, na.rm = TRUE))  %>% 
  ungroup() 

commonness_catch %>% 
  ggplot(aes(x = log(commonness))) + 
  geom_histogram()

#------------------------------------------------------------------------------# 
# CONSUMPTION LEVEL
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

commonness_cons <- pa_cons %>% 
  left_join (pa_sys, by = "scode_ccm") %>% 
  mutate(commonness = pa_sys * pa_cons,
         type = "cons") %>% 
  drop_na(pa_sys) %>% 
  group_by(hhid, type) %>% 
  summarise(commonness = mean(commonness, na.rm = TRUE)) %>% 
  ungroup() 

commonness_cons %>% 
  ggplot(aes(x = log(commonness))) + 
  geom_histogram()


#------------------------------------------------------------------------------# 
# SOLD LEVEL
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

commonness_sold <- pa_sold %>% 
  left_join (pa_sys, by = "scode_ccm") %>% 
  mutate(commonness = pa_sys * pa_sold,
         type = "sold") %>% 
  drop_na(pa_sys)  %>% 
  group_by(hhid, type) %>% 
  summarise(commonness = mean(commonness, na.rm = TRUE)) %>% 
  ungroup() 

commonness_sold %>% 
  ggplot(aes(x = log(commonness))) + 
  geom_histogram()


#------------------------------------------------------------------------------# 
# COMBINE FILES
#------------------------------------------------------------------------------# 

commonness <- commonness_sold %>% 
  rbind(commonness_catch) %>% 
  rbind(commonness_cons) %>% 
    mutate(cfrid = 0) %>% 
  rbind(commonness_cfr) 


table(commonness$type)


#------------------------------------------------------------------------------# 
# BOXPLOT
#------------------------------------------------------------------------------# 

# sys <- log(tl_system +1) # system total length

order <-c("cfr", "catch", "cons", "sold" )

commonness %>% 
  # mutate(type = factor(type, levels = c("cfr", "catch", "cons", "sold" ))) %>% 
  ggplot(aes(x = type, y = commonness, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.7, alpha = 0.4, width = 0.2, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.7) +
  scale_y_continuous(trans = "log1p") + #scale_y_log10() +
  coord_cartesian(ylim = c(0, 0.00007)) + # zooming the plot in
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        plot.caption = element_text(hjust = 0)) +
  ylab("Mean commonness index") +
  labs(title = "Mean commonness index by portfolio type",
       caption = "Figure is zoomed to show detail, truncating max values of household sold mean total length. \nWhite diamonds depict means.")

ggsave(path = "output/20221104/figures", "commonness_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)


#--------- Test mean differences in commonness -------------------------------*/

# convert CFR biom data to household level
cfrdata <- cfrid_to_hhid %>% 
  left_join(commonness_cfr, by = "cfrid") %>% 
  mutate(hhid = hhid.x) %>% 
  select(hhid, commonness, type)

# generate a sold data file that contains zero values for commonness for households that didn't sell fish
sold413 <- commonness_sold %>% 
  full_join(cfrid_to_hhid, by = "hhid") %>% 
  mutate(commonness = replace_na(commonness, 0),
         type = "sold") %>% 
  select(-cfrid)


# combine data files
data <- commonness %>% 
  select(commonness, type, hhid) %>% 
  filter(type != "cfr")   %>% 
  rbind(cfrdata) %>% 
  as_tibble() 

# compare differences with n = 413, replacing nd_score = 0 for households that did not sell fish.
t413 <- data %>% 
  filter(type != "sold") %>% 
  rbind(sold413) %>% 
  pairwise_t_test(commonness ~ type, paired = TRUE, p.adjust.method = "bonferroni")

# compare differences with n = 266, dropping all households that did not sell fish
t266 <- data %>% 
  pivot_wider(names_from = "type", values_from = "commonness") %>% 
  filter(sold > 0) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "commonness") %>% 
  pairwise_t_test(commonness ~ type, paired = TRUE, p.adjust.method = "bonferroni")

# export
t413 %>% rbind(t266) %>% 
  write.csv(., file = "output/20221104/tables/commonness_ttest.csv")


