# Liz Bageant
## November 22, 2022


#------------------------------------------------------------------------------# 
#
#  CAMBODIA BIODIVERSITY PAPER ANALYSIS--Commonness (aka relative abundance)
#
# General steps at each portfolio level:
#   1. Calculate the total biomass at the portfolio level
#   2. Remove cases where total biomass = 0. These are cases where a given species was not present in the portfolio in question. 
#   3. Calculate the proportional abundance of each species within each portfolio
#   4. Multiply portfolio-level proportional abundance by CFR-level proportional abundance (as of 11/11/ 2022)
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


#------------------------------------------------------------------------------# 
# CFR LEVEL
#------------------------------------------------------------------------------# 

# calculate proportional abundance at CFR level. We will use this in calculations at other levels. 
pa_cfr <- b %>%
  ungroup() %>% 
  group_by(cfrid, scode_biom) %>% 
  summarise(biomass = sum(totalweight_biom)) %>% 
  mutate(pa_cfr = biomass/total_biomass_biom) %>% 
  left_join(scode, by = "scode_biom")

commonness_cfr <- pa_cfr %>% 
  mutate(commonness = pa_cfr * pa_cfr,
         type = "cfr") %>% 
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
  left_join (pa_cfr, by = "scode_ccm") %>% 
  mutate(commonness = pa_cfr * pa_catch,
         type = "catch") %>% 
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
  left_join (pa_cfr, by = "scode_ccm") %>% 
  mutate(commonness = pa_cfr * pa_cons,
         type = "cons") %>% 
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
  left_join (pa_cfr, by = "scode_ccm") %>% 
  mutate(commonness = pa_cfr * pa_sold,
         type = "sold") %>% 
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
  rbind(commonness_cfr) %>% 
  # fix cfrid 
  left_join(cfrid_to_hhid, by = "hhid") %>% 
  mutate(cfrid.y = replace_na(cfrid.y, 0)) %>% 
  mutate(cfrid = cfrid.x + cfrid.y) %>% 
  select(type, commonness, hhid, cfrid)

write.csv(commonness, file = "data/processed/commonness.csv") 

table(commonness$type)


#------------------------------------------------------------------------------# 
# BOXPLOT
#------------------------------------------------------------------------------# 

order <-c("cfr", "catch", "cons", "sold" )

# Original boxplot
commonness %>% 
  ggplot(aes(x = type, y = commonness, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.7, alpha = 0.4, width = 0.2, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.7) +
  scale_y_continuous(trans = "log10") +
  #scale_y_continuous(trans = "log1p") + # this doesnt' seem to do anything
  #coord_cartesian(ylim = c(0, 0.00004)) + # zooming the plot in
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        plot.caption = element_text(hjust = 0)) +
  ylab("Mean commonness index") +
  labs(title = "Mean commonness index by portfolio type",
  caption = "
      White diamonds depict means. Means differences are significant between all groups 
      except CFR and sold (Paired t-tests with Bonferroni correction)")

path <- paste("output/",output_date,"/figures/",sep="")
ggsave(path = path, "commonness_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)



# Boxplot that maps CFR commonness to colors
cfr_colors <- commonness %>% 
  filter(type == "cfr") %>% 
  mutate(cfr_colors = commonness) %>% 
  select(cfrid, cfr_colors)

commonness %>% 
  arrange(-commonness) %>% 
  left_join(cfr_colors, by = "cfrid") %>% 
  ggplot(aes(x = type, y = commonness)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = cfr_colors), alpha = 0.7, width = 0.2) +
  scale_color_viridis(option = "C", name = "CFR \ncommonness") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  ylab("Commonness") +
  labs(title = "Commonness by portfolio type")

path <- paste("output/",output_date,"/figures/",sep="")
ggsave(path = path, "commonness_boxplot_cfr.png", width = 16, height =  12, units = "cm", dpi = 320)


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

  ## Export for use in stata
  data %>% 
    pivot_wider(names_from = "type", values_from = "commonness") %>% 
    write.csv(., file = "data/processed/commonness_hh_level.csv")

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
csv_name <- paste("output/",output_date,"/tables/ttests/commonness_ttest.csv",sep="")

t413 %>% rbind(t266) %>% 
  write.csv(., file = csv_name)




#--------- Scatter plots of commonness index ---------------------------------*/

com_cfr <- commonness_cfr %>% 
  left_join(cfrid_to_hhid, by = "cfrid") %>%
  mutate(com_cfr = commonness,
         hhid = hhid.y) %>% 
  select(hhid, com_cfr)
com_catch <- commonness_catch %>% 
  mutate(com_catch = commonness) %>% 
  select(hhid, com_catch)
com_cons <- commonness_cons %>% 
  mutate(com_cons = commonness) %>% 
  select(hhid, com_cons)
com_sold <- commonness_sold %>% 
  mutate(com_sold = commonness) %>% 
  select(hhid, com_sold)

com <- com_cfr %>% 
  left_join(com_catch, by = "hhid") %>% 
  left_join(com_cons, by = "hhid") %>% 
  left_join(com_sold, by = "hhid") %>% 
  mutate(com_sold = replace_na(com_sold, 0))


# Plot

path <- paste("output/",output_date,"/figures/scatter_plots",sep="")

# system X catch
com %>% 
  ggplot(aes(x = com_cfr, y = com_catch)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Commonness: Biomonitoring X Catch") +
  xlab("Commonness of biomonitoring (CFR level)") +
  ylab("Commonness of species caught (HH level)")

ggsave(path = path, "com_systemXcatch.png", width = 16, height =  12, units = "cm", dpi = 320)

# catch X consumption
com %>% 
  ggplot(aes(x = com_catch, y = com_cons)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Commonness: Catch X Consumption") +
  xlab("Commonness of catch (HH level)") +
  ylab("Commonness of consumed (HH level)")

ggsave(path = path, "com_catchXcons.png", width = 16, height =  12, units = "cm", dpi = 320)


# catch X sold
com %>% 
  filter(com_sold != 0) %>% 
  ggplot(aes(x = com_catch, y = com_sold)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Commonness: Catch X Sold (sold = 0 removed)") +
  xlab("Commonness of catch (HH level)") +
  ylab("Commonness of sold (HH level)")

ggsave(path = path, "com_catchXsold.png", width = 16, height =  12, units = "cm", dpi = 320)


# system X consumption
com %>% 
  ggplot(aes(x = com_cfr, y = com_cons)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Commonness: Biomonitoring X Consumption") +
  xlab("Commonness of biomonitoring (CFR level)") +
  ylab("Commonness of consumed (HH level)")

ggsave(path = path, "com_systemXcons.png", width = 16, height =  12, units = "cm", dpi = 320)

# system X sold

com %>% 
  filter(com_sold != 0) %>% 
  ggplot(aes(x = com_cfr, y = com_sold)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Commonness: Biomonitoring X Sold (sold = 0 removed)") +
  xlab("Commonness of biomonitoring (CFR level)") +
  ylab("Commonness of sold (HH level)")

ggsave(path = path, "com_systemXsold.png", width = 16, height =  12, units = "cm", dpi = 320)




