## Liz Bageant
## November 21, 2022

#------------------------------------------------------------------------------# 
#
#  CONSTRUCTING BODY SIZE FIGURES
#
# General steps at each portfolio level:
#   1. Calculate the total biomass at the portfolio level
#   2. Remove cases where total biomass = 0. These are cases where a given species was not present in the portfolio in question. 
#   3. Calculate the proportional abundance of each species within each portfolio
#   4. Multiply portfolio-level proportional abundance by total length
#   5. Take the portfolio level SUM of the proportional-abundance-weighted total length
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
# pa_sys <- b %>% 
#   ungroup() %>% 
#   group_by(scode_biom) %>% 
#   summarise(biomass = sum(totalweight_biom)) %>% 
#   mutate(pa_sys = biomass/total_biomass_biom)

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

# calculate total biomass at the CFR level
total_biomass_cfr <- b %>% 
  group_by(cfrid) %>% 
  summarise(total = sum(totalweight_biom))

# calculate proportional abundance at CFR level 
pa_cfr <- b %>%
  ungroup() %>% 
  group_by(cfrid, scode_biom) %>% 
  summarise(biomass = sum(totalweight_biom)) %>% 
  left_join(total_biomass_cfr, by = "cfrid")  %>%
  mutate(pa_cfr = biomass/total) 


# calculate mean total length at CFR level weighted by biomass
tl_cfr <- pa_cfr %>% 
  left_join(., select(ccm_traits_specieslevel, c(tl, scode_biom)), by = "scode_biom") %>% 
  ungroup() %>% 
  mutate(tl_weighted = tl* pa_cfr) %>% 
  group_by(cfrid) %>% 
  summarise(tl_cfr = sum(tl_weighted, na.rm = TRUE)) %>% 
  mutate(type = "cfr",
         tl = tl_cfr, 
         hhid = 0) %>% 
  select(type, tl, hhid, cfrid)


#------------------------------------------------------------------------------# 
# HOUSEHOLD LEVEL CATCH
#------------------------------------------------------------------------------# 

# total catch biomass for calculating proportional abundance
# total_biomass_catch <- c %>%
#   summarise(biomass = sum(catch_iweight)) %>% 
#   as.numeric()

total_biomass_catch <- c %>% 
  group_by(hhid) %>% 
  summarise(total = sum(catch_iweight))


# calculate proportional abundance of caught species
pa_catch <- c %>% 
  ungroup() %>% 
  group_by(hhid, scode_ccm) %>% 
  summarise(biomass = sum(catch_iweight)) %>% 
  left_join(total_biomass_catch, by = "hhid")  %>%
  filter(biomass != 0) %>%  # if there are species that are never caught, this removes them (but there aren't)
  mutate(pa_catch = biomass/total)

# calculate mean total length of caught spcies weighted by biomass
tl_catch <- pa_catch %>% 
  left_join(., select(ccm_traits_specieslevel, c(tl, scode_ccm)), by = "scode_ccm") %>% 
  ungroup() %>% 
  mutate(tl_weighted = tl* pa_catch) %>% 
  group_by(hhid) %>% 
  summarise(tl_catch = sum(tl_weighted, na.rm = TRUE)) %>% 
  mutate(type = "catch",
         tl = tl_catch) %>% 
  select(type, tl, hhid)


#------------------------------------------------------------------------------# 
# HOUSEHOLD LEVEL CONSUMPTION
#------------------------------------------------------------------------------# 

# total consumption biomass
# total_biomass_cons <- c %>% 
#   summarise(biomass = sum(catch_iweight)) %>% 
#   as.numeric()

total_biomass_cons <- c %>%  
  group_by(hhid) %>% 
  summarise(total = sum(atefresh_iweight))

# proportional abundance of consumed species
pa_cons <- c %>% 
  ungroup() %>% 
  group_by(hhid, scode_ccm) %>% 
  summarise(biomass = sum(atefresh_iweight)) %>%
left_join(total_biomass_cons, by = "hhid")  %>%
  filter(biomass != 0) %>%  # remove species that are not eaten (655 cases)
  mutate(pa_cons = biomass/total) 

# calculate weighted sum total length of consumed species weighted by biomass
tl_cons <- pa_cons %>% 
  left_join(., select(ccm_traits_specieslevel, c(tl, scode_ccm)), by = "scode_ccm") %>% 
  ungroup() %>% 
  mutate(tl_weighted = tl* pa_cons) %>% 
  group_by(hhid) %>% 
  summarise(tl_cons = sum(tl_weighted, na.rm = TRUE)) %>% 
  mutate(type = "cons",
         tl = tl_cons) %>% 
  select(type, tl, hhid) 


#------------------------------------------------------------------------------# 
# HOUSEHOLD LEVEL SOLD
#------------------------------------------------------------------------------# 


# total biomass sold
total_biomass_sold <- c %>% 
  group_by(hhid) %>% 
  summarise(total = sum(soldfresh_iweight))


# proportional abundance of sold species
pa_sold <- c %>% 
  ungroup() %>% 
  group_by(hhid, scode_ccm) %>% 
  summarise(biomass = sum(soldfresh_iweight)) %>% 
  left_join(total_biomass_sold, by = "hhid")  %>%
  filter(biomass != 0) %>%  # removing all cases where certain species were never sold. 
  mutate(pa_sold = biomass/total)

# calculate mean total length of consumed species weighted by biomass
tl_sold <- pa_sold %>% 
  left_join(., select(ccm_traits_specieslevel, c(tl, scode_ccm)), by = "scode_ccm") %>% 
  ungroup() %>% 
  mutate(tl_weighted = tl*pa_sold) %>% 
  group_by(hhid) %>% 
  summarise(tl_sold = sum(tl_weighted, na.rm = TRUE)) %>% 
  mutate(type = "sold",
         tl = tl_sold) %>% 
  select(type, tl, hhid) 


test <- pa_sold %>% arrange(hhid)
#------------------------------------------------------------------------------# 
# COMBINE FILES
#------------------------------------------------------------------------------# 

body_size <- tl_catch %>% 
  rbind(tl_cons) %>% 
  rbind(tl_sold) %>% 
  mutate(cfrid = 0) %>% 
  rbind(tl_cfr) %>% 
  filter(tl != 0) %>%  # there is one value where tl = 0. This is a case where the household sold no fish. 
  # fix cfrid 
  left_join(cfrid_to_hhid, by = "hhid") %>%   
  mutate(cfrid.y = replace_na(cfrid.y, 0)) %>% 
  mutate(cfrid = cfrid.x + cfrid.y) %>% 
  select(type, tl, hhid, cfrid)
  
  



#------------------------------------------------------------------------------# 
# BOXPLOT
#------------------------------------------------------------------------------# 

# sys <- log(tl_system +1) # system total length

order <-c("cfr", "catch", "cons", "sold" )

# Original boxplot
body_size %>% 
  ggplot(aes(x = type, y = tl, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.7, alpha = 0.4, width = 0.2, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        plot.caption = element_text(hjust = 0)) +
  ylab("Weighted total length") +
  labs(title = "Biomass-weighted total length by portfolio type",
       caption = "
       White diamonds depict means. Means differences are significant between all groups except: 
       catch and consumed; CFR and sold. (Paired t-tests with Bonferroni correction)")

path <- paste("output/",output_date,"/figures/",sep="")
ggsave(path = path, "body_size_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)

# Boxplot that maps CFR values to colors

cfr_colors <- body_size %>% 
  filter(type == "cfr") %>% 
  mutate(cfr_colors = tl) %>% 
  select(cfrid, cfr_colors)

body_size %>% 
  left_join(cfr_colors, by = "cfrid") %>% 
  ggplot(aes(x = type, y = tl)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = cfr_colors), alpha = 0.7, width = 0.2) +
  scale_color_viridis(option = "C", name = "CFR body \nsize") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  theme_bw() +
  ylab("Body size") +
  labs(title = "Body size by portfolio type")

path <- paste("output/",output_date,"/figures/",sep="")
ggsave(path = path, "body_size_boxplot_cfr.png", width = 16, height =  12, units = "cm", dpi = 320)

# --------- Test mean differences in mean body size ---------------------------#

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

  ## Export file for use in stata
  data %>% 
    pivot_wider(names_from = "type", values_from = "tl") %>% 
    write.csv(., file = "data/processed/body_size_hh_level.csv")

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
csv_name <- paste("output/",output_date,"/tables/ttests/body_size_ttest.csv",sep="")
t413 %>% rbind(t266) %>% 
  write.csv(., file = csv_name)

#------------------------------------------------------------------------------# 
# SCATTERPLOT
#------------------------------------------------------------------------------# 

# Prep data frame

tl_cfr <- tl_cfr %>% 
  select(-hhid) %>% 
  left_join(cfrid_to_hhid, by = "cfrid") %>% 
  mutate(tl_cfr = tl) %>% 
  select(tl_cfr, hhid)
tl_catch <- tl_catch %>% 
  mutate(tl_catch = tl) %>% 
  select(hhid, tl_catch)
tl_cons <- tl_cons %>% 
  mutate(tl_cons = tl) %>% 
  select(hhid, tl_cons)
tl_sold <- tl_sold %>% 
  mutate(tl_sold = tl) %>% 
  select(hhid, tl_sold)

tl <- tl_cfr %>% 
  left_join(tl_catch, by = "hhid") %>% 
  left_join(tl_cons, by = "hhid") %>% 
  left_join(tl_sold, by = "hhid") %>% 
  mutate(tl_sold = replace_na(tl_sold, 0))

# Plot

path <- paste("output/",output_date,"/figures/scatter_plots",sep="")

# system X catch
tl %>% 
  ggplot(aes(x = tl_cfr, y = tl_catch)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Body size: Biomonitoring X Catch") +
  xlab("Body size of biomonitoring (CFR level)") +
  ylab("Body size of species caught (HH level)")

ggsave(path = path, "tl_systemXcatch.png", width = 16, height =  12, units = "cm", dpi = 320)

# catch X consumption
tl %>% 
  ggplot(aes(x = tl_catch, y = tl_cons)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Body size: Catch X Consumption") +
  xlab("Body size of catch (HH level)") +
  ylab("Body size of consumed (HH level)")

ggsave(path = path, "tl_catchXcons.png", width = 16, height =  12, units = "cm", dpi = 320)


# catch X sold
tl %>% 
  filter(tl_sold != 0) %>% 
  ggplot(aes(x = tl_catch, y = tl_sold)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Body size: Catch X Sold (sold = 0 removed)") +
  xlab("Body size of catch (HH level)") +
  ylab("Body size of sold (HH level)")

ggsave(path = path, "tl_catchXsold.png", width = 16, height =  12, units = "cm", dpi = 320)


# system X consumption
tl %>% 
  ggplot(aes(x = tl_cfr, y = tl_cons)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Body size: Biomonitoring X Consumption") +
  xlab("Body size of biomonitoring (CFR level)") +
  ylab("Body size of consumed (HH level)")

ggsave(path = path, "tl_systemXcons.png", width = 16, height =  12, units = "cm", dpi = 320)

# system X sold

tl %>% 
  filter(tl_sold != 0) %>% 
  ggplot(aes(x = tl_cfr, y = tl_sold)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Body size: Biomonitoring X Sold (sold = 0 removed)") +
  xlab("Body size of biomonitoring (CFR level)") +
  ylab("Body size of sold (HH level)")

ggsave(path = path, "tl_systemXsold.png", width = 16, height =  12, units = "cm", dpi = 320)





