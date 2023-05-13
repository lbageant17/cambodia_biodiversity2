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



#--------- Box plot at household level ---------------------------------------------*/

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


# Number of species plot at household level
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

path <- paste("output/",output_date,"/figures/",sep="")
ggsave(path = path, "species_count_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)


#--------- Box plot with CFR-level values mapped to colors -------------------*/

# make a household-level variable that represents CFR-level species richness to map to colors
cfr_colors <- cfr_biodiv_long %>% 
  filter(type == "cfr") %>% 
  select(-hhid, -type) %>% 
  # expand to household level
  #left_join(cfrid_to_hhid, by = "cfrid") %>% 
  rename(cfr_colors = species)
  
  
# merge cfr_colors into main data file
plotfile <- cfr_biodiv_long %>% 
  # bring in cfrids where missing
  left_join(cfrid_to_hhid, by = "hhid") %>% 
  mutate(cfrid.y = replace_na(cfrid.y, 0)) %>% 
  mutate(cfrid = cfrid.x + cfrid.y) %>% 
  select(hhid, type, species, cfrid) %>% 
  left_join(cfr_colors, by = "cfrid")

plotfile %>% 
  ggplot(aes(x = type, y = species)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = cfr_colors), alpha = 0.7, width = 0.2) +
  scale_color_viridis(option = "C", name = "CFR species \nrichness") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  #scale_fill_viridis(discrete = TRUE, alpha = 0.7, option = "C") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  theme_bw() +
  ylab("Species richness") +
  labs(title = "Species richness by portfolio type")

path <- paste("output/",output_date,"/figures/",sep="")
ggsave(path = path, "species_boxplot_cfr.png", width = 16, height =  12, units = "cm", dpi = 320)


#--------- Test mean differences in number of species (hh level) -------------*/

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

#--------- Scatter plots -----------------------------------------------------*/

path <- paste("output/",output_date,"/figures/scatter_plots",sep="")

# system X catch
cfr_biodiv %>% 
  ggplot(aes(x = biom_species, y = catch_species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Number of species: Biomonitoring X Catch") +
  xlab("Number of species in biomonitoring (CFR level)") +
  ylab("Number of species caught (HH level)")

ggsave(path = path, "species_systemXcatch.png", width = 16, height =  12, units = "cm", dpi = 320)

  # calculate r-squared, slope and p-value
  m <- lm(cfr_biodiv$catch_species ~ cfr_biodiv$biom_species)
  summary(m)
  # slope = 0.34447
  # p = 2.04e-15
  # r2 = 0.14
 
    
# catch X consumption
cfr_biodiv %>% 
  ggplot(aes(x = catch_species, y = consumption_species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Number of species: Catch X Consumption") +
  xlab("Number of species in catch (HH level)") +
  ylab("Number of species consumed (HH level)")

ggsave(path = path, "species_catchXcons.png", width = 16, height =  12, units = "cm", dpi = 320)
  
  # calculate r-squared, slope and p-value
  m <- lm(cfr_biodiv$consumption_species ~ cfr_biodiv$catch_species)
  summary(m)
  # slope = 0.899111
  # p = 2e-16
  # r2 = 0.9523

# catch X sold
cfr_biodiv %>% 
  ggplot(aes(x = catch_species, y = sold_species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Number of species: Catch X Sold") +
  xlab("Number of species in catch (HH level)") +
  ylab("Number of species sold (HH level)")

ggsave(path = path, "species_catchXsold.png", width = 16, height =  12, units = "cm", dpi = 320)

  # calculate r-squared, slope and p-value
  m <- lm(cfr_biodiv$sold_species ~ cfr_biodiv$catch_species)
  summary(m)
  # slope = 0.27129
  # p = <2e-16
  # r2 = 0.2593


# system X consumption
cfr_biodiv %>% 
  ggplot(aes(x = biom_species, y = consumption_species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Number of species: Biomonitoring X Consumption") +
  xlab("Number of species in biomonitoring (CFR level)") +
  ylab("Number of species consumed (HH level)")

ggsave(path = path, "species_systemXcons.png", width = 16, height =  12, units = "cm", dpi = 320)



# system X sold

cfr_biodiv %>% 
  ggplot(aes(x = biom_species, y = sold_species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Number of species: Biomonitoring X Sold") +
  xlab("Number of species in biomonitoring (CFR level)") +
  ylab("Number of species sold (HH level)")

ggsave(path = path, "species_systemXsold.png", width = 16, height =  12, units = "cm", dpi = 320)


  
#--------- Look at number of species at each level (venn diagram situation) ---#

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
  



