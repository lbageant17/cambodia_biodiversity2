# Liz Bageant
# November 9, 2022




#------------------------------------------------------------------------------# 
# Load CSV files generated in Stata
#------------------------------------------------------------------------------# 


# cfr characteristics
cfrtype <- as_tibble(read.csv("data/processed/cfr_category.csv", na.strings = c("", "NA")))


# all dates
dates <- as_tibble(read.csv("data/processed/dates.csv", na.strings = c("", "NA")))

# species codes
scode <- as_tibble(read.csv("data/processed/species_codes.csv", na.strings = c("", "NA")))

# catch data
c <- as_tibble(read.csv("data/processed/ccm_q7.csv", na.strings = c("", "NA")))

# biomonitoring data
b <- as_tibble(read.csv("data/processed/biom_formb.csv", na.strings = c("", "NA")))

# traits data
t <- as_tibble(read.csv("data/processed/traits.csv", na.strings = c("", "NA")))
# merge codes into 

# effort data
e <- as_tibble(read.csv("data/processed/effort.csv", na.strings = c("", "NA")))

# livelihood data
l <- as_tibble(read.csv("data/processed/livelihood.csv", na.strings = c("", "NA"))) %>% 
  filter(year == 2012)


#------------------------------------------------------------------------------# 
# Create core data frames for subsequent analyses
#------------------------------------------------------------------------------# 

#### ----- Household level data files ------ #### 

# Create a household-level data frame that contains the number of species caught by each household
hh_catch <- c %>% 
  select(hhid, cfrid, scode_ccm, catch_iweight) %>% 
  filter(catch_iweight != 0) %>% 
  distinct(hhid, cfrid, scode_ccm) %>%   # nrow(hh_catch) 9687
  group_by(hhid, cfrid) %>% 
  summarise(catch_species = n())


# Create household-level data frame that contains the number of species consumed and sold by households
hh_cons <- c %>% 
  filter(atefresh == 1) %>% 
  distinct(hhid, cfrid, scode_ccm, atefresh) %>% # nrow(hh_consumption) 9032
  group_by(cfrid, hhid) %>% 
  summarise(consumption_species = sum(atefresh))

hh_sold <- c %>% 
  filter(soldfresh == 1) %>% 
  distinct(hhid, cfrid, scode_ccm, soldfresh) %>% # nrow(hh_sold) = 1879
  group_by(cfrid, hhid) %>% 
  summarise(sold_species = sum(soldfresh))

hh_soldcons <- full_join(hh_cons, hh_sold, by = c("cfrid", "hhid")) %>% 
  mutate(sold_species = replace_na(sold_species, 0)) ## replacing 146 NA values with zero

# check NAs
sum(is.na(hh_soldcons$sold_species))
sum(is.na(hh_soldcons$consumption_species))

## Create a household-level effort data frame
effort <- e %>% 
  group_by(hhid) %>% 
  summarize(effort = sum(effort_persondays))

## Create a household-species level data frame that contains the amount of each species caught, consumed, sold, processed etc by household

hh_ccm <- c %>% 
  group_by(hhid, scode_ccm) %>% 
  summarise(catch = sum(catch_iweight),
            eat = sum(atefresh_iweight),
            sold = sum(soldfresh_iweight),
            process = sum(process_iweight),
            lost = sum(lost_iweight),
            other = sum(other_iweight))


#### ----- CFR level data files ------ ####   

# Create a CFR-level data frame that contains the number of species caught during CFR biomonitoring
cfr_biom <- b %>% 
  select(cfrid, scode_biom) %>% 
  distinct(cfrid, scode_biom) %>% # dropping duplicates in terms of CFR and species
  group_by(cfrid) %>% 
  summarise(biom_species = n())


## Combine CFR-level data frames
cfr_biodiv <- full_join(cfr_biom, hh_catch, by = "cfrid") 
cfr_biodiv <- full_join(cfr_biodiv, hh_soldcons, by = c("cfrid", "hhid"))


#### ----- Species level data files ------ #### 

# Create a species-level data frame that contains relative abundance of different species
systemtotal <- b %>% 
  select(totalweight_biom) %>% 
  summarize(systemtotal = sum(totalweight_biom)) %>% 
  as.numeric()

rel_abundance <- b %>% 
  select(scode_biom, totalweight_biom) %>% 
  group_by(scode_biom) %>% 
  summarize(species_total = sum(totalweight_biom)) %>% 
  mutate(rel_abundance = species_total/systemtotal) %>%  # check: summarize(total = sum(rel_abundance)) = 1
  select(scode_biom, rel_abundance) %>% 
  mutate(scode_ccm = scode_biom) # set up key for later merge

# Combine traits and CCM data
ccm_traits <- t %>% 
  mutate(sname_ccm = sname_traits) %>% # unique(ccm_traits$sname_traits) 126 species
  full_join(c, key = "sname_traits") %>%  # unique(ccm_traits$sname_traits) 126 species + NA  
  select(cfrid, hhid, year, month, scode_ccm, sname_traits, soldfresh, atefresh, process, lost, other, ends_with("iweight"), tg, tl, ends_with("content"), ends_with("minbio"), ends_with("rda_100g"), starts_with("num_rda")) %>% 
  drop_na(cfrid, hhid) # Dropping cases where these key identifiers are missing. Allowing missing data in traits vectors.

# collapse ccm_traits data to species level 
ccm_traits_specieslevel <- ccm_traits %>%
  # generate RDA variables per nutrient. 1 = 100g of a given species meets 100% of the RDA for that nutrient.
  mutate(vita_rda100 = if_else(vita_rda_100g > 1, 1, 0),
         pr_rda100 = if_else(pr_rda_100g > 1, 1, 0),
         fe_rda100 = if_else(fe_rda_100g > 1, 1, 0),
         o3_rda100 = if_else(o3_rda_100g > 1, 1, 0),
         zn_rda100 = if_else(zn_rda_100g > 1, 1, 0),
         ca_rda100 = if_else(ca_rda_100g > 1, 1, 0)) %>% 
  group_by(sname_traits, scode_ccm) %>% 
  summarise(catch = sum(catch_iweight), 
            eat = sum(atefresh_iweight), 
            sell = sum(soldfresh_iweight),
            process = sum(process_iweight),
            # Calculate number of RDAs met per 100g of a given species at 100% of RDA and 50% of RDA. 
            num_rda100 = mean(num_rda_100), 
            num_rda50 = mean(num_rda_50),
            tl = mean(tl),
            log_tl = log(tl),
            pr_rda100 = max(pr_rda100),
            vita_rda100 = max(vita_rda100),
            fe_rda100 = max(fe_rda100),
            o3_rda100 = max(o3_rda100),
            zn_rda100 = max(zn_rda100),
            ca_rda100 = max(ca_rda100))


# Combine species-level traits, ccm and relative abundance data
ccm_traits_specieslevel <- full_join(ccm_traits_specieslevel,rel_abundance, by = "scode_ccm")

# Looking for NA values in number of species. If there are NA values, they should be replaced with zero.
sum(is.na(cfr_biodiv$biom_species))
sum(is.na(cfr_biodiv$consumption_species))
sum(is.na(cfr_biodiv$sold_species))
sum(is.na(cfr_biodiv$catch_species))

# What is the number of unique species present in each data frame?

numspecies_ccm <- c %>% 
  distinct(scode_ccm) %>% 
  nrow()

numspecies_traits <- t %>% 
  distinct(sname_traits) %>% 
  nrow()

numspecies_biom <- b %>% 
  distinct(sname_biom) %>% 
  nrow()

#### ----- Key file ------ ####   

# Create a key that matches HH IDs to CFR IDs
cfrid_to_hhid <- c %>% 
  select(cfrid, hhid) %>% 
  unique()
