# Liz Bageant
# November 17, 2022


# Calculating the percentage and amount of catch in our data that comes from top 20 commercial species

x <- cs %>% 
  filter(!is.na(scode)) %>% 
  mutate(scode_ccm = scode) %>% 
  full_join(c, by = "scode_ccm") %>% 
  mutate(flag = if_else(is.na(commercial_flag), 0, 1))

table(x$flag)

total_catch <- x %>% 
  summarise(total_kg = sum(catch_iweight)) %>% 
  as.numeric()

commercial_catch <- x %>% 
  group_by(flag) %>% 
  summarise(catch_kg = sum(catch_iweight),
            catch_pct = catch_kg/total_catch)


species <- x %>% 
  distinct(scode_ccm, flag) 
table(species$flag)


# Number of species in our sample caught, consumed, sold and processed

# caught
c %>% 
  filter(catch_iweight > 0) %>% # no change in N as expected
  distinct(scode_ccm) %>% 
  nrow()

#consumed
c %>% 
  filter(atefresh_iweight > 0) %>% 
  distinct(scode_ccm) %>% 
  nrow()

#sold 
c %>% 
  filter(soldfresh_iweight > 0) %>% 
  distinct(scode_ccm) %>% 
  nrow()

# processed
c %>% 
  filter(process_iweight > 0) %>% 
  distinct(scode_ccm) %>% 
  nrow()
    
  
