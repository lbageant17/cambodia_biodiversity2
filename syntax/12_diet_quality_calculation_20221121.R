## Liz Bageant
## November 8, 2022

#------------------------------------------------------------------------------# 
#
#  CAMBODIA BIODIVERSITY PAPER ANALYSIS--Diet quality
#
#  This file does the following: 
#    Calculates diet quality measures: RDAs met, minimum biomass, nutrient density score
#    Box plots and scatter plots of nutrient density score
#    Conducts means tests across CFR, catch, consumed and sold: Nutrient density score
#
#------------------------------------------------------------------------------# 


# Set RDA threshold (percentage of RDA met for a given nutrient) to apply to the whole file

rda_threshold <- 1 


#------------------------------------------------------------------------------# 
#  Set up data files for diet quality calculations                          #### 
#------------------------------------------------------------------------------# 

  # RDA thresholds by nutrient
  nutrient <- c("protein_g", "iron_mg", "zinc_mg", "calcium_mg", "vitamina_mcg", "omega3_mg")
  rda_under5 <- c(13, 7.55, 4.1, 450, 400, 285) %>% 
    as.data.frame() %>% 
    mutate(threshold = .*rda_threshold) %>% # applying RDA threshold set above
    select(threshold)
  rda <- data.frame(nutrient, rda_under5) %>% 
    pivot_wider(names_from = "nutrient", values_from = "threshold")

  # nutrient content by species
  nutr_content_by_species <- t %>% 
    select(sname_traits, contains("_content")) %>% 
    # merge scodes into this file
    left_join(scode, by = "sname_traits") %>%
    # drop cases where content value is missing for any nutrient
    drop_na(contains("_content"))
 
  # total system catch CCM
  total_sys_catch <- c %>% 
    select(scode_ccm, catch_iweight) %>% 
    summarise(total_sys_catch = sum(catch_iweight))
  
  # total system catch BIOM
  total_sys_biom <- b %>% 
    select(scode_biom, totalweight_biom) %>% 
    summarise(total_sys_biom = sum(totalweight_biom))

  # total CFR catch CCM
  total_cfr_ccm <- c %>% 
    select(cfrid, scode_ccm, catch_iweight) %>% 
    group_by(cfrid) %>% 
    summarise(total_cfr_catch = sum(catch_iweight)) 
  
  # total CFR catch BIOM
  total_cfr_biom <- b %>% 
    select(cfrid, scode_biom, totalweight_biom) %>% 
    group_by(cfrid) %>% 
    summarise(total_cfr_catch = sum(totalweight_biom))
 
  # total household catch, consumption, sold
  total_hh <- c %>% 
    select(hhid, scode_ccm, catch_iweight, atefresh_iweight, soldfresh_iweight) %>% 
    group_by(hhid) %>% 
    summarise(total_catch_kg = sum(catch_iweight),
              total_cons_kg = sum(atefresh_iweight),
              total_sold_kg = sum(soldfresh_iweight)) %>% 
    ungroup()
  
#------------------------------------------------------------------------------# 
#------------------------------------------------------------------------------# 
# HOUSEHOLD LEVEL catch, consumption and sale diet quality metrics
#------------------------------------------------------------------------------# 
#------------------------------------------------------------------------------# 
  
#------------------------------------------------------------------------------# 
# CATCH
#------------------------------------------------------------------------------# 
  
  
  #### ----- Calculate proportional abundance by species at the household level ------ ####   
  
  # household catch proportional abundance by species
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
      
  pa_hh_catch %>%  filter(pa == 0) # no zero values
 
     
     
  #### ----- Calculate nutrient concentration per 100g of a household's CATCH portfolio ------ ####    
  
  portfolio_nutrient_concentration <- pa_hh_catch %>% 
    left_join(nutr_content_by_species, by = "scode_ccm") %>%
    # drop cases where content value is missing for any nutrient. These are species that were CAUGHT but for which we have no trait info.
    drop_na(contains("_content")) %>% 
    # calculate the nutrient content relative to proportional abundance of CATCH
    mutate(fe_mg = fe_content*pa,
           pr_g = pr_content*pa,
           zn_mg = zn_content*pa,
           ca_mg = ca_content*pa,
           vita_mcg = vita_content*pa,
           o3_mg = o3_content*pa) %>% 
    select(hhid, scode, pa, pr_g, fe_mg, zn_mg, ca_mg, vita_mcg, o3_mg) %>% 
    # sum nutrients across all species
    group_by(hhid) %>% 
    summarise(pr_g = sum(pr_g),
              fe_mg = sum(fe_mg),
              zn_mg = sum(zn_mg),
              ca_mg = sum(ca_mg),
              vita_mcg = sum(vita_mcg),
              o3_mg = sum(o3_mg)) 

  
  #### ----- Calculate how many RDAs met by 100g of a given fish portfolio using RDA threshold set above ------ #### 

  rda_hh_catch <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% # 
    # compare nutrient concentrations to RDA thresholds
    mutate(pr_rda = if_else(pr_g > protein_g, 1, 0),
           fe_rda = if_else(fe_mg > zinc_mg, 1, 0),
           zn_rda = if_else(zn_mg > zinc_mg, 1, 0),
           ca_rda = if_else(ca_mg > calcium_mg, 1, 0),
           vita_rda = if_else(vita_mcg > vitamina_mcg, 1, 0),
           o3_rda = if_else(o3_mg > omega3_mg, 1, 0),
           # total number of RDAs met by 100g of each HH's portfolio at RDA threshold set above
           rda_hh_catch = pr_rda + fe_rda + zn_rda + ca_rda + vita_rda + o3_rda) %>% 
    select(hhid, rda_hh_catch)
 
  #### ----- Calculate the nutrient density score, or proportion of RDA met by 100g of a given fish portfolio, by nutrient ------ #### 
  
 nd_score_hh_catch <- portfolio_nutrient_concentration %>% 
  # bring in RDA thresholds
  cbind(rda) %>% 
  # compute proportions with a max value of 1
    mutate(pr_proportion = if_else(pr_g > protein_g, 1, pr_g/protein_g),
           fe_proportion = if_else(fe_mg > iron_mg, 1, fe_mg/iron_mg),
           zn_proportion = if_else(zn_mg > zinc_mg, 1, zn_mg/zinc_mg),
           ca_proportion = if_else(ca_mg > calcium_mg, 1, ca_mg/calcium_mg),
           vita_proportion = if_else(vita_mcg > vitamina_mcg, 1, vita_mcg/vitamina_mcg),
           o3_proportion = if_else(o3_mg > omega3_mg, 1, o3_mg/omega3_mg)) %>% 
    # sum acros proportions
    rowwise() %>%
    mutate(nd_score = sum(across(ends_with("_proportion")), na.rm = T)) %>% 
    select(hhid, contains("_proportion"), nd_score) %>% 
    mutate(type = "catch")
    
         
     
  #### ----- Calculate the minimum portion size of a household's portfolio that meets all RDAs ------ ####   

  minbio_hh_catch <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% 
    # calculate minbio by nutrient
    mutate(minbio_pr = (protein_g/pr_g)*100,
           minbio_fe = (iron_mg/fe_mg)*100,
           minbio_zn = (zinc_mg/zn_mg)*100,
           minbio_ca = (calcium_mg/ca_mg)*100,
           minbio_vita = (vitamina_mcg/vita_mcg)*100,
           minbio_o3 = (omega3_mg/o3_mg)*100) %>% 
    select(hhid, contains("minbio")) %>% 
    group_by(hhid) %>% 
    mutate(minbio_hh_catch_all = max(minbio_pr, minbio_fe, minbio_zn, minbio_ca, minbio_vita, minbio_o3)) %>%  # calculate minimum portion size for all 6 nutrients
           # minbio_hh_catch_except_vita = max(minbio_pr, minbio_fe, minbio_zn, minbio_ca, minbio_o3), # calculate minimum portion size for all nutrients except Vitamin A
           # minbio_hh_catch_except_vita_fe = max(minbio_pr, minbio_zn, minbio_ca, minbio_o3)) %>%  # calculate minimum portion size for all nutrients except Vitamin A and Iron
    select(hhid, contains("minbio_hh_catch"))
    

#------------------------------------------------------------------------------# 
# CONSUMPTION
#------------------------------------------------------------------------------#   
  
  #### ----- Calculate proportional abundance by species ------ ####   
  
  # household consumption proportional abundance by species
  pa_hh_cons <- c %>% 
    select(hhid, scode_ccm, atefresh_iweight) %>% 
    # get total consumption by species
    group_by(hhid, scode_ccm) %>% 
    summarise(cons_kg_species = sum(atefresh_iweight)) %>% 
    # bring in total consumption by household
    left_join(total_hh, by = "hhid") %>% 
    # calculate proportional abundance
    mutate(pa = cons_kg_species/total_cons_kg)  %>% 
    # Confirm that proportional abundances total to 1 for each household
    # group_by(hhid) %>% 
    # summarise(should_equal_1 = sum(pa)) ## all values = 1
    select(hhid, scode_ccm, pa)
  
  dim(pa_hh_cons)
  pa_hh_cons %>% filter(pa == 0) # 655 zero values
  
  
  #### ----- Calculate nutrient concentration per 100g of a household's CONSUMPTION portfolio ------ ####    
  
  portfolio_nutrient_concentration <- pa_hh_cons %>% 
    left_join(nutr_content_by_species, by = "scode_ccm") %>%
    # drop cases where content value is missing for any nutrient. These are species that were CONSUMED but for which we have no trait info.
    drop_na(contains("_content")) %>% 
    # calculate the nutrient content relative to proportional abundance of household's CONSUMPTION
    mutate(fe_mg = fe_content*pa,
           pr_g = pr_content*pa,
           zn_mg = zn_content*pa,
           ca_mg = ca_content*pa,
           vita_mcg = vita_content*pa,
           o3_mg = o3_content*pa) %>% 
    select(hhid, scode, pa, pr_g, fe_mg, zn_mg, ca_mg, vita_mcg, o3_mg) %>% 
    # sum nutrients across all species
    group_by(hhid) %>% 
    summarise(pr_g = sum(pr_g),
              fe_mg = sum(fe_mg),
              zn_mg = sum(zn_mg),
              ca_mg = sum(ca_mg),
              vita_mcg = sum(vita_mcg),
              o3_mg = sum(o3_mg)) 
  
  
  #### ----- Calculate how many RDAs are met by 100g of a given fish portfolio using RDA threshold set above ------ #### 
  
  rda_hh_cons <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% # to look at meeting 50% RDA, multiple thresholds by 0.5
    # compare nutrient concentrations to RDA thresholds
    mutate(pr_rda = if_else(pr_g > protein_g, 1, 0),
           fe_rda = if_else(fe_mg > zinc_mg, 1, 0),
           zn_rda = if_else(zn_mg > zinc_mg, 1, 0),
           ca_rda = if_else(ca_mg > calcium_mg, 1, 0),
           vita_rda = if_else(vita_mcg > vitamina_mcg, 1, 0),
           o3_rda = if_else(o3_mg > omega3_mg, 1, 0),
           # total number of RDAs met by 100g of each HH's portfolio at RDA threshold set above
           rda_hh_cons = pr_rda + fe_rda + zn_rda + ca_rda + vita_rda + o3_rda) %>% 
    select(hhid, rda_hh_cons)
  
  #### ----- Calculate the nutrient density score, or proportion of RDA met by 100g of a given fish portfolio, by nutrient ------ #### 
  
  nd_score_hh_cons <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% 
    # compute proportions  
    mutate(pr_proportion = if_else(pr_g > protein_g, 1, pr_g/protein_g),
           fe_proportion = if_else(fe_mg > iron_mg, 1, fe_mg/iron_mg),
           zn_proportion = if_else(zn_mg > zinc_mg, 1, zn_mg/zinc_mg),
           ca_proportion = if_else(ca_mg > calcium_mg, 1, ca_mg/calcium_mg),
           vita_proportion = if_else(vita_mcg > vitamina_mcg, 1, vita_mcg/vitamina_mcg),
           o3_proportion = if_else(o3_mg > omega3_mg, 1, o3_mg/omega3_mg)) %>% 
    # sum across proportions
    rowwise() %>%
    mutate(nd_score = sum(across(ends_with("_proportion")), na.rm = T)) %>% 
    select(hhid, contains("_proportion"), nd_score) %>% 
    mutate(type = "cons")
  

  #### ----- Calculate the minimum portion size of a household's CONSUMPTION portfolio that meets all RDAs ------ ####   
  
  minbio_hh_cons <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% 
    # calculate minbio by nutrient
    mutate(minbio_pr = (protein_g/pr_g)*100,
           minbio_fe = (iron_mg/fe_mg)*100,
           minbio_zn = (zinc_mg/zn_mg)*100,
           minbio_ca = (calcium_mg/ca_mg)*100,
           minbio_vita = (vitamina_mcg/vita_mcg)*100,
           minbio_o3 = (omega3_mg/o3_mg)*100) %>% 
    select(hhid, contains("minbio")) %>% 
    group_by(hhid) %>% 
    mutate(minbio_hh_cons_all = max(minbio_pr, minbio_fe, minbio_zn, minbio_ca, minbio_vita, minbio_o3)) %>% # calculate minimum portion size for all 6 nutrients
           # minbio_hh_cons_except_vita = max(minbio_pr, minbio_fe, minbio_zn, minbio_ca, minbio_o3), # calculate minimum portion size for all nutrients except Vitamin A
           # minbio_hh_cons_except_vita_fe = max(minbio_pr, minbio_zn, minbio_ca, minbio_o3)) %>%  # calculate minimum portion size for all nutrients except Vitamin A and Iron
    select(hhid, contains("minbio_hh_cons"))

#------------------------------------------------------------------------------# 
# SOLD
#------------------------------------------------------------------------------# 
  
  #### ----- Calculate proportional abundance by species ------ ####   
  
  # household SOLD proportional abundance by species
  pa_hh_sold <- c %>% 
    select(hhid, scode_ccm, soldfresh_iweight) %>% 
    # get total SOLD by species
    group_by(hhid, scode_ccm) %>% 
    summarise(sold_kg_species = sum(soldfresh_iweight)) %>% 
    # bring in total SOLD by household
    left_join(total_hh, by = "hhid") %>% 
    # calculate proportional abundance
    mutate(pa = sold_kg_species/total_sold_kg) %>% 
    mutate_at(vars(pa), ~replace(., is.nan(.), 0)) %>% # change all NaN values to zero
      # Confirm that proportional abundances total to 1 for each household
      #group_by(hhid) %>% 
      #summarise(should_equal_1 = sum(pa)) 
      #table(pa_hh_sold$should_equal_1) ## There are 146 households that did not sell any fish ever during the study period..
    select(hhid, scode_ccm, pa)
    
    dim(pa_hh_sold)
    pa_hh_sold %>%  filter(pa == 0) # 7808 zero values
    
    
  #### ----- Calculate nutrient concentration per 100g of a household's SOLD portfolio ------ ####    
  
  portfolio_nutrient_concentration <- pa_hh_sold %>% 
    left_join(nutr_content_by_species, by = "scode_ccm") %>%
    # drop cases where content value is missing for any nutrient. These are species that were SOLD but for which we have no trait info.
    drop_na(contains("_content")) %>% 
    # calculate the nutrient content relative to proportional abundance of household's SOLD
    mutate(fe_mg = fe_content*pa,
           pr_g = pr_content*pa,
           zn_mg = zn_content*pa,
           ca_mg = ca_content*pa,
           vita_mcg = vita_content*pa,
           o3_mg = o3_content*pa) %>% 
    select(hhid, scode, pa, pr_g, fe_mg, zn_mg, ca_mg, vita_mcg, o3_mg) %>% 
    # sum nutrients across all species
    group_by(hhid) %>% 
    summarise(pr_g = sum(pr_g),
              fe_mg = sum(fe_mg),
              zn_mg = sum(zn_mg),
              ca_mg = sum(ca_mg),
              vita_mcg = sum(vita_mcg),
              o3_mg = sum(o3_mg))   
  
  #### ----- Calculate how many RDAs are met by 100g of a given fish portfolio using RDA threshold set above ------ #### 
  
  rda_hh_sold <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% # to look at meeting 50% RDA, multiple thresholds by 0.5
    # compare nutrient concentrations to RDA thresholds
    mutate(pr_rda = if_else(pr_g > protein_g, 1, 0),
           fe_rda = if_else(fe_mg > zinc_mg, 1, 0),
           zn_rda = if_else(zn_mg > zinc_mg, 1, 0),
           ca_rda = if_else(ca_mg > calcium_mg, 1, 0),
           vita_rda = if_else(vita_mcg > vitamina_mcg, 1, 0),
           o3_rda = if_else(o3_mg > omega3_mg, 1, 0),
           # total number of RDAs met by 100g of each HH's portfolio at RDA threshold set above
           rda_hh_sold = pr_rda + fe_rda + zn_rda + ca_rda + vita_rda + o3_rda) %>% 
    select(hhid, rda_hh_sold)

    #### ----- Calculate the nutrient density score, or proportion of RDA met by 100g of a given fish portfolio, by nutrient ------ #### 
    
    nd_score_hh_sold <- portfolio_nutrient_concentration %>% 
      # bring in RDA thresholds
      cbind(rda) %>% 
      # compute proportions with a max value of 1 
      mutate(pr_proportion = if_else(pr_g > protein_g, 1, pr_g/protein_g),
             fe_proportion = if_else(fe_mg > iron_mg, 1, fe_mg/iron_mg),
             zn_proportion = if_else(zn_mg > zinc_mg, 1, zn_mg/zinc_mg),
             ca_proportion = if_else(ca_mg > calcium_mg, 1, ca_mg/calcium_mg),
             vita_proportion = if_else(vita_mcg > vitamina_mcg, 1, vita_mcg/vitamina_mcg),
             o3_proportion = if_else(o3_mg > omega3_mg, 1, o3_mg/omega3_mg)) %>% 
      # sum across proportions
      rowwise() %>%
      mutate(nd_score = sum(across(ends_with("_proportion")), na.rm = T)) %>% 
      select(hhid, contains("_proportion"), nd_score) %>% 
      mutate(type = "sold")
    # NOTE: For households that did not sell fish, nutrient density score = 0
    
    
  #### ----- Calculate the minimum portion size of a household's SOLD portfolio that meets all RDAs ------ ####   
  
  minbio_hh_sold <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% 
    # calculate minbio by nutrient
    mutate(minbio_pr = (protein_g/pr_g)*100,
           minbio_fe = (iron_mg/fe_mg)*100,
           minbio_zn = (zinc_mg/zn_mg)*100,
           minbio_ca = (calcium_mg/ca_mg)*100,
           minbio_vita = (vitamina_mcg/vita_mcg)*100,
           minbio_o3 = (omega3_mg/o3_mg)*100) %>% 
    select(hhid, contains("minbio")) %>% 
    group_by(hhid) %>% 
    mutate(minbio_hh_sold_all = max(minbio_pr, minbio_fe, minbio_zn, minbio_ca, minbio_vita, minbio_o3)) %>%  # calculate minimum portion size for all 6 nutrients
           # minbio_hh_sold_except_vita = max(minbio_pr, minbio_fe, minbio_zn, minbio_ca, minbio_o3), # calculate minimum portion size for all nutrients except Vitamin A
           # minbio_hh_sold_except_vita_fe = max(minbio_pr, minbio_zn, minbio_ca, minbio_o3)) %>%  # calculate minimum portion size for all nutrients except Vitamin A and Iron
    select(hhid, contains("minbio_hh_sold"))      
  
  ## NOTE: For households that did not sell fish (n = 167) all minbio values are "inf".

  
#------------------------------------------------------------------------------# 
# CFR LEVEL USING BIOM DATA
#------------------------------------------------------------------------------#    
  
  #### ----- Calculate proportional abundance by species at the CFR level------ ####   
  
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
    
    pa_cfr %>% filter(pa == 0) # 22 zero values
    
  
  #### ----- Calculate nutrient concentration per 100g of each CFR's portfolio ------ ####    
  
  portfolio_nutrient_concentration <- pa_cfr %>% 
    left_join(nutr_content_by_species, by = "scode_biom") %>%
    # drop cases where content value is missing for any nutrient. These are species that were in the BIOM data but for which we have no trait info.
    drop_na(contains("_content")) %>% 
    # calculate the nutrient content relative to proportional abundance of CFR's portfolio
    mutate(fe_mg = fe_content*pa,
           pr_g = pr_content*pa,
           zn_mg = zn_content*pa,
           ca_mg = ca_content*pa,
           vita_mcg = vita_content*pa,
           o3_mg = o3_content*pa) %>% 
    select(cfrid, scode, pa, pr_g, fe_mg, zn_mg, ca_mg, vita_mcg, o3_mg) %>% 
    # sum nutrients across all species
    group_by(cfrid) %>% 
    summarise(pr_g = sum(pr_g),
              fe_mg = sum(fe_mg),
              zn_mg = sum(zn_mg),
              ca_mg = sum(ca_mg),
              vita_mcg = sum(vita_mcg),
              o3_mg = sum(o3_mg))   
  
  
  #### ----- Calculate how many RDAs are met by 100g of a given fish portfolio using RDA threshold set above ------ #### 
  
  rda_cfr <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% 
    # compare nutrient concentrations to RDA thresholds
    mutate(pr_rda = if_else(pr_g > protein_g, 1, 0),
           fe_rda = if_else(fe_mg > zinc_mg, 1, 0),
           zn_rda = if_else(zn_mg > zinc_mg, 1, 0),
           ca_rda = if_else(ca_mg > calcium_mg, 1, 0),
           vita_rda = if_else(vita_mcg > vitamina_mcg, 1, 0),
           o3_rda = if_else(o3_mg > omega3_mg, 1, 0),
           # total number of RDAs met by 100g of each CFR's portfolio at RDA threshold set above
           rda_cfr = pr_rda + fe_rda + zn_rda + ca_rda + vita_rda + o3_rda) %>% 
    select(cfrid, rda_cfr)

    #### ----- Calculate the nutrient density score, or proportion of RDA met by 100g of a given fish portfolio, by nutrient ------ #### 
    
    nd_score_cfr <- portfolio_nutrient_concentration %>% 
      # bring in RDA thresholds
      cbind(rda) %>% 
      # compute proportions with a max value of 1
      mutate(pr_proportion = if_else(pr_g > protein_g, 1, pr_g/protein_g),
             fe_proportion = if_else(fe_mg > iron_mg, 1, fe_mg/iron_mg),
             zn_proportion = if_else(zn_mg > zinc_mg, 1, zn_mg/zinc_mg),
             ca_proportion = if_else(ca_mg > calcium_mg, 1, ca_mg/calcium_mg),
             vita_proportion = if_else(vita_mcg > vitamina_mcg, 1, vita_mcg/vitamina_mcg),
             o3_proportion = if_else(o3_mg > omega3_mg, 1, o3_mg/omega3_mg)) %>% 
      # sum across proportions
      rowwise() %>%
      mutate(nd_score = sum(across(ends_with("_proportion")), na.rm = T)) %>% 
      select(cfrid, contains("_proportion"), nd_score) %>% 
      mutate(type = "cfr", 
             hhid = 0)
  
  #### ----- Calculate the minimum portion size of each CFR's portfolio that meets all RDAs ------ ####   
  
  minbio_cfr <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% # to look at meeting 50% RDA, multiply thresholds by 0.5 
    # calculate minbio by nutrient
    mutate(minbio_pr = (protein_g/pr_g)*100,
           minbio_fe = (iron_mg/fe_mg)*100,
           minbio_zn = (zinc_mg/zn_mg)*100,
           minbio_ca = (calcium_mg/ca_mg)*100,
           minbio_vita = (vitamina_mcg/vita_mcg)*100,
           minbio_o3 = (omega3_mg/o3_mg)*100) %>% 
    select(cfrid, contains("minbio")) %>% 
    group_by(cfrid) %>% 
    mutate(minbio_cfr = max(minbio_pr, minbio_fe, minbio_zn, minbio_ca, minbio_vita, minbio_o3)) %>%  # calculate minimum portion size for all 6 nutrients
    # minbio_cfr_except_vita = max(minbio_pr, minbio_fe, minbio_zn, minbio_ca, minbio_o3), # calculate minimum portion size for all nutrients except Vitamin A
    # minbio_cfr_except_vita_fe = max(minbio_pr, minbio_zn, minbio_ca, minbio_o3)) %>%  # calculate minimum portion size for all nutrients except Vitamin A and Iron
    select(cfrid, contains("minbio_cfr"))      
  
  
  

#------------------------------------------------------------------------------# 
# SYSTEM LEVEL USING BIOM DATA
#------------------------------------------------------------------------------#   
  

  #### ----- Calculate proportional abundance by species from BIOM data ------ ####   
  
    # system proportional abundance by species using BIOM data
    pa_sys_biom <- b %>% 
      select(scode_biom, totalweight_biom) %>% 
      # get total system amount by species
      group_by(scode_biom) %>% 
      summarise(sys_kg_species = sum(totalweight_biom)) %>%
      mutate(total = as.numeric(total_sys_biom[1,1]), 
             pa = sys_kg_species/total) %>% 
      # Confirm that proportional abundances total to 1 
        # ungroup() %>%      
        # summarise(should_equal_1 = sum(pa)) 
        # table(pa_sys_biom$should_equal_1)
      select(scode_biom, pa) 
  
  #### ----- Calculate nutrient concentration per 100g of the system BIOM portfolio ------ ####    
  
    portfolio_nutrient_concentration <- pa_sys_biom %>% 
      left_join(nutr_content_by_species, by = "scode_biom") %>%
      # drop cases where content value is missing for any nutrient. These are species that are in the CCM data but for which we have no trait info.
      drop_na(contains("_content")) %>% # this leaves 110 species with nutrient content info
      # calculate the nutrient content relative to proportional abundance of the system's BIOM portfolio
      mutate(fe_mg = fe_content*pa,
             pr_g = pr_content*pa,
             zn_mg = zn_content*pa,
             ca_mg = ca_content*pa,
             vita_mcg = vita_content*pa,
             o3_mg = o3_content*pa) %>% 
      select(scode, pa, pr_g, fe_mg, zn_mg, ca_mg, vita_mcg, o3_mg) %>% 
      # sum nutrients across all species
      summarise(pr_g = sum(pr_g),
                fe_mg = sum(fe_mg),
                zn_mg = sum(zn_mg),
                ca_mg = sum(ca_mg),
                vita_mcg = sum(vita_mcg),
                o3_mg = sum(o3_mg))   


  #### ----- Calculate how many RDAs are met by 100g of a given fish portfolio using RDA threshold set above ------ #### 

    rda_sys_biom <- portfolio_nutrient_concentration %>% 
      # bring in RDA thresholds
      cbind(rda) %>% 
      # compare nutrient concentrations to RDA thresholds
      mutate(pr_rda = if_else(pr_g > protein_g, 1, 0),
             fe_rda = if_else(fe_mg > zinc_mg, 1, 0),
             zn_rda = if_else(zn_mg > zinc_mg, 1, 0),
             ca_rda = if_else(ca_mg > calcium_mg, 1, 0),
             vita_rda = if_else(vita_mcg > vitamina_mcg, 1, 0),
             o3_rda = if_else(o3_mg > omega3_mg, 1, 0),
             # total number of RDAs met by 100g of system BIOM portfolio at the RDA threshold set above
             rda_sys_biom = pr_rda + fe_rda + zn_rda + ca_rda + vita_rda + o3_rda) %>% 
      select(rda_sys_biom)


#### ----- Calculate the minimum portion size of the system's BIOM portfolio that meets all RDAs ------ ####   

minbio_sys_biom <- portfolio_nutrient_concentration %>% 
  # bring in RDA thresholds
  cbind(rda) %>% 
  # calculate minbio by nutrient
  mutate(minbio_pr = (protein_g/pr_g)*100,
         minbio_fe = (iron_mg/fe_mg)*100,
         minbio_zn = (zinc_mg/zn_mg)*100,
         minbio_ca = (calcium_mg/ca_mg)*100,
         minbio_vita = (vitamina_mcg/vita_mcg)*100,
         minbio_o3 = (omega3_mg/o3_mg)*100) %>% 
  select(contains("minbio")) %>% 
  mutate(minbio_sys_biom_all = max(minbio_pr, minbio_fe, minbio_zn, minbio_ca, minbio_vita, minbio_o3)) %>% # calculate minimum portion size for all 6 nutrients
  # minbio_sys_ccm_except_vita = max(minbio_pr, minbio_fe, minbio_zn, minbio_ca, minbio_o3), # calculate minimum portion size for all nutrients except Vitamin A
  # minbio_sys_ccm_except_vita_ca = max(minbio_pr, minbio_zn, minbio_fe, minbio_o3)) %>%  # calculate minimum portion size for all nutrients except Vitamin A and Iron
  select(contains("minbio_sys_biom"))   


#------------------------------------------------------------------------------# 
# COMBINE FILES
#------------------------------------------------------------------------------# 

# combining nd_score files
  nd_score <- nd_score_hh_catch %>% 
    rbind(nd_score_hh_cons) %>% 
    rbind(nd_score_hh_sold) %>% 
    #select(-hhid) %>%
    mutate(cfrid = 0) %>% 
    rbind(nd_score_cfr) 
  
#------------------------------------------------------------------------------# 
# PLOT
#------------------------------------------------------------------------------# 
  
#----- Plot differences in nutrient density score across portfolios -----------#

order <-c("cfr", "catch", "cons", "sold" )
nd_score %>%  
  select(-cfrid) %>% 
  filter(nd_score != 0) %>%  # removing cases where households sold no fish
  ggplot(aes(x = type, y = nd_score, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 1.3, alpha = 0.5, width = 0.2, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.7, name = "Portfolio type", labels = c("CFR", "Household Catch", "Household Consumption", "Household Sold")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        plot.caption = element_text(hjust = 0)) +
  ylab("Nutrient density score") +
  labs(title = "Nutrient density scores of household and CFR portfolios", 
  caption = "
       Sold plot excludes households that sold no fish. White diamonds depict means. 
       Means differences are significant between all groups except catch and consumed 
      (Paired t-tests with Bonferroni correction)")

# ggsave(path = "output/20221109/figures", "nd_score_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)
#ggsave(path = main_figures, "nd_score_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)

path <- paste("output/",output_date,"/figures/",sep="")
ggsave(path = path, "nd_score_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)


#--------- Box plot with CFR-level values mapped to colors -------------------*/

# make a household-level variable that represents CFR-level nutrient density to map to colors
cfr_colors <- nd_score %>% 
  select(cfrid, nd_score, type) %>% 
  filter(type == "cfr") %>% 
  select(-type) %>% 
  rename(cfr_colors = nd_score)

# merge cfr_colors into main data file
plotfile <- nd_score %>% 
  select(hhid, type, cfrid, nd_score) %>% 
  # bring in missing cfrids
  left_join(cfrid_to_hhid, by = "hhid") %>% 
  mutate(cfrid.x = replace_na(cfrid.x, 0)) %>% 
  mutate(cfrid.y = replace_na(cfrid.y, 0)) %>% 
  mutate(cfrid = cfrid.y + cfrid.x) %>% 
  select(hhid, type, nd_score, cfrid) %>% 
  left_join(cfr_colors, by = "cfrid")

plotfile %>% 
  filter(nd_score != 0) %>% # removing hh that sold no fish
  arrange(-nd_score) %>% 
  ggplot(aes(x = type, y = nd_score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = cfr_colors), alpha = 0.7, width = 0.2) +
  scale_color_viridis(option = "C", name = "Nutrient density \nscore") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "black", fill = "white") +
  #scale_fill_viridis(discrete = TRUE, alpha = 0.7, option = "C") +
  scale_x_discrete(limits = order, labels=c("CFR","Catch","Consumed", "Sold")) +
  theme_bw() +
  ylab("Nutrient density score") +
  labs(title = "Nutrient density score by portfolio type (CFR level)")

path <- paste("output/",output_date,"/figures/",sep="")
ggsave(path = path, "species_boxplot_cfr.png", width = 16, height =  12, units = "cm", dpi = 320)
  

#--------- Test mean differences in nutrient density score -------------------*/

# convert CFR biom data to household level
cfrdata <- cfrid_to_hhid %>% 
  left_join(nd_score_cfr, by = "cfrid") %>% 
  mutate(hhid = hhid.x) %>% 
  select(hhid, nd_score, type)

# combine data files
data <- nd_score %>% 
  select(nd_score, type, hhid) %>% 
  filter(type != "cfr")   %>% 
  rbind(cfrdata) %>% 
  as_tibble() 

## Export for use in stata
data %>% 
  pivot_wider(names_from = "type", values_from = "nd_score") %>% 
  write.csv(., file = "data/processed/nd_score_hh_level.csv")


# compare differences with n = 413, replacing nd_score = 0 for households that did not sell fish.
t413 <- data %>% 
  pairwise_t_test(nd_score ~ type, paired = TRUE, p.adjust.method = "bonferroni")

# compare differences with n = 266, dropping all households that did not sell fish
t266 <- data %>% 
  pivot_wider(names_from = "type", values_from = "nd_score") %>% 
  filter(sold > 0) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "nd_score") %>% 
  pairwise_t_test(nd_score ~ type, paired = TRUE, p.adjust.method = "bonferroni")

# export

csv_name <- paste("output/",output_date,"/tables/ttests/diet_quality_ttest.csv",sep="")
t413 %>% rbind(t266) %>% 
  write_csv(., file = csv_name)


#--------- Scatter plots of nutrient density scores --------------------------*/

# set up data frame. 
nd_catch <- nd_score_hh_catch %>% 
  mutate(nd_catch = nd_score) %>% 
  select(hhid, nd_catch)
nd_cons <- nd_score_hh_cons %>% 
  mutate(nd_cons = nd_score) %>% 
  select(hhid, nd_cons)
nd_sold <- nd_score_hh_sold %>% 
  mutate(nd_sold = nd_score) %>% 
  select(hhid, nd_sold)
nd_cfr <- nd_score_cfr %>% 
  mutate(nd_cfr = nd_score) %>% 
  select(cfrid, nd_cfr)

nd <- nd_catch %>% 
  left_join(nd_cons, by = "hhid") %>% 
  left_join(nd_sold, by = "hhid") %>% 
  left_join(cfrid_to_hhid, by = "hhid") %>% 
  left_join(nd_cfr, by = "cfrid")

# Plot

path <- paste("output/",output_date,"/figures/scatter_plots",sep="")

# system X catch
nd %>% 
  ggplot(aes(x = nd_cfr, y = nd_catch)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Nutrient density score: Biomonitoring X Catch") +
  xlab("Nutrient density score of biomonitoring (CFR level)") +
  ylab("Nutrient density score of species caught (HH level)")

ggsave(path = path, "nd_systemXcatch.png", width = 16, height =  12, units = "cm", dpi = 320)

# catch X consumption
nd %>% 
  ggplot(aes(x = nd_catch, y = nd_cons)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Nutrient density score: Catch X Consumption") +
  xlab("Nutrient density score of catch (HH level)") +
  ylab("Nutrient density score of consumed (HH level)")

ggsave(path = path, "nd_catchXcons.png", width = 16, height =  12, units = "cm", dpi = 320)


# catch X sold
nd %>% 
  filter(nd_sold != 0) %>% 
  ggplot(aes(x = nd_catch, y = nd_sold)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Nutrient density score: Catch X Sold (sold = 0 removed)") +
  xlab("Nutrient density score of catch (HH level)") +
  ylab("Nutrient density score of sold (HH level)")

ggsave(path = path, "nd_catchXsold.png", width = 16, height =  12, units = "cm", dpi = 320)


# system X consumption
nd %>% 
  ggplot(aes(x = nd_cfr, y = nd_cons)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Nutrient density score: Biomonitoring X Consumption") +
  xlab("Nutrient density score of biomonitoring (CFR level)") +
  ylab("Nutrient density score of consumed (HH level)")

ggsave(path = path, "nd_systemXcons.png", width = 16, height =  12, units = "cm", dpi = 320)

# system X sold

nd %>% 
  filter(nd_sold != 0) %>% 
  ggplot(aes(x = nd_cfr, y = nd_sold)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#008B8B") +
  theme_bw() +
  ggtitle("Nutrient density score: Biomonitoring X Sold (sold = 0 removed") +
  xlab("Nutrient density score of biomonitoring (CFR level)") +
  ylab("Nutrient density score of sold (HH level)")

ggsave(path = path, "nd_systemXsold.png", width = 16, height =  12, units = "cm", dpi = 320)

