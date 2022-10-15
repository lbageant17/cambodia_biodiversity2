## Liz Bageant
## September 27, 2022

#------------------------------------------------------------------------------# 
#
#  CONSTRUCTING DIET QUALITY METRICS USING RDAs
#
#------------------------------------------------------------------------------#


# Set RDA threshold

rda_threshold <- 0.5 # 100%


#------------------------------------------------------------------------------# 
#  Set up data files for diet quality calculations                          #### 
#------------------------------------------------------------------------------# 

  # RDA thresholds by nutrient
  nutrient <- c("protein_g", "iron_mg", "zinc_mg", "calcium_mg", "vitamina_mcg", "omega3_mg")
  rda_under5 <- c(13, 7.55, 4.1, 450, 400, 285) %>% 
    as.data.frame() %>% 
    mutate(threshold = .*rda_threshold) %>% 
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
  
  
  #### ----- Calculate proportional abundance by species ------ ####   
  
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

  
  #### ----- Calculate how many RDAs met by 100g of a given fish portfolio using RDA threshold set above------ #### 

  rda_hh_catch <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% # to look at meeting 50% RDA, multiple thresholds by 0.5
    # compare nutrient concentrations to RDA thresholds
    mutate(pr_rda = if_else(pr_g > protein_g, 1, 0),
           fe_rda = if_else(fe_mg > zinc_mg, 1, 0),
           zn_rda = if_else(zn_mg > zinc_mg, 1, 0),
           ca_rda = if_else(ca_mg > calcium_mg, 1, 0),
           vita_rda = if_else(vita_mcg > vitamina_mcg, 1, 0),
           o3_rda = if_else(o3_mg > omega3_mg, 1, 0),
           # total number of RDAs 100% met by 100g of portfolio
           rda_100pct_hh_catch = pr_rda + fe_rda + zn_rda + ca_rda + vita_rda + o3_rda) %>% 
    select(hhid, rda_100pct_hh_catch)
 
     
  #### ----- Calculate the minimum portion size of a household's portfolio that meets all RDAs ------ ####   

  minbio_hh_catch <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% # to look at meeting 50% RDA, multiply thresholds by 0.5 
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
  
  
  #### ----- Calculate how many RDAs are 100% met by 100g of a given CONSUMPTION portfolio ------ #### 
  
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
           # total number of RDAs 100% met by 100g of CONSUMPTION portfolio
           rda_100pct_hh_cons = pr_rda + fe_rda + zn_rda + ca_rda + vita_rda + o3_rda) %>% 
    select(hhid, rda_100pct_hh_cons)
  

  #### ----- Calculate the minimum portion size of a household's CONSUMPTION portfolio that meets all RDAs ------ ####   
  
  minbio_hh_cons <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% # to look at meeting 50% RDA, multiply thresholds by 0.5 
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
  
  #### ----- Calculate how many RDAs are 100% met by 100g of a given SOLD portfolio ------ #### 
  
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
           # total number of RDAs 100% met by 100g of SOLD portfolio
           rda_100pct_hh_sold = pr_rda + fe_rda + zn_rda + ca_rda + vita_rda + o3_rda) %>% 
    select(hhid, rda_100pct_hh_sold)
  
  #### ----- Calculate the minimum portion size of a household's SOLD portfolio that meets all RDAs ------ ####   
  
  minbio_hh_sold <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% # to look at meeting 50% RDA, multiply thresholds by 0.5 
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
  
  # household CFR-level proportional abundance by species
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
  
  
  #### ----- Calculate how many RDAs are 100% met by 100g of each CFR's portfolio ------ #### 
  
  rda_cfr <- portfolio_nutrient_concentration %>% 
    # bring in RDA thresholds
    cbind(rda) %>% # to look at meeting 50% RDA, multiple thresholds by 0.5
    # compare nutrient concentrations to RDA thresholds
    mutate(pr_rda = if_else(pr_g > protein_g, 1, 0),
           fe_rda = if_else(fe_mg > zinc_mg, 1, 0),
           zn_rda = if_else(zn_mg > zinc_mg, 1, 0),
           ca_rda = if_else(ca_mg > calcium_mg, 1, 0),
           vita_rda = if_else(vita_mcg > vitamina_mcg, 1, 0),
           o3_rda = if_else(o3_mg > omega3_mg, 1, 0),
           # total number of RDAs 100% met by 100g of each CFR's portfolio
           rda_100pct_cfr = pr_rda + fe_rda + zn_rda + ca_rda + vita_rda + o3_rda) %>% 
    select(cfrid, rda_100pct_cfr)
  
  
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
# SYSTEM LEVEL USING CCM DATA
#------------------------------------------------------------------------------#   
  
  #### ----- Calculate proportional abundance by species from CCM data ------ ####   

    # household system proportional abundance by species
    pa_sys_ccm <- c %>% 
      select(scode_ccm, catch_iweight) %>% 
      # get total system amount by species
      group_by(scode_ccm) %>% 
      summarise(sys_kg_species = sum(catch_iweight)) %>%
      mutate(total = as.numeric(total_sys_catch[1,1]), 
             pa = sys_kg_species/total) %>% 
      # Confirm that proportional abundances total to 1 
      # summarise(should_equal_1 = sum(pa)) 
      # table(pa_sys_ccm$should_equal_1)
      select(scode_ccm, pa)

  #### ----- Calculate nutrient concentration per 100g of the system CCM portfolio ------ ####    
  
    portfolio_nutrient_concentration <- pa_sys_ccm %>% 
      left_join(nutr_content_by_species, by = "scode_ccm") %>%
      # drop cases where content value is missing for any nutrient. These are species that are in the CCM data but for which we have no trait info.
      drop_na(contains("_content")) %>% # this leaves 112 species with nutrient content info
      # calculate the nutrient content relative to proportional abundance of the system's CCM portfolio
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
   
   
  #### ----- Calculate how many RDAs are 100% met by 100g of the system CCM portfolio ------ #### 
  
    rda_sys_ccm <- portfolio_nutrient_concentration %>% 
      # bring in RDA thresholds
      cbind(rda) %>% # to look at meeting 50% RDA, multiple thresholds by 0.5
      # compare nutrient concentrations to RDA thresholds
      mutate(pr_rda = if_else(pr_g > protein_g, 1, 0),
             fe_rda = if_else(fe_mg > zinc_mg, 1, 0),
             zn_rda = if_else(zn_mg > zinc_mg, 1, 0),
             ca_rda = if_else(ca_mg > calcium_mg, 1, 0),
             vita_rda = if_else(vita_mcg > vitamina_mcg, 1, 0),
             o3_rda = if_else(o3_mg > omega3_mg, 1, 0),
             # total number of RDAs 100% met by 100g of system CCM portfolio
             rda_100pct_sys_ccm = pr_rda + fe_rda + zn_rda + ca_rda + vita_rda + o3_rda) %>% 
      select(rda_100pct_sys_ccm)


  #### ----- Calculate the minimum portion size of the system CCM portfolio that meets all RDAs ------ ####   
    
    minbio_sys_ccm <- portfolio_nutrient_concentration %>% 
      # bring in RDA thresholds
      cbind(rda) %>% # to look at meeting 50% RDA, multiply thresholds by 0.5 
      # calculate minbio by nutrient
      mutate(minbio_pr = (protein_g/pr_g)*100,
             minbio_fe = (iron_mg/fe_mg)*100,
             minbio_zn = (zinc_mg/zn_mg)*100,
             minbio_ca = (calcium_mg/ca_mg)*100,
             minbio_vita = (vitamina_mcg/vita_mcg)*100,
             minbio_o3 = (omega3_mg/o3_mg)*100) %>% 
      select(contains("minbio")) %>% 
      mutate(minbio_sys_ccm_all = max(minbio_pr, minbio_fe, minbio_zn, minbio_ca, minbio_vita, minbio_o3)) %>% # calculate minimum portion size for all 6 nutrients
             # minbio_sys_ccm_except_vita = max(minbio_pr, minbio_fe, minbio_zn, minbio_ca, minbio_o3), # calculate minimum portion size for all nutrients except Vitamin A
             # minbio_sys_ccm_except_vita_ca = max(minbio_pr, minbio_zn, minbio_fe, minbio_o3)) %>%  # calculate minimum portion size for all nutrients except Vitamin A and Iron
      select(contains("minbio_sys_ccm"))     
  
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


  #### ----- Calculate how many RDAs are 100% met by 100g of the system BIOM portfolio ------ #### 

    rda_sys_biom <- portfolio_nutrient_concentration %>% 
      # bring in RDA thresholds
      cbind(rda) %>% # to look at meeting 50% RDA, multiple thresholds by 0.5
      # compare nutrient concentrations to RDA thresholds
      mutate(pr_rda = if_else(pr_g > protein_g, 1, 0),
             fe_rda = if_else(fe_mg > zinc_mg, 1, 0),
             zn_rda = if_else(zn_mg > zinc_mg, 1, 0),
             ca_rda = if_else(ca_mg > calcium_mg, 1, 0),
             vita_rda = if_else(vita_mcg > vitamina_mcg, 1, 0),
             o3_rda = if_else(o3_mg > omega3_mg, 1, 0),
             # total number of RDAs 100% met by 100g of system CCM portfolio
             rda_100pct_sys_biom = pr_rda + fe_rda + zn_rda + ca_rda + vita_rda + o3_rda) %>% 
      select(rda_100pct_sys_biom)


#### ----- Calculate the minimum portion size of the system's BIOM portfolio that meets all RDAs ------ ####   

minbio_sys_biom <- portfolio_nutrient_concentration %>% 
  # bring in RDA thresholds
  cbind(rda) %>% # to look at meeting 50% RDA, multiply thresholds by 0.5 
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

# combining household level files
  hh_dq <- rda_hh_catch %>% 
    left_join(minbio_hh_catch, by = "hhid") %>% 
    left_join(rda_hh_cons, by = "hhid") %>% 
    left_join(minbio_hh_cons, by = "hhid") %>% 
    left_join(rda_hh_sold, by = "hhid") %>% 
    left_join(minbio_hh_sold, by = "hhid")
  
# combine system level files
  sys_dq <- minbio_sys_biom %>% 
    cbind(minbio_sys_ccm)
  
  # exploratory plot  
  
  # set up CFR level file for plotting
  cfrplotfile <- minbio_cfr %>% 
    ungroup() %>% 
    mutate(type = "cfr", 
           amount = minbio_cfr) %>% 
    select(type, amount)
  
  # plot
  #order <- c("cfr", "minbio_hh_catch_all", "minbio_hh_cons_all", "minbio_hh_sold_all")
  sys <- minbio_sys_biom$minbio_sys_biom_all[1]
    
  hh_dq %>% select(hhid, contains("all")) %>% 
    pivot_longer(!hhid, names_to = "type", values_to = "amount") %>% 
    mutate(type = as.factor(type)) %>% 
    select(type, amount) %>% 
    rbind(cfrplotfile) %>% 
    arrange(type) %>% 
    ggplot(aes(x = type, y = amount, fill = type)) +
    geom_hline(yintercept = sys, color = "#00CCCC", size = 1) + # horizontal line corresponding to value in minbio_sys_biom
    geom_boxplot(outlier.shape = NA) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.8, name = "Portfolio type", labels = c("Household Catch", "Household Consumption", "Household Sold", "CFR")) +
    #scale_color_viridis(discrete = TRUE) +
    geom_jitter(size = 0.7, alpha = 0.4, width = 0.2, color = "black") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.caption = element_text(hjust = 0)) +
    ylim(0, 2000) +
    #scale_fill_viridis(name = "Household level \nportfolio type", labels = c("Catch", "Consumption", "Sold")) +
    # ggtitle("Minimum biomass of household portfolio required to meet all RDAs \nrelative to minimum biomass of system level portfolio") +
    ylab("Biomass (g)") +
    labs(title = "Minimum biomass of household and CFR portfolios required to meet \nall RDAs relative to minimum biomass of system level portfolio",
       caption = "RDA threshold = 100%. \nHorizontal line indicates minimum biomass of the system level portfolio that yields 100% of all RDAs, \ncalculated using biomonitoring data (264.48g)")
  
  ggsave(path = "output/", "minbio_boxplot.png", width = 16, height =  12, units = "cm", dpi = 320)
  
