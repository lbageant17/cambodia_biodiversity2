## Liz Bageant
## November 21, 2022


#------------------------------------------------------------------------------# 
#
#  CAMBODIA BIODIVERSITY MASTER FILE
#
#------------------------------------------------------------------------------# 


#------------------------------------------------------------------------------# 
#  Packages
#------------------------------------------------------------------------------# 

library(Hmisc) # install.packages("Hmisc") # this package interferes with summarize (with a z), use summarise with an s instead. Sometimes ungroup() is needed when you least expect it.
library(lubridate)
library(ggalluvial) # install.packages("ggalluvial")
library(alluvial) #install.packages("alluvial")
library(tidyverse)
library(viridis) #install.packages("viridis")
library (hrbrthemes) #install.packages("hrbrthemes")
library(vegan) #install.packages("vegan")
library(readxl)
library(multcompView) #install.packages("multcompView")
library(rstatix)


# Set up R to run stata files from R script below
library(RStata) #install.packages("RStata")
  options(RStata.StataPath = "/Applications/Stata/StataIC.app/Contents/MacOS/stataic") 
  options(RStata.StataVersion = 15)
  options(RStata.StataEcho = FALSE)
  
# Create date-specific file paths
  output_date <- "20221121"     # <---- THIS MUST MATCH THE DATE IN _0_initialize_macros.do !!

  

  

#------------------------------------------------------------------------------# 
#  Phase 1: Data prep in Stata
#------------------------------------------------------------------------------# 

  stata("syntax/_PHASE_1_MASTER.do")
 
#------------------------------------------------------------------------------# 
#  Phase 2: Data analysis and visualization in R
#------------------------------------------------------------------------------#  

  source("syntax/9_r_data_prep.R")
  source("syntax/10_diversity_index_calculation_20221116.R")
  source("syntax/11_species_count_analysis_20221121.R")
  source("syntax/12_diet_quality_calculation_20221121.R")
  source("syntax/13_commonness_analysis_20221111.R")
  source("syntax/14_body_size_calculation_20221121.R")
  
#------------------------------------------------------------------------------# 
#  Phase 3: Summary statistics and regression analysis in Stata
#------------------------------------------------------------------------------#    
  
  stata("syntax/_PHASE_3_MASTER.do")

#------------------------------------------------------------------------------# 
#  Phase 4: Plot regression analyses in R
#------------------------------------------------------------------------------#    

  source("syntax/21_plot_betas.R")
  source("syntax/22_variance_decomposition_plots.R")  
  source("syntax/23_commercial_species_calculations.R")
  source("syntax/24_quantity_explore.R")  

  
  
