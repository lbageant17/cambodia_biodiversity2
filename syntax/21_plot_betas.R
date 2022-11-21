# Liz Bageant
# November 18, 2022

#------------------------------------------------------------------------------# 
#
#  PLOT REGRESSION MODEL COEFFICIENTS 
#
#------------------------------------------------------------------------------#

# set path for output of all beta coefficient plots
betapath <- paste("output/",output_date,"/figures/beta_compare/",sep="")

# Species count household models
path <- paste("output/",output_date,"/tables/effort/betas_species.xlsx",sep="")
species_betas_e <- read_excel(path)
path <- paste("output/",output_date,"/tables/no_effort/betas_species_noeffort.xlsx",sep="")
species_betas_ne <- read_excel(path) 

# Traits household models
path <- paste("output/",output_date,"/tables/effort/betas_traits.xlsx",sep="")
traits_betas_e <- read_excel(path)
path <- paste("output/",output_date,"/tables/no_effort/betas_traits_noeffort.xlsx",sep="")
traits_betas_ne <- read_excel(path) 

# Index household models 
path <- paste("output/",output_date,"/tables/effort/betas_index.xlsx",sep="")
index_betas_e <- read_excel(path)

#------------------------------------------------------------------------------# 
#  Species count models
#------------------------------------------------------------------------------# 

# System species count point estimates with effort controls
species_betas_e %>% 
    filter(Predictor == "cfr_species") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls", "Add Effort", "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Catch species", "Consumption species", "Sold species")) +
    xlim(-0.02, 1) +
    theme_bw() +
    ggtitle("System species count point estimates with effort controls and 95% CIs") 
  ggsave(path = betapath, "system_species.png", width = 10, height =  4, dpi = 320)

# System species count point estimates without effort controls
  species_betas_ne %>% 
    filter(Predictor == "cfr_species") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls",  "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Catch species", "Consumption species", "Sold species")) +
    xlim(-0.02, 1) +
    theme_bw() +
    ggtitle("System species count point estimates with no effort controls and 95% CIs") 
  ggsave(path = betapath, "system_species_NE.png", width = 10, height =  4, dpi = 320)


# Catch species count point estimates with effort controls
  species_betas_e %>% 
    filter(Predictor == "catch_species") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls", "Add Effort", "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Consumption species", "Sold species")) +
    xlim(-0.02, 1) +
    theme_bw() +
    ggtitle("Catch species count point estimates with effort controls and 95% CIs") +
    theme(plot.caption = element_text(hjust = 0)) +
    labs(caption = "
        Catch species count coefficients between sold species model with no controls 
        and sold species model with effort controls are statistically significantly different (p = 0.025)")
  
  ggsave(path = betapath, "catch_species.png", width = 10, height =  4, dpi = 320)

# Catch species count point estimates without effort controls
  species_betas_ne %>% 
    filter(Predictor == "catch_species") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls",  "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Consumption species", "Sold species")) +
    xlim(-0.02, 1) +
    theme_bw() +
    ggtitle("Catch species count point estimates with no effort controls and 95% CIs") 
  ggsave(path = betapath, "catch_species_NE.png", width = 10, height =  4, dpi = 320)


#------------------------------------------------------------------------------# 
#  Nutrient density models
#------------------------------------------------------------------------------# 
  
# System nutrient density models with effort controls
traits_betas_e %>% 
    filter(Predictor == "nd_score_cfr") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls", "Add Effort", "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Catch nutrient density", "Consumption nutrient density", "Sold nutrient density")) +
    xlim(-0.5, 1.25) +
    theme_bw() +
    ggtitle("System nutrient density point estimates with effort controls and 95% CIs") +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(caption = "
        System nutrient density coefficients between sold nutrient density model with no controls 
        and sold nutrient density model with effort controls are statistically significantly different (p = 0.092)")

  ggsave(path = betapath, "system_nd.png", width = 10, height =  4, dpi = 320)


# System nutrient density models without effort controls
  traits_betas_ne %>% 
    filter(Predictor == "nd_score_cfr") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls",  "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Catch nutrient density", "Consumption nutrient density", "Sold nutrient density")) +
    xlim(-0.5, 1.25) +
    theme_bw() +
    ggtitle("System nutrient density point estimates with no effort controls and 95% CIs") 
  ggsave(path = betapath, "system_nd_NE.png", width = 10, height =  4, dpi = 320)
  
# Catch nutrient density models with effort controls
  traits_betas_e %>% 
    filter(Predictor == "nd_score_catch") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls", "Add Effort", "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Consumption nutrient density", "Sold nutrient density")) +
    xlim(-0.5, 1.25) +
    theme_bw() +
    ggtitle("Catch nutrient density point estimates with effort controls and 95% CIs") +
    theme(plot.caption = element_text(hjust = 0)) +
    labs(caption = "
        Catch nutrient density coefficients between sold nutrient density model with no controls 
        and sold nutrient density model with effort controls are statistically significantly different (p = 0.058)")
  
  ggsave(path = betapath, "catch_nd.png", width = 10, height =  4, dpi = 320)
  
# Catch nutrient density models without effort controls
  traits_betas_ne %>% 
    filter(Predictor == "nd_score_catch") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls",  "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Consumption nutrient density", "Sold nutrient density")) +
    xlim(-0.5, 1.25) +
    theme_bw() +
    ggtitle("Catch nutrient density point estimates with no effort controls and 95% CIs") 
  ggsave(path = betapath, "catch_nd_NE.png", width = 10, height =  4, dpi = 320)

  
#------------------------------------------------------------------------------# 
#  Body size models
#------------------------------------------------------------------------------# 

# System body size models with effort controls
  traits_betas_e %>% 
    filter(Predictor == "body_size_cfr") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls", "Add Effort", "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Catch body size", "Consumption body size", "Sold body size")) +
    #xlim(-0.3, 1) +
    theme_bw() +
    ggtitle("System body size point estimates with effort controls and 95% CIs") 
  ggsave(path = betapath, "system_body_size.png", width = 10, height =  4, dpi = 320)
  
# System body size models without effort controls
  traits_betas_ne %>% 
    filter(Predictor == "body_size_cfr") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls",  "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Catch body size", "Consumption body size", "Sold body size")) +
    #xlim(-0.3, 1) +
    theme_bw() +
    ggtitle("System body size point estimates with no effort controls and 95% CIs") 
  ggsave(path = betapath, "system_body_size_NE.png", width = 10, height =  4, dpi = 320)
  
# Catch body size models with effort controls
x <-  traits_betas_e %>% 
    filter(Predictor == "body_size_catch") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls", "Add Effort", "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Consumption body size", "Sold body size")) +
    #xlim(-0.5, 1.25) +
    theme_bw() +
    ggtitle("Catch body size point estimates with effort controls and 95% CIs") 
  ggsave(path = betapath, "catch_body_size.png", width = 10, height =  4, dpi = 320)
  
# Catch body size models without effort controls
  traits_betas_ne %>% 
    filter(Predictor == "body_size_catch") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls",  "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Consumption body size", "Sold body size")) +
    #xlim(0, 6.2) +
    theme_bw() +
    ggtitle("Catch body size point estimates with no effort controls and 95% CIs") 
  ggsave(path = betapath, "catch_body_size_NE.png", width = 10, height =  4, dpi = 320)
  

#------------------------------------------------------------------------------# 
#  Commonness models
#------------------------------------------------------------------------------# 
  
# System commonness models with effort controls
  traits_betas_e %>% 
    filter(Predictor == "com_cfr") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls", "Add Effort", "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Catch commonness", "Consumption commonness", "Sold commonness")) +
    #xlim(-.5, 0.65) +
    theme_bw() +
    ggtitle("System commonness point estimates with effort controls and 95% CIs") 
  ggsave(path = betapath, "system_commonness.png", width = 10, height =  4, dpi = 320)
  
# System commonness models without effort controls
  traits_betas_ne %>% 
    filter(Predictor == "com_cfr") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls",  "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Catch commonness", "Consumption commonness", "Sold commonness")) +
    #xlim(-.01, 0.65) +
    theme_bw() +
    ggtitle("System commonness point estimates with no effort controls and 95% CIs") 
  ggsave(path = betapath, "system_commonness_NE.png", width = 10, height =  4, dpi = 320)
  
# Catch commonness models with effort controls
  traits_betas_e %>% 
    filter(Predictor == "com_catch") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40))+
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls", "Add Effort", "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Consumption commonness", "Sold commonness")) +
    #xlim(0, 7) +
    theme_bw() +
    ggtitle("Catch commonness point estimates with effort controls and 95% CIs") 
  ggsave(path = betapath, "catch_commonness.png", width = 10, height =  4, dpi = 320)

# Catch commonness models without effort controls
  traits_betas_ne %>% 
    filter(Predictor == "com_catch") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls",  "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Consumption commonness", "Sold commonness")) +
    #xlim(0, 7) +
    theme_bw() +
    ggtitle("Catch commonness point estimates with no effort controls and 95% CIs") 
  ggsave(path = betapath, "catch_commonness_NE.png", width = 10, height =  4, dpi = 320)
  


#------------------------------------------------------------------------------# 
#  Shannon models
#------------------------------------------------------------------------------# 
  index_betas_e %>% 
    filter(Predictor == "h_cfr") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls", "Add Effort", "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Catch Shannon", "Consumption Shannon", "Sold Shannon")) +
    #xlim(-.5, 0.65) +
    theme_bw() +
    ggtitle("System Shannon index point estimates with effort controls and 95% CIs") 
  ggsave(path = betapath, "system_shannon.png", width = 10, height =  4, dpi = 320)
  
  
# Catch Shannon index
  index_betas_e %>% 
    filter(Predictor == "h_catch") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls", "Add Effort", "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Consumption Shannon", "Sold Shannon")) +
    #xlim(-.5, 0.65) +
    theme_bw() +
    ggtitle("Catch Shannon index point estimates with effort controls and 95% CIs") 
  ggsave(path = betapath, "catch_shannon.png", width = 10, height =  4, dpi = 320)
  

#------------------------------------------------------------------------------# 
#  Simpson models
#------------------------------------------------------------------------------# 

  # System Simpson index
  index_betas_e %>% 
    filter(Predictor == "d1_cfr") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls", "Add Effort", "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Catch Simpson", "Consumption Simpson", "Sold Simpson")) +
    #xlim(-.5, 0.65) +
    theme_bw() +
    ggtitle("System Simpson index point estimates with effort controls and 95% CIs") 
  ggsave(path = betapath, "system_simpson.png", width = 10, height =  4, dpi = 320)
  
  
  # Catch Simpson index
  index_betas_e %>% 
    filter(Predictor == "d1_catch") %>% 
    ggplot() +
    geom_pointrange(aes(x = coeff, 
                        y = Outcome,
                        xmin = min,
                        xmax = max,
                        group = Model,
                        color = Model),
                    position = position_dodge(width = 0.40)) +
    scale_color_viridis(discrete = TRUE, 
                        name = "Model", 
                        labels = c("No controls", "Add Effort", "Add HH Characteristics", "Add Market Access")) +
    scale_y_discrete(name = "", labels = c("Consumption Simpson", "Sold Simpson")) +
    #xlim(-.5, 0.65) +
    theme_bw() +
    ggtitle("Catch Simpson index point estimates with effort controls and 95% CIs") 
  ggsave(path = betapath, "catch_simpson.png", width = 10, height =  4, dpi = 320)



#------------------------------------------------------------------------------# 
#  Inverse Simpson models
  
# Inverse Simpson regressions are complicated by infinite values of the index, 
# so they have not been run as of 11/18/2022 
#------------------------------------------------------------------------------# 



