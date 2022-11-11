# plotting beta coefficients of different models

# Species count household models
hh_betas_e <- read_excel("output/20221109/tables/betas_hh.xlsx")
hh_betas_ne <- read_excel("output/20221109/tables/betas_hh_noeffort.xlsx") 

# Traits household models
traits_betas_e <- read_excel("output/20221109/tables/betas_traits.xlsx")
traits_betas_ne <- read_excel("output/20221109/tables/betas_traits_noeffort.xlsx") 

#------------------------------------------------------------------------------# 
#  Species count models
#------------------------------------------------------------------------------# 

# System species count point estimates with effort controls
hh_betas_e %>% 
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
  ggsave(path = "output/20221109/figures/beta_compare", "system_species.png", width = 10, height =  4, dpi = 320)

# System species count point estimates without effort controls
  hh_betas_ne %>% 
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
  ggsave(path = "output/20221109/figures/beta_compare", "system_species_NE.png", width = 10, height =  4, dpi = 320)


# Catch species count point estimates with effort controls
  hh_betas_e %>% 
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
  
  ggsave(path = "output/20221109/figures/beta_compare", "catch_species.png", width = 10, height =  4, dpi = 320)

# Catch species count point estimates without effort controls
  hh_betas_ne %>% 
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
  ggsave(path = "output/20221109/figures/beta_compare", "catch_species_NE.png", width = 10, height =  4, dpi = 320)


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

  ggsave(path = "output/20221109/figures/beta_compare", "system_nd.png", width = 10, height =  4, dpi = 320)


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
  ggsave(path = "output/20221109/figures/beta_compare", "system_nd_NE.png", width = 10, height =  4, dpi = 320)
  
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
  
  ggsave(path = "output/20221109/figures/beta_compare", "catch_nd.png", width = 10, height =  4, dpi = 320)
  
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
  ggsave(path = "output/20221109/figures/beta_compare", "catch_nd_NE.png", width = 10, height =  4, dpi = 320)

  
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
    xlim(-0.3, 1) +
    theme_bw() +
    ggtitle("System body size point estimates with effort controls and 95% CIs") 
  ggsave(path = "output/20221109/figures/beta_compare", "system_body_size.png", width = 10, height =  4, dpi = 320)
  
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
    xlim(-0.3, 1) +
    theme_bw() +
    ggtitle("System body size point estimates with no effort controls and 95% CIs") 
  ggsave(path = "output/20221109/figures/beta_compare", "system_body_size_NE.png", width = 10, height =  4, dpi = 320)
  
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
  ggsave(path = "output/20221109/figures/beta_compare", "catch_body_size.png", width = 10, height =  4, dpi = 320)
  
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
    xlim(0, 6.2) +
    theme_bw() +
    ggtitle("Catch body size point estimates with no effort controls and 95% CIs") 
  ggsave(path = "output/20221109/figures/beta_compare", "catch_body_size_NE.png", width = 10, height =  4, dpi = 320)
  

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
    xlim(-.01, 0.65) +
    theme_bw() +
    ggtitle("System commonness point estimates with effort controls and 95% CIs") 
  ggsave(path = "output/20221109/figures/beta_compare", "system_commonness.png", width = 10, height =  4, dpi = 320)
  
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
    xlim(-.01, 0.65) +
    theme_bw() +
    ggtitle("System commonness point estimates with no effort controls and 95% CIs") 
  ggsave(path = "output/20221109/figures/beta_compare", "system_commonness_NE.png", width = 10, height =  4, dpi = 320)
  
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
    xlim(0, 7) +
    theme_bw() +
    ggtitle("Catch commonness point estimates with effort controls and 95% CIs") 
  ggsave(path = "output/20221109/figures/beta_compare", "catch_commonness.png", width = 10, height =  4, dpi = 320)

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
    xlim(0, 7) +
    theme_bw() +
    ggtitle("Catch commonness point estimates with no effort controls and 95% CIs") 
  ggsave(path = "output/20221109/figures/beta_compare", "catch_commonness_NE.png", width = 10, height =  4, dpi = 320)
  







