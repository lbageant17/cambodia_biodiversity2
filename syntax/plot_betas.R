# plotting beta coefficients of different models


betas <- read_excel("data/for_plotting/betas.xlsx")



betas %>%
  filter(is.na(note),
         Predictor == "biom_species",
         coeff_var == "biom_species") %>% 
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
  ggtitle("System diversity point estimates") 
  ggsave(path = "output/", "coeff_panel1.png", width = 10, height =  4, dpi = 320)


betas %>%
  filter(is.na(note),
         Predictor == "catch_species",
         coeff_var == "catch_species") %>% 
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
  ggtitle("Catch diversity point estimates") +
  xlab("Coefficient estimate w/ 95% confidence interval")

betas %>%
  filter(is.na(note),
         Predictor == "biom_species",
         coeff_var == "effort") %>% 
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
                      labels = c("Add Effort", "Add HH Characteristics", "Add Market Access")) +
  scale_y_discrete(name = "", labels = c("Sold species", "Consumption species", "Catch species")) +
  xlim(-0.01, 0.25) +
  theme_bw() +
  ggtitle("Effort point estimates for system diversity models") +
  xlab("Coefficient estimate w/ 95% confidence interval")

ggsave(path = "output/", "coeff_panel3.png", width = 10, height =  2.6, dpi = 320)


betas %>%
  filter(is.na(note),
         Predictor == "catch_species",
         coeff_var == "effort") %>% 
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
                      labels = c("Add Effort", "Add HH Characteristics", "Add Market Access")) +
  scale_y_discrete(name = "", labels = c("Sold species", "Consumption species")) +
  xlim(-0.01, 0.25) +
  theme_bw() +
  ggtitle("Effort point estimates for catch diversity models") +
  xlab("Coefficient estimate w/ 95% confidence interval")

ggsave(path = "output/", "coeff_panel4.png", width = 10, height =  2.6, dpi = 320)


