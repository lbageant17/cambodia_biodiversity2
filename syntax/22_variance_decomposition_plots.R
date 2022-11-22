## Liz Bageant
## November 21, 2022

# Plot shapley-owen decompositions

#------------------------------------------------------------------------------# 
#
#  PLOT SHAPLEY OWEN DECOMPOSITIONS
#  In this file we look at plots of shapley-owen decompositions only for models
#  with system diversity/traits as the predictor. 
#
#------------------------------------------------------------------------------#

# Bring in shapley values from excel
path <- (paste("output/",output_date,"/tables/variance_decomposition/variance_decomposition.xlsx",sep=""))
shapley <- read_excel(path)


# set path for output of all plots
shapleypath <- paste("output/",output_date,"/figures/variance_decomposition/",sep="")


#------------------------------------------------------------------------------# 
#  Species count models
#------------------------------------------------------------------------------# 

shapley %>% 
  filter(Predictor == "cfr_species") %>% 
  mutate(effort = effort + effort2) %>% 
  select(-effort2, -Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc",  "livelihood", "amenities", "city_distance", "effort", "pred")) %>% 
  ggplot(aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Variables",
                     labels = c("Dependency share", "Household size", "Education", "Livelihood index", "Amenities index", "Market access", "Effort", "CFR species richness")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for species count models")
ggsave(path = shapleypath, "species_counts.png", width = 10, height = 7, dpi = 320)


#------------------------------------------------------------------------------# 
#  Shannon models
#------------------------------------------------------------------------------# 

shapley %>% 
  filter(Predictor == "h_cfr") %>% 
  mutate(effort = effort + effort2) %>% 
  select(-effort2, -Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc",  "livelihood", "amenities", "city_distance", "effort", "pred")) %>% 
  ggplot(aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Variables",
                     labels = c("Dependency share", "Household size", "Education", "Livelihood index", "Amenities index", "Market access", "Effort", "CFR Shannon index")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for Shannon index models")
ggsave(path = shapleypath, "shannon.png", width = 10, height = 7, dpi = 320)


#------------------------------------------------------------------------------# 
#  Simpson models
#------------------------------------------------------------------------------# 

s <- shapley %>% 
  filter(Predictor == "d1_cfr") %>% 
  mutate(effort = effort + effort2) %>% 
  select(-effort2, -Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc",  "livelihood", "amenities", "city_distance", "effort", "pred")) %>% 
  ggplot(aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Variables",
                     labels = c("Dependency share", "Household size", "Education", "Livelihood index", "Amenities index", "Market access", "Effort", "CFR Simpson index")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for Simpson index models")
ggsave(path = shapleypath, "simpson.png", width = 10, height = 7, dpi = 320)


#------------------------------------------------------------------------------# 
#  Commonness models
#------------------------------------------------------------------------------# 

s <- shapley %>% 
  filter(Predictor == "com_cfr") %>% 
  mutate(effort = effort + effort2) %>% 
  select(-effort2, -Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc",  "livelihood", "amenities", "city_distance", "effort", "pred")) %>% 
  ggplot(aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Variables",
                     labels = c("Dependency share", "Household size", "Education", "Livelihood index", "Amenities index", "Market access", "Effort", "CFR commonness")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for commonness index models")
ggsave(path = shapleypath, "commonness.png", width = 10, height = 7, dpi = 320)

#------------------------------------------------------------------------------# 
#  Body size models
#------------------------------------------------------------------------------# 

shapley %>% 
  filter(Predictor == "body_size_cfr") %>% 
  mutate(effort = effort + effort2) %>% 
  select(-effort2, -Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc",  "livelihood", "amenities", "city_distance", "effort", "pred")) %>% 
  ggplot(aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Variables",
                     labels = c("Dependency share", "Household size", "Education", "Livelihood index", "Amenities index", "Market access", "Effort", "CFR body size")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for body size models")
ggsave(path = shapleypath, "body_size.png", width = 10, height = 7, dpi = 320)


#------------------------------------------------------------------------------# 
#  Nutrient density models
#------------------------------------------------------------------------------# 

shapley %>% 
  filter(Predictor == "nd_score_cfr") %>% 
  mutate(effort = effort + effort2) %>% 
  select(-effort2, -Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc",  "livelihood", "amenities", "city_distance", "effort", "pred")) %>% 
  ggplot(aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Variables",
                     labels = c("Dependency share", "Household size", "Education", "Livelihood index", "Amenities index", "Market access", "Effort", "CFR nutrient density")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for nutrient density models")
ggsave(path = shapleypath, "nd_score.png", width = 10, height = 7, dpi = 320)




#------------------------------------------------------------------------------# 
#  Species count models--no catch
#------------------------------------------------------------------------------# 

shapley %>% 
  filter(Predictor == "cfr_species") %>%
  filter(Outcome != "catch_species") %>% 
  mutate(effort = effort + effort2) %>% 
  select(-effort2, -Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc",  "livelihood", "amenities", "city_distance", "effort", "pred")) %>% 
  ggplot(aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Variables",
                     labels = c("Dependency share", "Household size", "Education", "Livelihood index", "Amenities index", "Market access", "Effort", "CFR species richness")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for species count models")
ggsave(path = shapleypath, "species_counts_nocatch.png", width = 10, height = 7, dpi = 320)


#------------------------------------------------------------------------------# 
#  Shannon models--no catch
#------------------------------------------------------------------------------# 

shapley %>% 
  filter(Predictor == "h_cfr") %>% 
  filter(Outcome != "h_catch") %>% 
  mutate(effort = effort + effort2) %>% 
  select(-effort2, -Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc",  "livelihood", "amenities", "city_distance", "effort", "pred")) %>% 
  ggplot(aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Variables",
                     labels = c("Dependency share", "Household size", "Education", "Livelihood index", "Amenities index", "Market access", "Effort", "CFR Shannon index")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for Shannon index models")
ggsave(path = shapleypath, "shannon_nocatch.png", width = 10, height = 7, dpi = 320)


#------------------------------------------------------------------------------# 
#  Simpson models--no catch
#------------------------------------------------------------------------------# 

s <- shapley %>% 
  filter(Predictor == "d1_cfr") %>% 
  filter(Outcome != "d1_catch") %>% 
  mutate(effort = effort + effort2) %>% 
  select(-effort2, -Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc",  "livelihood", "amenities", "city_distance", "effort", "pred")) %>% 
  ggplot(aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Variables",
                     labels = c("Dependency share", "Household size", "Education", "Livelihood index", "Amenities index", "Market access", "Effort", "CFR Simpson index")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for Simpson index models")
ggsave(path = shapleypath, "simpson_nocatch.png", width = 10, height = 7, dpi = 320)


#------------------------------------------------------------------------------# 
#  Commonness models--no catch
#------------------------------------------------------------------------------# 

s <- shapley %>% 
  filter(Predictor == "com_cfr") %>% 
  filter(Outcome != "com_catch") %>% 
  mutate(effort = effort + effort2) %>% 
  select(-effort2, -Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc",  "livelihood", "amenities", "city_distance", "effort", "pred")) %>% 
  ggplot(aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Variables",
                     labels = c("Dependency share", "Household size", "Education", "Livelihood index", "Amenities index", "Market access", "Effort", "CFR commonness")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for commonness index models")
ggsave(path = shapleypath, "commonness_nocatch.png", width = 10, height = 7, dpi = 320)

#------------------------------------------------------------------------------# 
#  Body size models--no catch
#------------------------------------------------------------------------------# 

shapley %>% 
  filter(Predictor == "body_size_cfr") %>% 
  filter(Outcome != "body_size_catch") %>% 
  mutate(effort = effort + effort2) %>% 
  select(-effort2, -Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc",  "livelihood", "amenities", "city_distance", "effort", "pred")) %>% 
  ggplot(aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Variables",
                     labels = c("Dependency share", "Household size", "Education", "Livelihood index", "Amenities index", "Market access", "Effort", "CFR body size")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for body size models")
ggsave(path = shapleypath, "body_size_nocatch.png", width = 10, height = 7, dpi = 320)


#------------------------------------------------------------------------------# 
#  Nutrient density models--no catch
#------------------------------------------------------------------------------# 

shapley %>% 
  filter(Predictor == "nd_score_cfr") %>% 
  filter(Outcome != "nd_score_catch") %>% 
  mutate(effort = effort + effort2) %>% 
  select(-effort2, -Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc",  "livelihood", "amenities", "city_distance", "effort", "pred")) %>% 
  ggplot(aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Variables",
                     labels = c("Dependency share", "Household size", "Education", "Livelihood index", "Amenities index", "Market access", "Effort", "CFR nutrient density")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for nutrient density models")
ggsave(path = shapleypath, "nd_score_nocatch.png", width = 10, height = 7, dpi = 320)


