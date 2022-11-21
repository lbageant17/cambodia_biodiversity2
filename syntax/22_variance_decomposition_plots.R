## Liz Bageant
## November 21, 2022

# Plot shapley-owen decompositions



#------------------------------------------------------------------------------# 
#
#  PLOT SHAPLEY OWEN DECOMPOSITIONS
#
#------------------------------------------------------------------------------#

# set path for output of all plots
shapleypath <- paste("output/",output_date,"/figures/variance_decomposition/",sep="")



#------------------------------------------------------------------------------# 
#  Species count models
#------------------------------------------------------------------------------# 


# Species count household models
path <- paste("output/",output_date,"/tables/variance_decomposition/variance_decomposition.xlsx",sep="")
shapley <- read_excel(path)

s <- shapley %>% 
  filter(Outcome == c("catch_species", "consumption_species", "sold_species")) %>% 
  select(-Predictor) %>% 
  pivot_longer(!Outcome, names_to = "variable", values_to = "r2_explained") %>% 
  filter(variable != "total_r2") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = fct_relevel(variable, "depshare", "hhsize", "maxeduc", "city_distance", "amenities", "livelihood", "effort2", "effort", "pred"))


ggplot(s, aes(fill = variable, y = r2_explained, x = Outcome)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = T, 
                     name = "Varibles",
                     labels = c("Dependency share", "Household size", "Education", "Market access", "Amenities index", "Livelihood index", "Effort-squared", "Effort", "Diversity predictor")) +
  theme_bw() +
  ylab("R-squared") +
  xlab("") +
  ggtitle("Decomposed R-squared for species count models")

ggsave(path = shapleypath, "species_counts.png", dpi = 320)


