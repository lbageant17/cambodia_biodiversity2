## Liz Bageant
## October 26, 2022


## Incorporating household characteristics into main plots


#------------------------------------------------------------------------------# 
# LINEAR REGRESSION WITH HOUSEHOLD CHARACTERISTICS
#------------------------------------------------------------------------------# 
# regression resources: https://andrewproctor.github.io/rcourse/module5.html

# output info: https://cran.r-project.org/web/packages/jtools/vignettes/summ.html#Table_output_for_Word_and_RMarkdown_documents
library(effects) #install.packages("effects")
library(openxlsx) #install.packages("openxlsx")
library(flextable) #install.packages("flextable")
library(officer) #install.packages("officer")
library(jtools) #install.packages("jtools")
library(huxtable) #install.packages("huxtable")
library(geepack) #install.packages("geepack")



#------------------------------------------------------------------------------# 
# EXAMINE HOUSEHOLD CHARACTERISTICS
#------------------------------------------------------------------------------# 

hh <- l %>% 
  left_join(cfrid_to_hhid, by = "hhid") %>% 
  left_join(cfrtype, by = "cfrid") %>% 
  drop_na(city_distance) 
  
hh %>% summary()

hh %>% 
  ggplot(aes(x = city_distance)) +
  geom_histogram(binwidth = 1)





# Plotting interaction term according to: https://ademos.people.uic.edu/Chapter13.html
# run the interaction model
x1 <- lm(formula = catch_species ~ biom_species*city_distance + hhsize + maxeduc + index1 + index2, data = using)
summ(x1, robust = "HC1")
# using info from itneraction model, use effect package to generate values at specified levels (using quartile values)
inter <- effect('biom_species*city_distance', x1,
                xlevels = list(city_distance = c(23.2, 37.3, 48.5),
                               biom_species = c(40, 53, 62)),
                se = TRUE, confidence.level = 0.95, typical = mean)

inter <- as.data.frame(inter)

# create a factor of city_distance and biom_species
inter$city_distance <- factor(inter$city_distance,
                              levels = c(23.2, 37.3, 48.5),
                              labels = c("25 pctile", "median", "75 pctile"))
inter$biom_species <- factor(inter$biom_species,
                             levels = c(40, 53, 62),
                             labels = c("25 pctile", "median", "75 pctile"))

inter %>% ggplot(aes(x = city_distance, y = fit, group = biom_species)) +
  geom_line(size = 2, aes(color = biom_species)) 
  


### Biomonitoring X Catch ####
using <- cfr_biodiv %>% 
  left_join(cfrtype, by = "cfrid") %>% 
  left_join(l, by = "hhid")

x <- lm(formula = catch_species ~ biom_species , data = using)
x1 <- lm(formula = catch_species ~ biom_species + hhsize + maxeduc + index1 + index2, data = using)
x2 <- lm(formula = catch_species ~ biom_species + city_distance + hhsize + maxeduc + index1 + index2, data = using)
x3 <- lm(formula = catch_species ~ biom_species*city_distance + hhsize + maxeduc + index1 + index2, data = using)
summ(x1, robust = "HC1")
summ(x2, robust = "HC1")
0.04^2export_summs(x, x1, x2, x3, robust = "HC1", to.file = "xlsx", file.name = "TEST.xlsx")

plot(x1)
plot(x)

