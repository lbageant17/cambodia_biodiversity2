
# 11/18/2022
#number of species caught, consumed and sold and processed


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



# 11/16/2022. How does our "commonness" index relate to the Simpson index calculated by R? Badly. 

simpson <- diversity_indices_hh_level %>% 
  select(hhid, contains("d1")) %>%
  rename(cons = d1_cons, 
         sold = d1_sold, 
         catch = d1_catch) %>% 
  pivot_longer(!hhid, names_to = "type", values_to = "simpson") 


data <- commonness %>% 
  left_join(simpson, by = c("hhid", "type")) %>% 
  drop_na(simpson)

data %>% filter(type == "cons") %>% 
ggplot((aes(x = simpson, y = commonness))) +
  geom_point() +
  scale_y_continuous(trans = "log10")



## misc rank abundance things
library(vegan) # install.packages("vegan")
library(BiodiversityR) #install.packages("BiodiversityR")
data(dune.env)
data(dune)
RankAbun.1 <- rankabundance(dune)
RankAbun.1
rankabunplot(RankAbun.1, scale='abundance', addit=FALSE, specnames=c(1,2,3))
rankabunplot(RankAbun.1, scale='logabun', addit=FALSE, specnames=c(1:30), 
             srt=45, ylim=c(1,100))
rankabuncomp(dune, y=dune.env, factor='Management', 
             scale='proportion', legend=FALSE)
## CLICK IN THE GRAPH TO INDICATE WHERE THE LEGEND NEEDS TO BE PLACED
## IF YOU OPT FOR LEGEND=TRUE.

## Not run: 
# ggplot2 plotting method

# Only label the two most abundant species
RA.data <- rankabuncomp(dune, y=dune.env, factor='Management', 
                        return.data=TRUE, specnames=c(1:2), legend=FALSE)

library(ggplot2)
library(ggrepel)

# possibly need for extrafont::loadfonts(device="win") to have Arial
# as alternative, use library(ggThemeAssist)
BioR.theme <- theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line("gray25"),
  text = element_text(size = 12, family="Arial"),
  axis.text = element_text(size = 10, colour = "gray25"),
  axis.title = element_text(size = 14, colour = "gray25"),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.key = element_blank())

plotgg1 <- ggplot(data=RA.data, aes(x = rank, y = abundance)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=1) +
  geom_point(aes(colour=Grouping, shape=Grouping), size=5, alpha=0.7) +
  geom_text_repel(data=subset(RA.data, labelit == TRUE), 
                  aes(colour=Grouping, label=species), 
                  angle=45, nudge_x=1, nudge_y=1, show.legend=FALSE) +
  BioR.theme +
  scale_color_brewer(palette = "Set1") +
  labs(x = "rank", y = "abundance", colour = "Management", shape = "Management")

plotgg1

# use different facets
# now label first 10 species
RA.data <- rankabuncomp(dune, y=dune.env, factor='Management', 
                        return.data=TRUE, specnames=c(1:10), legend=FALSE)

plotgg2 <- ggplot(data=RA.data, aes(x = rank, y = abundance)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=1) +
  geom_point(aes(colour=Grouping), size=5, alpha=0.7) +
  geom_text_repel(data=subset(RA.data, labelit == TRUE), 
                  aes(label=species), 
                  angle=45, nudge_x=1, nudge_y=1, show.legend=FALSE) +
  BioR.theme +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ Grouping) +
  labs(x = "rank", y = "abundance", colour = "Management")

plotgg2
