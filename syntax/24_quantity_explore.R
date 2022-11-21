# Liz Bageant
# November 16, 2022


## Quantity explore

q <- c %>% 
  select(hhid, cfrid,  scode_ccm, atefresh_iweight, soldfresh_iweight, process_iweight, lost_iweight, other_iweight, catch_iweight) %>% 
  group_by(hhid) %>% 
  summarise(eat = sum(atefresh_iweight),
            sell = sum(soldfresh_iweight),
            process = sum(process_iweight),
            lost = sum(lost_iweight),
            other = sum(other_iweight),
            catch = sum(catch_iweight)) %>% 
  mutate(eat_pct = eat/catch,
         sell_pct = sell/catch,
         process_pct = process/catch,
         lost_pct = lost/catch, 
         other_pct = other/catch)

# What is the mean quantity of fish eaten, sold, processed etc
mean_kg <- c %>% 
  select(hhid, cfrid,  scode_ccm, atefresh_iweight, soldfresh_iweight, process_iweight, lost_iweight, other_iweight, catch_iweight) %>% 
  group_by(hhid) %>% 
  summarise(eat = sum(atefresh_iweight),
            sell = sum(soldfresh_iweight),
            process = sum(process_iweight),
            lost = sum(lost_iweight),
            other = sum(other_iweight),
            catch = sum(catch_iweight)) %>% 
  pivot_longer(!hhid, names_to = "Use", values_to = "Kg") %>% 
  group_by(Use) %>% 
  summarise(means = mean(Kg)) %>% 
  arrange(-means) 


# What proportion of fish are eaten, sold, processed, etc?
data <- q %>% 
  select(hhid, contains("_pct")) %>% 
  pivot_longer(!hhid, names_to = "Use", values_to = "Pct") %>% 
  group_by(Use) %>% 
  summarise(means = mean(Pct)) %>% 
  arrange(-means) 

names = c("56% Eaten", "20% Processed","16% Sold", "7% Lost", "2% Other")
pie(data$means, labels = names)
    

# What is the relationship between...

# quantity caught and quantity sold
q %>% 
  filter(sell!=0) %>% 
ggplot(aes(x = catch, y = sell)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(trans = "log1p") +
  scale_x_continuous(trans = "log1p") 

#tobit <- vglm(sell ~ catch, tobit(Lower = 0), data = q)


# quantity caught and quantity eaten
ggplot(q, aes(x = catch, y = eat)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(trans = "log1p") +
  scale_x_continuous(trans = "log1p") 

# quantity caught and quantity processed
q %>% 
  filter(process!=0) %>% 
ggplot(aes(x = catch, y = process)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(trans = "log1p") +
  scale_x_continuous(trans = "log1p") 



# quantity eaten and quantity processed

ggplot(q, aes(x = eat, y = process)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(trans = "log1p") +
  scale_x_continuous(trans = "log1p") 


# What to catch-eat plots look like for households that sell fish vs those that don't 
# hh that sell
q %>% 
  filter(sell != 0) %>% 
  ggplot(aes(x = catch, y = eat)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(trans = "log1p") +
  scale_x_continuous(trans = "log1p") 

q %>% 
  filter(sell == 0) %>% 
  ggplot(aes(x = catch, y = eat)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(trans = "log1p") +
  scale_x_continuous(trans = "log1p") 

