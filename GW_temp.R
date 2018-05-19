library(tidyverse)
library(lubridate)
library(stringi)
detach("package:hydroGOF", unload=TRUE)
setwd("C:/Users/Russ/Desktop/Masterarbeit/Grundwasser/")
file <- "GW_Temp_clean.csv"

### read in (cleaned) dataset of GW-temp-stations
gw_stations <- read_csv(file, col_names = c("stat","quarter","date","WT"),skip=1,
                        col_types = list(col_character(), col_character(), col_date(format = "%d.%m.%Y"), col_double()))
View(gw_stations)

gw_stations


### keine ahnung was da passiert!
#  gw_means1 sollte doch das gleiche sein wie gw_means2??
gw_q_means2 <- gw_stations %>% 
  filter(stat!="PG31900832",stat !="PG31900872")%>% #2 stat gefiltert weil zuwenige stationen
  group_by(stat,quarter) %>% 
  summarise(count = n(),
            quarter_mean = mean(WT)) 

View(gw_q_means2)

gw_means1 <- gw_q_means2 %>% 
  group_by(quarter) %>% 
  summarise(count = n(),
            quarter_mean = mean(quarter_mean))

View(gw_means1)

gw_means2 <- gw_stations %>% 
  filter(stat!="PG31900832",stat !="PG31900872") %>% 
  group_by(quarter) %>% 
  summarise(count = n(),
            quarter_mean = mean(WT)) 

View(gw_means2)






ggplot(gw_q_means, aes(x = quarter, y = stat, height = quarter_mean, group=stat)) + 
  geom_ridgeline(scale=.08)

ggplot(gw_q_means, aes(x = quarter, y = stat, fill = quarter_mean, group=stat)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016')
