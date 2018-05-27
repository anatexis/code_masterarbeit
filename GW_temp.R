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


### zusammenfassen der GW-stationen
# gruppieren nach stationen und quartalen
# mittlere WT pro quartal pro station
gw_q_means <- gw_stations %>% 
  filter(stat!="PG31900832",stat !="PG31900872")%>% #2 stat gefiltert weil zuwenige stationen
  group_by(stat,quarter) %>% 
  summarise(count = n(),
            quarter_mean = mean(WT)) 

all_gw_q_means <- gw_stations %>% 
  group_by(stat,quarter) %>% 
  summarise(count = n(),
            quarter_mean = mean(WT)) 


# # mittlere wt temp pro quartal (ist aber falsch gewichtet, stationen mit wenigen daten
# # werden im vergleich stärker gewichtet als stationen mit vielen daten)
# gw_means1 <- gw_q_means %>% 
#   group_by(quarter) %>% 
#   summarise(count = n(),
#             quarter_mean = mean(quarter_mean))


# gruppiert nur nach quartalen
gw_means2 <- gw_stations %>% 
  filter(stat!="PG31900832",stat !="PG31900872") %>% 
  group_by(quarter) %>% 
  summarise(count = n(),
            quarter_mean = mean(WT),
            quarter_variance = var(WT)) 

# View(gw_q_means2)
# View(gw_means1)
 View(gw_means2)


ggplot(gw_q_means, aes(x = quarter, y = quarter_mean)) +
  geom_point(aes(colour = count), size = 2) +
  scale_colour_gradientn(colours=rainbow(6)) +
  facet_grid(quarter~stat)

ggplot(all_gw_q_means, aes(x = quarter, y = quarter_mean)) +
  geom_point(aes(colour = count), size = 2) +
  scale_colour_gradientn(colours=rainbow(6)) +
  facet_grid(quarter~stat)

# maybe remove station


ggplot(gw_means2, aes(x = quarter, y = quarter_mean), colours=count) +
  geom_point(aes(colour = count), size= 3) +
  geom_boxplot(aes(quarter)) +
  scale_colour_gradientn(colours=rainbow(6))

# weiß nicht wirklich wie ich die GW-Temperatur ansetzen soll - ich hab das mittel im quartal
# und dazu die varianz, aber ich brauche wöchentliche werte. ich kann nicht einfach eine normalverteilung
# im quartal annehmen!!? (1 quartal sind 13 wochen bzw. 14 fürs letzte, weil da ja einm messwert zuviel
# rauskommt vom modmod, da kommt irgendwas raus):plot(rnorm(13,gw_means2$quarter_mean[1],gw_means2$quarter_variance[1]))
# ich probiere einfach mal die quartalswerte für die wochenwerte einzusetzen
multi <- c(13,13,13,14)

gw_temp <- tibble(weeks=seq(1:53),water_temperature=rep(gw_means2$quarter_mean,multi))

setwd ("C:/Users/Russ/Desktop/master/Daten/output_R/")

# # comment out when written
# # complete data
# write.table(gw_temp,file = paste(format(Sys.time(), "%Y-%m-%d"),
#                                 "_weekly_groundwater-temperature", ".txt", sep = "") ,sep=",",
#             row.names=FALSE,col.names = c("week", "groundwater_temperature"),
#             eol = "\r\n", quote = F)



## sources

# facet_grid: http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
