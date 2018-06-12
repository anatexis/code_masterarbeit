library(tidyverse)
library(lubridate)
library(stringi)
detach("package:hydroGOF", unload=TRUE)
setwd("C:/Users/Russ/Desktop/Masterarbeit/Grundwasser/")
file <- "GW_Temp_clean.csv"

### read in (cleaned) dataset of GW-temp-stations
gw_stations <- read_csv(file, col_names = c("stat","quarter","date","WT"),skip=1,
                        col_types = list(col_character(), col_character(), col_date(format = "%d.%m.%Y"), col_double()))
#View(gw_stations)

gw_stations


### zusammenfassen der GW-stationen
# gruppieren nach stationen und quartalen
# mittlere WT pro quartal pro station

target <- c("PG31900572", "PG31900562")

gw_q_means <- gw_stations %>% 
  filter(stat %in% target) %>% # 2 nächste stat
  group_by(stat,quarter) %>% 
  summarise(count = n(),
            quarter_mean = mean(WT)) 



# gruppiert nur nach quartalen
gw_means2 <- gw_stations %>% 
  filter(stat %in% target) %>% # 2 nächste stat
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

ggplot(gw_q_means, aes(x = quarter, y = quarter_mean)) +
  geom_point(aes(colour = count), size = 2) +
  scale_colour_gradientn(colours=rainbow(6)) +
  facet_grid(stat~quarter)




ggplot(gw_means2, aes(x = quarter, y = quarter_mean), colours=count) +
  geom_point(aes(colour = count), size= 3) +
  scale_colour_gradientn(colours=rainbow(6))

# weiß nicht wirklich wie ich die GW-Temperatur ansetzen soll - ich hab das mittel im quartal
# und dazu die varianz, aber ich brauche wöchentliche werte. ich kann nicht einfach eine normalverteilung
# im quartal annehmen!!? (1 quartal sind 13 wochen bzw. 14 fürs letzte, weil da ja einm messwert zuviel
# rauskommt vom modmod, da kommt irgendwas raus):plot(rnorm(13,gw_means2$quarter_mean[1],gw_means2$quarter_variance[1]))
# ich probiere einfach mal die quartalswerte für die wochenwerte einzusetzen
multi <- c(13,13,13,14)


gw_temp <- tibble(weeks=seq(1:53),water_temperature=rep(gw_means2$quarter_mean,multi))

setwd ("C:/Users/Russ/Desktop/master/Daten/output_R/")


### linear interpoliert

t <- c(1,13,26,39,53)
dat <- gw_means2$quarter_mean
dat <- append(dat,gw_means2$quarter_mean[1])

f <- approxfun(t,dat)

lin_int_gw <- round(f(seq(from=1, to=53, by=1)),2)
plot(lin_int_gw)

gw_temp_linint <- tibble(weeks=seq(1:53),gwater_temperature=lin_int_gw)
# comment out when written
# complete data
write.table(gw_temp_linint,file = paste(format(Sys.time(), "%Y-%m-%d"),
                                "_weekly_groundwater-temperature_lin_intpol", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = c("week", "groundwater_temperature"),
            eol = "\r\n", quote = F)



## sources

# facet_grid: http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
