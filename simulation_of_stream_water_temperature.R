library(tidyverse)
library(lubridate)

detach("package:hydroGOF", unload=TRUE)

### load air_water, discharge files

setwd("C:/Users/Russ/Desktop/master/daten/output_R")
file <- "2018-05-27_air_groundwater_weekly_temperature.txt"
file2 <- "2018-05-27_percentage_of_fast_and_GW_weekly_discharge.txt"


air_gwater_temp <- read_csv(file, col_names = T)

discharge <- read_csv(file2, col_names = T)



sim_air_stream_water <- air_gwater_temp[2]*discharge[2]
sim_gwater_stream_water <- air_gwater_temp[3]*discharge[3]

sim_temp <- as_tibble(sim_air_stream_water+sim_gwater_stream_water) %>% 
  select(.,stemp=air_temperature)


plot(sim_air_stream_water$air_temperature)
plot(sim_gwater_stream_water$groundwater_temperature)
plot(sim_stream_temp$air_temperature)

ggplot(sim_temp, aes(x=seq_along(sim_temp$stemp),
                            y=sim_temp$stemp)) +
  ylim(0,16) +
  geom_point() +
  xlab("week") +
  ylab("simulated stream water temperature [C°]")+
  geom_smooth(method = "loess") #confidence intervall seems way too little?! .95 and moste of the points are outside??


### comparison to measured stream water temperature
# load weekly stream temperature data
#
setwd("C:/Users/Russ/Desktop/master/daten/output_R")
file3 <- "2018-05-27_measured_weekly_streamwater_temperature.txt"

obs_temp <- read_csv(file3, col_names = T) %>% 
  select(., otemp = streamwater_temperature)

ggplot(obs_temp, aes(x=seq_along(obs_temp$otemp),
                     y=obs_temp$otemp)) +
  ylim(0,16) +
  geom_point() +
  xlab("week") +
  ylab("simulated stream water temperature [C°]")+
  geom_smooth(method = "loess") #confidence intervall seems way too little?! .95 and moste of the points are outside??





require(hydroGOF) ## VARIABLE OBEN ANPASSEN (sim_stream_temp$air_temperature sollte $sim_stream water_temp sein)
nse <- NSE(sim_temp$stemp,obs_temp$otemp)
kge <- KGE(sim_temp$stemp,obs_temp$otemp)



gof(sim_temp$stemp,obs_temp$otemp)
ggof(sim_temp$stemp,obs_temp$otemp)




## sources

# plot just one variable: https://stackoverflow.com/a/13837856

### (to implement for efficiency comparison:) read in (cleaned) dataset of stream-water-temp-stations

