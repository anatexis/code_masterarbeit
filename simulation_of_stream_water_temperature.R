library(tidyverse)
library(lubridate)

detach("package:hydroGOF", unload=TRUE)


setwd("C:/Users/Russ/Desktop/master/daten/output_R")

# load air_water, discharge files
file <- "2018-06-03_air_groundwater_weekly_temperature.txt"
file2 <- "2018-05-27_percentage_of_fast_and_GW_weekly_discharge.txt"

air_gwater_temp <- read_csv(file, col_names = T)
discharge <- read_csv(file2, col_names = T)

# load weekly stream temperature data
file3 <- "2018-05-27_measured_weekly_streamwater_temperature.txt"
obs_temp <- read_csv(file3, col_names = T) %>% 
  select(., otemp = streamwater_temperature)


######
# simulation from simple to complex
require(hydroGOF)

# plot of observed temperature
simplot <- ggplot() +
  geom_line(data=obs_temp, aes(x=seq_along(obs_temp$otemp),
                               y=obs_temp$otemp), colour = "black", show.legend = T) +
  xlab("week") +
  ylab("stream water temperature [C°]")
simplot

### just air tempterature

sim1 <- air_gwater_temp[2]
ggof(sim1$air_temperature, obs_temp$otemp)

simplot <- simplot + geom_line(data=sim1, aes(x=seq_along(sim1$air_temperature),
                                       y=sim1$air_temperature),
                    colour = "red")
simplot

### just groundwater temperature
sim_gwater_stream_water <- air_gwater_temp[3]

sim1.2 <- as_tibble(sim_gwater_stream_water) %>% 
  select(.,stemp=groundwater_temperature)

ggof(sim1.2$stemp,obs_temp$otemp)

simplot <- simplot + geom_line(data=sim1.2, aes(x=seq_along(sim1.2$stemp),
                                              y=sim1.2$stemp),
                               colour = "blue")
simplot


### air temperature and groundwater temperature without discharge weight (just 50-50)

sim_air_stream_water <- air_gwater_temp[2]/2
sim_gwater_stream_water <- air_gwater_temp[3]/2

sim2 <- as_tibble(sim_air_stream_water+sim_gwater_stream_water) %>% 
  select(.,stemp=air_temperature)

ggof(sim2$stemp,obs_temp$otemp)

simplot <- simplot + geom_line(data=sim2, aes(x=seq_along(sim2$stemp),
                                              y=sim2$stemp),
                               colour = "green")
simplot


### air temperature and groundwater temperature weighted by discharge

sim_air_stream_water <- air_gwater_temp[2]*discharge[2]
sim_gwater_stream_water <- air_gwater_temp[3]*discharge[3] # auch ienmal ohne proz gewichtung machen"!

sim3 <- as_tibble(sim_air_stream_water+sim_gwater_stream_water) %>% 
  select(.,stemp=air_temperature)

ggof(sim3$stemp,obs_temp$otemp)

simplot <- simplot + geom_line(data=sim3, aes(x=seq_along(sim3$stemp),
                                              y=sim3$stemp),
                               colour = "violet")
simplot ### way worse than with sim2 (which is just half groundwater temperature and half airtemperature...)



######## sources

# plot just one variable: https://stackoverflow.com/a/13837856

### (to implement for efficiency comparison:) read in (cleaned) dataset of stream-water-temp-stations

