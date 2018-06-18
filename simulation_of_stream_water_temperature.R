library(tidyverse)
library(lubridate)

detach("package:hydroGOF", unload=TRUE)


setwd("C:/Users/Russ/Desktop/master/daten/output_R")

# load air_water, discharge files
file <- "2018-06-10_air_groundwater_weekly_temperature.txt"
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
#ggof(sim1$air_temperature, obs_temp$otemp)

simplot_at <- simplot + geom_line(data=sim1, aes(x=seq_along(sim1$air_temperature),
                                       y=sim1$air_temperature),
                    colour = "red")
simplot_at

### just groundwater temperature
sim_gwater_stream_water <- air_gwater_temp[3]

sim1.2 <- as_tibble(sim_gwater_stream_water) %>% 
  select(.,stemp=groundwater_temperature)

#ggof(sim1.2$stemp,obs_temp$otemp)

simplot_wt <- simplot +
  geom_line(data=sim1, aes(x=seq_along(sim1$air_temperature),
                           y=sim1$air_temperature), colour = "grey") +
  geom_line(data=sim1.2, aes(x=seq_along(sim1.2$stemp), y=sim1.2$stemp), colour = "red")
simplot_wt


### air temperature and groundwater temperature without discharge weight (just 50-50)

sim_air_stream_water <- air_gwater_temp[2]/2
sim_gwater_stream_water <- air_gwater_temp[3]/2

sim2 <- as_tibble(sim_air_stream_water+sim_gwater_stream_water) %>% 
  select(.,stemp=air_temperature)

#ggof(sim2$stemp,obs_temp$otemp)

simplot_at_wt_uw <- simplot +
  geom_line(data=sim1.2, aes(x=seq_along(sim1.2$stemp),
                             y=sim1.2$stemp), colour = "grey")+
  geom_line(data=sim1, aes(x=seq_along(sim1$air_temperature),
                           y=sim1$air_temperature), colour = "grey") + 
  geom_line(data=sim2, aes(x=seq_along(sim2$stemp), y=sim2$stemp), colour = "red")

simplot_at_wt_uw


### air temperature and groundwater temperature weighted by discharge

sim_air_stream_water <- air_gwater_temp[2]*discharge[2]
sim_gwater_stream_water <- air_gwater_temp[3]*discharge[3] # auch ienmal ohne proz gewichtung machen"!

sim3 <- as_tibble(sim_air_stream_water+sim_gwater_stream_water) %>% 
  select(.,stemp=air_temperature)

#ggof(sim3$stemp,obs_temp$otemp)

simplot_at_wt_Qw <- simplot +
  geom_line(data=sim2, aes(x=seq_along(sim2$stemp),
                           y=sim2$stemp),colour = "grey")+
  geom_line(data=sim1.2, aes(x=seq_along(sim1.2$stemp),
                             y=sim1.2$stemp),colour = "grey")+
  geom_line(data=sim1, aes(x=seq_along(sim1$air_temperature),
                           y=sim1$air_temperature),colour = "grey") + 
  geom_line(data=sim3, aes(x=seq_along(sim3$stemp), y=sim3$stemp),colour = "red") 

simplot_at_wt_Qw ### way worse than with sim2 (which is just half groundwater temperature and half airtemperature...)

### mohseni
simplot_mohseni <- simplot +
  geom_line(data=sim2, aes(x=seq_along(sim2$stemp),
                           y=sim2$stemp),colour = "grey")+
  geom_line(data=sim1.2, aes(x=seq_along(sim1.2$stemp),
                             y=sim1.2$stemp),colour = "grey")+
  geom_line(data=sim1, aes(x=seq_along(sim1$air_temperature),
                           y=sim1$air_temperature),colour = "grey") + 
  geom_line(data=sim3, aes(x=seq_along(sim3$stemp), y=sim3$stemp),colour = "grey") +
  geom_line(data=T_s, aes(x=seq_along(alltogether_m$sim_swt_mohseni),
                        y=alltogether_m$sim_swt_mohseni),
          colour = "red")
simplot_mohseni

######## sources

# plot just one variable: https://stackoverflow.com/a/13837856

### (to implement for efficiency comparison:) read in (cleaned) dataset of stream-water-temp-stations



###############################################################################################
################### test-test-test plot machen wie fig.2 mohseni et al. 1998
# join obs_t + air_gwater_t 

require(ggrepel)

alltogether <- add_column(air_gwater_temp, obs_temp$otemp)
alltogether <- plyr::rename(alltogether, c(week="week",
                                           air_temperature="air_t",
                                            groundwater_temperature="gwater_t",
                                           `obs_temp$otemp`="obs_streamw_t"))
ggplot(alltogether, aes(x=air_t,y=obs_streamw_t)) +
  geom_path()+
  geom_point() +
  geom_label_repel(aes(label = ifelse(week%%2 ==1, as.character(week),"")))

plot(alltogether$air_t, alltogether$obs_streamw_t)


### mohseni geschätzt
mu <- 0.0
a <- 19
b <- 10
gam <- .12
T_s <- as_tibble(mu+(a-mu)/(1+exp(gam*(b-alltogether$air_t))))
alltogether_m <- add_column(alltogether, T_s$value)
alltogether_m <- plyr::rename(alltogether_m, c(week="week",
                                           air_t="air_t",
                                           gwater_t="gwater_t",
                                           obs_streamw_t="obs_streamw_t",
                                           `T_s$value`="sim_swt_mohseni"))

## plotten
simplot_test <- ggplot() +
  geom_line(data=obs_temp, aes(x=seq_along(obs_temp$otemp),
                               y=obs_temp$otemp), colour = "black", show.legend = T) +
  xlab("week") +
  ylab("stream water temperature [C°]")
simplot_test
simplot_test+ geom_line(data=T_s, aes(x=seq_along(alltogether_m$sim_swt_mohseni),
                                       y=alltogether_m$sim_swt_mohseni),
                        colour = "yellow")

ggof(alltogether_m$sim_swt_mohseni,obs_temp$otemp)


######################## Fehler modell #### error plot
alltogether_e_p <- add_column(alltogether,
                              dis_perc_fast = discharge$percentage_Fast,
                              dis_perc_slow = discharge$percentage_GW )
error_plot <- ggplot(alltogether_e_p,
                     aes(x=week,
                        y = (dis_perc_fast*air_t+dis_perc_slow*gwater_t)-obs_streamw_t))+
  geom_line()+
  geom_smooth()
error_plot

### sources for labeling: https://stackoverflow.com/a/48762376
