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

sim_stream_temp <- sim_air_stream_water+sim_gwater_stream_water ### RENAME VARIABLE NAME!!

plot(sim_air_stream_water$air_temperature)
plot(sim_gwater_stream_water$groundwater_temperature)
plot(sim_stream_temp$air_temperature)

ggplot(sim_stream_temp, aes(x=seq_along(sim_stream_temp$air_temperature),
                            y=sim_stream_temp$air_temperature)) +
  geom_point() +
  xlab("week") +
  ylab("simulated stream water temperature [C°]")+
  geom_smooth(method = "loess") #confidence intervall seems way too little?! .95 and moste of the points are outside??


### comparison to measured stream water temperature
# load weekly stream temperature data
#
setwd("C:/Users/Russ/Desktop/master/daten/output_R")
file3 <- "2018-05-27_measured_weekly_streamwater_temperature.txt"

measured_stream_water_temperature <- read_csv(file3, col_names = T)

require(hydroGOF) ## VARIABLE OBEN ANPASSEN (sim_stream_temp$air_temperature sollte $sim_stream water_temp sein)
nse <- NSE(sim_stream_temp$air_temperature,measured_stream_water_temperature$streamwater_temperature)
kge <- KGE(sim_stream_temp$air_temperature,measured_stream_water_temperature$streamwater_temperature)

###PLOT ANPASSEN!!

# Q <- ggplot(data= discharge)+
#   geom_line( aes(x=TTMMYYYY, y=Qobs, color = "Qobs"))+
#   geom_line( aes(x=TTMMYYYY, y=qsim, color = "Qsim"))+
#   geom_line( aes(x=TTMMYYYY, y=linout, color = "lin"))+
#   geom_line( aes(x=TTMMYYYY, y=cascout, color = "casc"))+
#   xlab("Date")+
#   ylab("Discharge [mm]")+
#   annotate("text", x=as.Date(16022), y=37,label="nse= ")+
#   annotate("text", x=as.Date(16064), y=37,label=as.character(round(nse,2)))+
#   annotate("text", x=as.Date(16022), y=34,label="kge= ")+
#   annotate("text", x=as.Date(16064), y=34,label=as.character(round(kge,2)))+
#   scale_color_manual(values=c("Qobs"="#00BFC4", "Qsim"="#C77CFF",
#                               "lin"="#7CAE00", "casc"="#F8766D"))
# 
# Q
#
##
#
#simulation machen!!



## sources

# plot just one variable: https://stackoverflow.com/a/13837856

### (to implement for efficiency comparison:) read in (cleaned) dataset of stream-water-temp-stations

