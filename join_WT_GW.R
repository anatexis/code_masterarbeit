library(tidyverse)
library(lubridate)
library(stringi)

setwd("C:/Users/Russ/Desktop/master/daten/output_R")
file <- "2018-05-23_weekly_air-temperature.txt"
file2 <- "2018-06-03_weekly_groundwater-temperature_lin_intpol.txt"

### get air temperature
air_t <- read_csv(file, col_names = T,
                        cols(week= "i",
                             .default=col_double())) %>% 
  round(., 2)

### get water temperature
gwater_t <- read_csv(file2, col_names = T,
                  cols(week= "i",
                       .default=col_double())) %>% 
  round(., 2)

### add the two together
air_gwater <- add_column(air_t,gwater_t$groundwater_temperature)
air_gwater <- plyr::rename(air_gwater, c(week="week",`air-temperature`="air_t",
                                   `gwater_t$groundwater_temperature`="gwater_t"))
#dplyr macht irgendwas komisch deswegen mit plyr


setwd ("C:/Users/Russ/Desktop/master/Daten/output_R/")

#commented out when written
# complete data
write.table(air_gwater,file = paste(format(Sys.time(), "%Y-%m-%d"),
                                "_air_groundwater_weekly_temperature", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = c("week", "air_temperature", "groundwater_temperature"),
            eol = "\r\n", quote = F)
