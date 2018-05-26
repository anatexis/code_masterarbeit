library(tidyverse)
library(lubridate)
library(stringi)

setwd("C:/Users/Russ/Desktop/master/daten/output_R")
file <- "2018-05-23_weekly_air-temperature.txt"
file2 <- "2018-05-23_weekly_water-temperature.txt"

### get air temperature
air_t <- read_csv(file, col_names = T,
                        cols(week= "i",
                             .default=col_double())) %>% 
  round(., 2)

### get water temperature
water_t <- read_csv(file2, col_names = T,
                  cols(week= "i",
                       .default=col_double())) %>% 
  round(., 2)

### add the two together
air_water <- add_column(air_t,water_t$`water-temperature`)
air_water <- rename(air_water, c(week="week",`air-temperature`="air_t",
                                 `water_t$\`water-temperature\``="water_t")) #ignore the warning?


setwd ("C:/Users/Russ/Desktop/master/Daten/output_R/")

# commented out when written
# complete data
write.table(air_water,file = paste(format(Sys.time(), "%Y-%m-%d"),
                                "_air_water_weekly_temperature", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = c("week", "air_temperature", "water_temperature"),
            eol = "\r\n", quote = F)
