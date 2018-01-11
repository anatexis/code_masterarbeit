library(tidyverse)
library(lubridate)
################### runoff
setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten")
file <- "Q214312-TM-Loich.dat"

rff <- read_table(file, col_names = F, skip = 26, cols(
                X1 = col_date(format = "%d.%m.%Y"),
                X2 = col_time(format = ""),
                X3 = col_double()
))

#subsetting to epot beobachtungszeitraum bis 2014-01-16
q_obs <- rff[as_date(rff$X1)<as_date("2014-01-17"), ]

# m³/s to mm/d 
q_obs <- mutate(q_obs,
       q_mm=round(X3*1000*60*60*24/142494769,2))

# selecting only date and q
# select doesn't work when raster is loaded
if (isNamespaceLoaded("raster") == T) detach("package:raster", unload=TRUE)
q_obs <- select(q_obs, X1, q_mm)

## change formate for fortran input
q_obs$X1 <- format(q_obs$X1, "%d%m%Y") 

#write
setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_R")

write.table(q_obs,file = paste(format(Sys.time(), "%Y-%m-%d"),
                             "_q_obs", ".txt", sep = "") ,sep=",",
            row.names=FALSE, col.names = c("Datum", "Q"),
            eol = "\r\n", quote = F)

