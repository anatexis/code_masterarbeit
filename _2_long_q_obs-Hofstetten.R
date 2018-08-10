library(tidyverse)
library(lubridate)
################### runoff

# WAS FEHLT: SACHEN RAUS, DIE NUR für LOICH VON BEDEUTUNG SIND

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)

file <- "Q207852-TM-Hofstetten(Bad).dat"

rff <- read_table(file, col_names = F, skip = 27, na = "Lücke", cols(
                X1 = col_date(format = "%d.%m.%Y"),
                X2 = col_time(format = ""),
                X3 = col_double()
))

sum(is.na(rff[3])) # just 

# #subsetting to epot beobachtungszeitraum bis 2014-01-16 [[loich!]]
# q_obs <- rff[as_date(rff$X1)<as_date("2014-01-17"), ]


# selecting only date and q
# select doesn't work when raster is loaded
if (isNamespaceLoaded("raster") == T) detach("package:raster", unload=TRUE)
q_obs <- select(rff, X1, X3)

## change formate for fortran input
q_obs$X1 <- format(q_obs$X1, "%d%m%Y") 

#write
path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_R"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/output_R//"
setwd(path)

write.table(q_obs,file = paste(format(Sys.time(), "%Y-%m-%d"),
                             "_Hofstetten_q_obs", ".txt", sep = "") ,sep=",",
            row.names=FALSE, col.names = c("Datum", "Q"),
            eol = "\r\n", quote = F)

