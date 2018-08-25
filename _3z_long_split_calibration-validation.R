

#######################

library(tidyverse)
library(lubridate)
library(reshape)



# Einlesen von Qobs und P_T_ET


path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_R/"
if( .Platform$OS.type == "windows" )
        path <- "C:/Users/Russ/Desktop/master/daten/output_R/"
setwd(path)

file1 <- "2018-08-21P_T_ET-Hofstetten.txt"
file2 <- "2018-08-12_Hofstetten_q_obs.txt"

PTET <- read_csv(file1, col_names = T, cols(Datum = col_date(format = "%d%m%Y"),
                                             .default=col_double()))

Qobs <- read_csv(file2, col_names = T, cols( Datum = col_date(format = "%d%m%Y"),
                                                Q = col_double()))


# cut timeseries to overlapping period from 01.01.1991 to 16.01.2014 

PTET_valid <- PTET[as_date(PTET$Datum) > as_date("2003-12-31"), ]
PTET_calib <- PTET[as_date(PTET$Datum)  < as_date("2004-01-01"), ]

Qobs_valid <- Qobs[as_date(Qobs$Datum) > as_date("2003-12-31"), ]
Qobs_calib <- Qobs[as_date(Qobs$Datum) < as_date("2004-01-01"), ]




# change date format for input modna (müsste doch irgendwie mit listen gehen schas)

#function
change_date_format<- function(x) {
    x[1] <- format(x[[1]],"%d%m%Y")
}


PTET_valid$Datum <- change_date_format(PTET_valid)
PTET_calib$Datum <- change_date_format(PTET_calib)
Qobs_valid$Datum <- change_date_format(Qobs_valid)
Qobs_calib$Datum <- change_date_format(Qobs_calib)





path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_R/"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/output_R/"
setwd(path)


# commented out when written
# complete data

#PTET_valid
write.table(PTET_valid,file = paste(format(Sys.time(), "%Y-%m-%d"),
                                "_PTET_VALID_Hofstetten", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = c("Datum", "t", "NSeff", "ET"),
#add if on linux:           eol = "\r\n",
                                            quote = F)

#PTET_calib
write.table(PTET_calib,file = paste(format(Sys.time(), "%Y-%m-%d"),
                                    "_PTET_CALIB_Hofstetten", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = c("Datum", "t", "NSeff", "ET"),
            #add if on linux:           eol = "\r\n",
            quote = F)

#Qobs_valid !!!!!!!!!!!!!!!ACHTUNG für input in modna noch eine dummyzeile einfügen!!!!!
write.table(Qobs_valid,file = paste(format(Sys.time(), "%Y-%m-%d"),
                                    "_Qobs_VALID_Hofstetten", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = c("Datum", "Q"),
            #add if on linux:           eol = "\r\n",
            quote = F)

#Qobs_calib !!!!!!!!!!!!!!!!ACHTUNG für input in modna noch eine dummyzeile einfügen!!!!!
write.table(Qobs_calib,file = paste(format(Sys.time(), "%Y-%m-%d"),
                                    "_Qobs_CALIB_Hofstetten", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = c("Datum", "Q"),
            #add if on linux:           eol = "\r\n",
            quote = F)

