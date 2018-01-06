

library(tidyverse)
library(lubridate)
library(reshape)



# Einlesen der Temperaturschlagsdaten & Koordinaten


setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/EZG/output")
file1 <- "2017-12-08_P-output.txt"
file2 <- "2017-12-08_Temp-output.txt"

pst_P <- read_table(file1, col_names = F, cols( X1 = col_date(format = "%d%m%Y"),
                                                X2 = col_double()
                                                ))

pst_T <- read_table(file2, col_names = F, cols( X1 = col_date(format = "%d%m%Y"),
                                                X2 = col_double()
                                                ))
# cut timeseries to overlapping period from 01.01.1991 to 31.12.2014

pst_P1 <- pst_P[as_date(pst_P$X1) > as_date("1990-12-31"), ]
pst_T1 <- pst_T[as_date(pst_T$X1) < as_date("2015-01-01"), ]

# merge T and P and PET
P_T <- add_column(pst_T1,pst_P1$X2)

P_T$X1 <- format(P_T$X1, "%d%m%Y") # for input modna

#add pot ET (calculated)
setwd ("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/")
file3 <- "petout.txt"
PET <- read_table(file3, col_names = F, cols(X1 = col_date(format = "%d%m%Y"),
                                                 X2 = col_double()
))

P_T <- add_column(PET$X2) #doesnt work because PET has missing values (sometimes no 01.01., year starts with 02.01??)


# rounding to one digit

P_T$X2 <- round(P_T$X2,1)
P_T$`pst_P1$X2` <- round(P_T$`pst_P1$X2`,1)

setwd ("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/EZG/output/")
# commented to not overwrite stuff
 write.table(P_T,file = paste(format(Sys.time(), "%Y-%m-%d"),
                                       "_tnse", ".txt", sep = "") ,sep=",", row.names=FALSE,
             col.names = c("Datum", "t", "NSeff"), quote = F)
