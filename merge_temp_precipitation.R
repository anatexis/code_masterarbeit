

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

# merge datasets
P_T <- add_column(pst_T1,pst_P1$X2)

P_T$X1 <- format(P_T$X1, "%d%m%Y") # für input modna



setwd ("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/EZG/output//")
# comment not to overwrite stuff
# write.table(P_T,file = paste(format(Sys.time(), "%Y-%m-%d"),
#                                       "_tnse", ".txt", sep = "") ,sep=" ", row.names=FALSE,
#             col.names = c("Datum", "t", "NSeff"), quote = F)
