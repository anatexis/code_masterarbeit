

# TO IMPLEMENT
# P_T_ET with removed 29.02 P_T_ET_rm
# write P_T_ET_rm

#######################

library(tidyverse)
library(lubridate)
library(reshape)



# Einlesen der Temperaturschlagsdaten & Koordinaten


setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/EZG/output_R")
file1 <- "2017-12-08_P-output.txt"
file2 <- "2017-12-08_Temp-output.txt"

pst_P <- read_table(file1, col_names = F, cols( X1 = col_date(format = "%d%m%Y"),
                                                X2 = col_double()
                                                ))

pst_T <- read_table(file2, col_names = F, cols( X1 = col_date(format = "%d%m%Y"),
                                                X2 = col_double()
                                                ))
# cut timeseries to overlapping period from 01.01.1991 to 16.01.2014 (because of
# output from fortran)

pst_P1 <- pst_P[as_date(pst_P$X1) > as_date("1990-12-31"), ]
pst_P1 <- pst_P1[as_date(pst_P1$X1) < as_date("2014-01-17"), ]
pst_T1 <- pst_T[as_date(pst_T$X1) < as_date("2014-01-17"), ]

# merge T and P
P_T <- add_column(pst_T1,pst_P1$X2)

# rounding to one digit
P_T$X2 <- round(P_T$X2,1)
P_T$`pst_P1$X2` <- round(P_T$`pst_P1$X2`,1)

#removing all 29.02 from dataframw
P_T2902rm <- P_T[as.numeric(strftime(P_T$X1, "%m%d")) != 229,]

# change date format for input modna
P_T$X1 <- format(P_T$X1, "%d%m%Y") 
P_T2902rm$X1 <- format(P_T2902rm$X1, "%d%m%Y") 

# add pot ET (calculated from fortran epot_main.for)
setwd ("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/")
file3 <- "petout_fill.txt"
PET <- read_table(file3, col_names = F, cols(X1 = col_date(format = "%d%m%Y"),
                                                 X2 = col_double()
))

# P_T_ET variable with precipitation, temp and Evapotranspiration
P_T_ET <- add_column(P_T,PET$X2)

# to do: P_T_ET with removed 29.02.d



setwd ("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/EZG/output_R/")

# commented out when written
# complete data
# write.table(P_T_ET,file = paste(format(Sys.time(), "%Y-%m-%d"),"P_T_ET", ".txt",
#                                 sep = "") ,sep=",",row.names=FALSE,
#             col.names = c("Datum", "t", "NSeff", "ET"), eol = "\r\n", quote = F)
# 

#----------TO DO----------------
# 
# # write removed 29.02
# write.table(P_T2902rm,file = paste(format(Sys.time(), "%Y-%m-%d"),
#                              "_tnse2902rm", ".txt", sep = "") ,sep=",", row.names=FALSE,
#             col.names = c("Datum", "t", "NSeff"), quote = F)

