

#######################

library(tidyverse)
library(lubridate)
library(reshape)



# Einlesen der Temperaturschlagsdaten & Koordinaten


setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_R")
file1 <- "2017-12-08_P-output.txt"
file2 <- "2017-12-08_Temp-output.txt"

pst_P <- read_table(file1, col_names = F, cols( X1 = col_date(format = "%d%m%Y"),
                                                X2 = col_double()
                                                ))

pst_T <- read_table(file2, col_names = F, cols( X1 = col_date(format = "%d%m%Y"),
                                                X2 = col_double()
                                                ))
# cut timeseries to overlapping period from 01.01.1991 to 16.01.2014 

pst_P1 <- pst_P[as_date(pst_P$X1) > as_date("2012-12-31"), ]
pst_P1 <- pst_P1[as_date(pst_P1$X1) < as_date("2014-01-01"), ]

pst_T <- pst_T[as_date(pst_T$X1) > as_date("2012-12-31"), ]
pst_T1 <- pst_T[as_date(pst_T$X1) < as_date("2014-01-01"), ]

# merge T and P
P_T <- add_column(pst_T1,pst_P1$X2)

# rounding to one digit
P_T$X2 <- round(P_T$X2,1)
P_T$`pst_P1$X2` <- round(P_T$`pst_P1$X2`,1)

# add pot ET (calculated from fortran epot_main.for)
setwd ("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/")
file3 <- "petout_richtig3.txt"
PET <- read_table(file3, col_names = F, cols(X1 = col_date(format = "%d%m%Y"),
                                                 X2 = col_double()
))

PET <- PET[as_date(PET$X1) > as_date("2012-12-31"), ]
PET <- PET[as_date(PET$X1) < as_date("2014-01-01"), ]

# P_T_ET variable with precipitation, temp and Evapotranspiration
P_T_ET <- add_column(P_T,PET$X2)

# start timeseries at 20.11.2012 because thats earliest q observation
P_T_ET <- P_T_ET[as_date(P_T_ET$X1) > as_date("2012-12-31"), ]

# change date format for input modna
P_T_ET$X1 <- format(P_T_ET$X1, "%d%m%Y") 


#removing all 29.02 from dataframe and change formate (there is no 29.02 in ts 
# from 20.11.2012-17.01.2014 but maybe we get longer data for q)
P_T2902rm <- P_T_ET[as.numeric(strftime(P_T$X1, "%m%d")) != 229,]



setwd ("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_R/")

# commented out when written
# complete data
write.table(P_T_ET,file = paste(format(Sys.time(), "%Y-%m-%d"),
                                "P_T_ET-2013", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = c("Datum", "t", "NSeff", "ET"),
            eol = "\r\n", quote = F)
