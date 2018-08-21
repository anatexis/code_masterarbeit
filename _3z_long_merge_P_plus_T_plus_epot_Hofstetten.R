

#######################

library(tidyverse)
library(lubridate)
library(reshape)



# Einlesen der Temperaturschlagsdaten & Koordinaten


path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_R/"
if( .Platform$OS.type == "windows" )
        path <- "C:/Users/Russ/Desktop/master/daten/output_R/"
setwd(path)

file1 <- "2018-08-11_P-output_HOFSTETTEN.txt"
file2 <- "2018-08-11_Temp-output_Hofstetten.txt"

pst_P <- read_table(file1, col_names = F, cols( X1 = col_date(format = "%d%m%Y"),
                                                X2 = col_double()
                                                ))

pst_T <- read_table(file2, col_names = F, cols( X1 = col_date(format = "%d%m%Y"),
                                                X2 = col_double()
                                                ))
# cut timeseries to overlapping period from 01.01.1991 to 16.01.2014 

pst_P1 <- pst_P[as_date(pst_P$X1) > as_date("1990-12-31"), ]
pst_P1 <- pst_P1[as_date(pst_P1$X1) < as_date("2015-01-01"), ]

pst_T <- pst_T[as_date(pst_T$X1) > as_date("1990-12-31"), ]
pst_T1 <- pst_T[as_date(pst_T$X1) < as_date("2015-01-01"), ]

# merge T and P
P_T <- add_column(pst_T1,pst_P1$X2)

# rounding to one digit
P_T$X2 <- round(P_T$X2,1)
P_T$`pst_P1$X2` <- round(P_T$`pst_P1$X2`,1)

#removing all 29.02 from dataframw
P_T2902rm <- P_T[as.numeric(strftime(P_T$X1, "%m%d")) != 229,]

# add pot ET (calculated from fortran epot_main.for) and corrected
# with "_3clong_correct...output.R" script

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_etpot_main/"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/output_etpot_main/"
setwd(path)

file3 <- "2018-08-15petout_hofstn_korr.txt"


PET <- read_csv(file3, col_names = c("X1","X2"),col_types = c("c","d")) #schreit fehler aber passt!??
# library(stringi)
# ### to get r to read in files with in the form of
# ### dmmyyy AND ddmmyyy we have to do smt like this:
# stri_sub(PET$X1,-6,0) <- "-"
# stri_sub(PET$X1,-4,0) <- "-"
# PET$X1 <- as.Date(PET$X1, "%d-%m-%Y")

PET$X1 <- as.Date(PET$X1, "%Y-%m-%d")

PET <- PET[as_date(PET$X1) > as_date("1990-12-31"), ]
PET <- PET[as_date(PET$X1) < as_date("2015-01-01"), ]

# P_T_ET variable with precipitation, temp and Evapotranspiration
P_T_ET <- add_column(P_T2902rm,PET$X2)

# change date format for input modna
P_T_ET$X1 <- format(P_T_ET$X1, "%d%m%Y") 


path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_R/"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/output_R/"
setwd(path)


# commented out when written
# complete data
write.table(P_T_ET,file = paste(format(Sys.time(), "%Y-%m-%d"),
                                "P_T_ET-Hofstetten", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = c("Datum", "t", "NSeff", "ET"),
#add if on linux:           eol = "\r\n", 
                                            quote = F)
