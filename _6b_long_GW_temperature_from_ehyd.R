library(tidyverse)
library(sp)
library(gstat)
library(rgdal)
library(lubridate)
library(reshape)


# Einlesen der GW-Daten


path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)

file1 <- "Grundwassertemperatur-Monatsmittel-322891.csv"
file2 <- "Grundwassertemperatur-Monatsmittel-322917.csv"
file3 <- "Grundwassertemperatur-Monatsmittel-327106.csv"
file4 <- "Grundwassertemperatur-Monatsmittel-327122.csv"
file5 <- "Grundwassertemperatur-Monatsmittel-327148.csv"
file6 <- "Grundwassertemperatur-Monatsmittel-337055.csv"

pst1 <- read_csv2(file1, col_names = F, skip = 34, na = "Lücke",cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
)) %>% select(.,X1,X2)
# because of col_date it takes just the date not the time (specified with %T)

pst2 <- read_csv2(file2, col_names = F, skip = 34, na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
)) %>% select(.,X1,X2)

pst3 <- read_csv2(file3, col_names = F, skip = 34, na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
)) %>% select(.,X1,X2)

pst4 <- read_csv2(file4, col_names = F, skip = 34, na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
)) %>% select(.,X1,X2)

pst5 <- read_csv2(file5, col_names = F, skip = 34, na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
)) %>% select(.,X1,X2)

pst6 <- read_csv2(file6, col_names = F, skip = 34, na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
)) %>% select(.,X1,X2)


# last entry is "Lücke" so we cut it off

pst1 <- pst1[as_date(pst1$X1) < as_date("2016-01-01"), ]
pst2 <- pst2[as_date(pst2$X1) < as_date("2016-01-01"), ]
pst3 <- pst3[as_date(pst3$X1) < as_date("2016-01-01"), ]
pst4 <- pst4[as_date(pst4$X1) < as_date("2016-01-01"), ]
pst5 <- pst5[as_date(pst5$X1) < as_date("2016-01-01"), ]
pst6 <- pst6[as_date(pst6$X1) < as_date("2016-01-01"), ]


# # last two stations start at 1981 so we have to cut the others to this date
# 
# pst1 <- pst1[as_date(pst1$X1) > as_date("1980-12-31"), ]
# pst2 <- pst2[as_date(pst2$X1) > as_date("1980-12-31"), ]
# pst3 <- pst3[as_date(pst3$X1) > as_date("1980-12-31"), ]
# pst4 <- pst4[as_date(pst4$X1) > as_date("1980-12-31"), ]
# pst5 <- pst5[as_date(pst5$X1) > as_date("1980-12-31"), ]
# pst6 <- pst6[as_date(pst6$X1) > as_date("1980-12-31"), ]

# put all P-data in one data-frame

pst1 <- add_column(pst1,pst2$X2,pst3$X2,pst4$X2, pst5$X2,pst6$X2)
pst1 <- rename(pst1, c(X1="date",X2="N107177", 'pst2$X2'="N107193",'pst3$X2'="N107300",
                       'pst4$X2'="N107466",'pst5$X2'="N107318",'pst6$X2'="N107334",
                       'pst7$X2'="N109082",'pst8$X2'="N109074"))
for (i in seq(6)){
  plot_i <- eval(as.symbol(paste("pst",i,sep=""))) # quelle: https://stackoverflow.com/a/32954322
  plot(plot_i, type="l")
  
}
