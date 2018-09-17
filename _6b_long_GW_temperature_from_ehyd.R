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

file1 <- "Grundwassertemperatur-Monatsmittel-322917.csv" #1.näheste Station
file2 <- "Grundwassertemperatur-Monatsmittel-322891.csv" #2.
file3 <- "Grundwassertemperatur-Monatsmittel-327148.csv" #3.
file4 <- "Grundwassertemperatur-Monatsmittel-337055.csv" #4.
file5 <- "Grundwassertemperatur-Monatsmittel-327106.csv" #5.
file6 <- "Grundwassertemperatur-Monatsmittel-327122.csv" #6. fernste stat

# the data are the monthly means

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

# loop for plot

for (i in seq(6)){
  plot_i <- eval(as.symbol(paste("pst",i,sep=""))) # quelle: https://stackoverflow.com/a/32954322
  plot(plot_i, type="l")
  
}

# loop for ggplot with trend

for (i in seq(6)) {
  plot_i <- eval(as.symbol(paste("pst",i,sep=""))) # quelle: https://stackoverflow.com/a/32954322
  ploti <- ggplot(data = plot_i, aes(x = X1, y = X2) )+
    geom_line()+
    geom_smooth(method="lm")+
    ggtitle(paste("Station"," Nr.", i, sep=""))
  print(ploti)
#  print(max(plot_i$X2, na.rm=T))
#  print(min(plot_i$X2, na.rm=T))
#   print(tail(plot_i))
  }

