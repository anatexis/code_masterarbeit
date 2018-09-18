library(tidyverse)
library(lubridate)
library(stringi)
library(timetk)
library(xts)

detach("package:hydroGOF", unload=TRUE)

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
file6 <- "Grundwassertemperatur-Monatsmittel-327122.csv" #6. fernste station

for(i in seq(6)){
  gw_i <- paste("gwst",i,sep="")
  assign(gw_i,read_csv2(eval(as.symbol(paste("file",i,sep=""))),
                        col_names = F, skip = 34, na = "Lücke",cols(
                        X1 = col_date(format = "%d.%m.%Y %T"), 
                        X2 = col_double())) %>% select(.,X1,X2))
}

# der loop oben macht das gleiche wie in _6c_ ab Zeile 100

############################################################################
################## interpolation#######################################

# 
# gwst1_ts <- xts(gwst1[,-1], order.by = gwst1$X1)
# gwst1_ts_1989 <- gwst1_ts[1:13,]
# plot(gwst1_ts_1989)
# 
# # vor 1989 haben wir nichts, deswegen startet t (=die wochen) mit der 2. woche also 
# # mitte Jänner
# t <- seq(from=2, to=53, by=4)
# dat <- gwst1_ts_1989$X2
# 
# f <- approxfun(t,dat)
# 
# lin_int_gw <- round(f(seq(from=1, to=53, by=1)),2)
# plot(lin_int_gw) # gleicher plot wie monatlich! nice!
# 
# 
# # test mit 2003 (geht auch ohne umformung in xts objekt)
# gw_2003 <- gwst1$X2[168:182] # dez 2002 bis jan 2004
# plot(gw_2003)
# 
# t <- seq(from=-2, to=54, by=4)
# dat <- gw_2003
# 
# f <- approxfun(t,dat)
# 
# lin_int_gw <- round(f(seq(from=1, to=53, by=1)),2)
# plot(lin_int_gw) # gleicher plot wie monatlich! nice!

#### INTERPOLATION for years 1991 - 2003 für station gwst1

for(i in seq(2003-1991+1)){
  gw_year <- gwst1[24:length(gwst1$X2),]
  
  dat_i <- gw_year$X2[((i-1)*12+1):(i*12+3)]
  
  t <- seq(from=-2, to=54, by=4)
  
  f <- approxfun(t,dat_i)
  
  lin_int_gw <- tibble(week=1:53,gw_temp=round(f(seq(from=1, to=53, by=1)),2))
  
  ploti <- ggplot(data = lin_int_gw, aes(x = week, y=gw_temp) )+
    geom_point()+
    ggtitle(paste("Year", i+1990, sep=" "))
  print(ploti)
}

#### INTERPOLATION for years 1995 - 2003 für station gwst4
for(i in seq(2003-1995+1)){
  gw_year <- gwst4[8:length(gwst4$X2),]
  
  dat_i <- gw_year$X2[((i-1)*12+1):(i*12+3)]
  
  t <- seq(from=-2, to=54, by=4)
  
  f <- approxfun(t,dat_i)
  
  lin_int_gw <- tibble(week=1:53,gw_temp=round(f(seq(from=1, to=53, by=1)),2))
  
  ploti <- ggplot(data = lin_int_gw, aes(x = week, y=gw_temp) )+
    geom_point()+
    ggtitle(paste("Year", i+1994, "(station 4)",sep=" "))
  print(ploti)
}


# jahre in denen NAs vorkommen:

gwst1[is.na(gwst1$X2),] 
