library(tidyverse)
library(lubridate)
library(stringi)
library(timetk)
library(xts)

detach("package:hydroGOF", unload=TRUE)

################################################################################################
###################                Einlesen der AirT-Daten             #########################
################################################################################################


setwd("C:/Users/Russ/Desktop/master/daten/output")
file2 <- "ha2snowglaz01.txt"
#use tt2snowglaz01 to get the air temperature
airtemp <- read_table(file2, col_names = T,
                      cols(TTMMYYY = "c",
                           .default=col_double())) %>% slice(-1)

stri_sub(airtemp$TTMMYYY,-6,0) <- "-"
stri_sub(airtemp$TTMMYYY,-4,0) <- "-"
airtemp$TTMMYYY <- as.Date(airtemp$TTMMYYY, "%d-%m-%Y")
airtemp <- head(airtemp, -1) 
# now the dates are correctly read in

# for plots see _6c_ !!

# aggregate months
AT_m <- airtemp %>% select(., date=TTMMYYY,AirTemp=Temp)%>%
  group_by(year = year(date),
           month = month(date))%>% ### calculate monthly mean temp
  summarise(AirTemp = mean(AirTemp))



################################################################################################
###################                  Einlesen der GWT-Daten            #########################
################################################################################################

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
                        X2 = col_double())) %>% select(.,date=X1,GWTemp=X2))
}

# der loop oben macht das gleiche wie in _6c_ ab Zeile 100


# subset to 1991-2003
gwst1
gwtemp_m <- gwst1[as_date(gwst1$date) < as_date("2004-01-01"), ]
GWT_m <- gwtemp_m[as_date(gwtemp_m$date) > as_date("1990-12-31"), ]
tail(gwtemp_m)
# jahre in denen NAs vorkommen:

gwst1[is.na(gwst1$GWTemp),] 

gwtemp_m[is.na(gwtemp_m$GWTemp),]
plot(gwtemp_m,type="l")



################################################################################################
###################                  Einlesen der WT-Daten             #########################
################################################################################################

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)

wtfile <- "WT-Tagesmitte-Hofstetten(Bad).dat"


# Station Hofstetten
WT <- read_table(wtfile, col_names = F, skip = 31,na = "Lücke", cols(
  X1 = col_date(format = "%d.%m.%Y"), 
  X2 = col_time("%T"),
  X3 = col_double()
)) %>% select(., date=X1, WTemp=X3)

#subset to 1991-2003

WT <- WT[as_date(WT$date) < as_date("2004-01-01"), ]
WT <- WT[as_date(WT$date) > as_date("1990-12-31"), ]

# aggregate months
WT_m <- WT %>%
  group_by(year = year(date),
           month = month(date))%>% ### calculate monthly mean temp
  summarise(WTemp = mean(WTemp))
  

#########################################################################################
#########################################################################################
###################               CORRELATIONS                  #########################
#########################################################################################


(cor(data.frame(WT_m$WTemp,AT_m$AirTemp))) ## very high corr!

(cor(data.frame(WT_m$WTemp,GWT_m$GWTemp))) ## have to remove nas!
