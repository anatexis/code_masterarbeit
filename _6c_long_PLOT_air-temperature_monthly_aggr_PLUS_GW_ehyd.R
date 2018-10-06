library(tidyverse)
library(lubridate)
library(stringi)
library(timetk)
library(xts)

detach("package:hydroGOF", unload=TRUE)
setwd("C:/Users/Russ/Desktop/master/daten/output")
file <- "ha2summary.txt"
file2 <- "ha2snowglaz01.txt"

### to get r to read in files with in the form of
### dmmyyy AND ddmmyyy we have to do smt like this:
discharge <- read_table(file, col_names = T,
                        cols(TTMMYYYY = "c",
                             .default=col_double()))
stri_sub(discharge$TTMMYYYY,-6,0) <- "-"
stri_sub(discharge$TTMMYYYY,-4,0) <- "-"
discharge$TTMMYYYY <- as.Date(discharge$TTMMYYYY, "%d-%m-%Y")
discharge <- head(discharge, -1)
# now the dates are correctly read in


#calculate qsim and select output which is interesting for us
discharge_ts <- discharge %>% mutate(qsim=linout + cascout) %>%
  select(., TTMMYYYY,qsim)

#convert tp timeseries!! Quelle: https://cran.r-project.org/web/packages/timetk/timetk.pdf tk_xts

discharge_ts <- xts(discharge_ts[,-1], order.by = discharge_ts$TTMMYYYY)

discharge_ts

# 2 möglichkeiten:
ts_month <- apply.monthly(discharge_ts,FUN=mean) # a xts object

monthlyTS <- aggregate(discharge_ts, as.yearmon, mean) # a zoo objetct

#plot(monthlyTS) # with plot(), this looks better
#plot(ts_month)   # looks not good with plot()

AT_p <- ggplot()+
  geom_line(data=ts_month, aes(x = Index, y=qsim))
AT_p
autoplot(as.zoo(ts_month), geom = "line")


####################################################################
################ AIR TEMPERATURE ###################################

#use tt2snowglaz01 to get the air temperature
airtemp <- read_table(file2, col_names = T,
                      cols(TTMMYYY = "c",
                           .default=col_double())) %>% slice(-1)

stri_sub(airtemp$TTMMYYY,-6,0) <- "-"
stri_sub(airtemp$TTMMYYY,-4,0) <- "-"
airtemp$TTMMYYY <- as.Date(airtemp$TTMMYYY, "%d-%m-%Y")
airtemp <- head(airtemp, -1) 
# now the dates are correctly read in


# select output which is interesting for us
airtemp_ts <- airtemp %>% select(., TTMMYYY,Temp)

airtemp_ts <- xts(airtemp_ts[,-1], order.by = airtemp_ts$TTMMYYY)

airtemp_ts

# create time series
ts_month_t <- apply.monthly(airtemp_ts,FUN=mean)


# autoplot or ggplot2 (here: ggplot2)
  
air_t_p <- ggplot()+
  geom_line(data=ts_month_t, aes(x = Index, y=Temp))+ #quelle: https://stackoverflow.com/a/43345938
  scale_x_date(date_labels="%y",date_breaks  ="1 year") #https://stackoverflow.com/a/41856325

air_t_p

####################################################################
################ GROUND WATER TEMPERATURE ##########################

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

gwst1 <- read_csv2(file1, col_names = F, skip = 34, na = "Lücke",cols(
  X1 = col_date(format = "%d.%m.%Y %T"), 
  X2 = col_double()
)) %>% select(.,X1,X2)
# because of col_date it takes just the date not the time (specified with %T)

gwst2 <- read_csv2(file2, col_names = F, skip = 34, na = "Lücke", cols(
  X1 = col_date(format = "%d.%m.%Y %T"), 
  X2 = col_double()
)) %>% select(.,X1,X2)

gwst3 <- read_csv2(file3, col_names = F, skip = 34, na = "Lücke", cols(
  X1 = col_date(format = "%d.%m.%Y %T"), 
  X2 = col_double()
)) %>% select(.,X1,X2)

gwst4 <- read_csv2(file4, col_names = F, skip = 34, na = "Lücke", cols(
  X1 = col_date(format = "%d.%m.%Y %T"), 
  X2 = col_double()
)) %>% select(.,X1,X2)

gwst5 <- read_csv2(file5, col_names = F, skip = 34, na = "Lücke", cols(
  X1 = col_date(format = "%d.%m.%Y %T"), 
  X2 = col_double()
)) %>% select(.,X1,X2)

gwst6 <- read_csv2(file6, col_names = F, skip = 34, na = "Lücke", cols(
  X1 = col_date(format = "%d.%m.%Y %T"), 
  X2 = col_double()
)) %>% select(.,X1,X2)



# tailor the gw temperature time series' to our calibration time span (which is 1991-2003)

gwst1_1991_2003 <- gwst1[as_date(gwst1$X1) < as_date("2004-01-01"), ]
gwst1_1991_2003 <- gwst1_1991_2003[as_date(gwst1_1991_2003$X1) > as_date("1990-12-31"), ]
# gwst2 <- gwst2[as_date(gwst2$X1) < as_date("2004-01-01"), ] starts after 2003
# gwst3 <- gwst3[as_date(gwst3$X1) < as_date("2004-01-01"), ] starts after 2003
gwst4_1991_2003 <- gwst4[as_date(gwst4$X1) < as_date("2004-01-01"), ]
gwst4_1991_2003 <- gwst4_1991_2003[as_date(gwst4_1991_2003$X1) > as_date("1990-12-31"), ] # starts 1994-06-01
# gwst5 <- gwst5[as_date(gwst5$X1) < as_date("2004-01-01"), ] starts after 2003
# gwst6 <- gwst6[as_date(gwst6$X1) < as_date("2004-01-01"), ] starts after 2003

gwst1
air_t_p +
  geom_line(data = gwst1_1991_2003, aes(x=X1, y=X2), color="red")

air_t_p +
  geom_line(data = gwst4_1991_2003, aes(x=X1, y=X2), color="red")

ggplot()+
  geom_line(data = gwst1, aes(x=X1, y=X2, color="gwst1")) +
  geom_smooth(data = gwst1, aes(x=X1, y=X2, color="gwst1"), method = "lm")+
  # geom_line(data = gwst2, aes(x=X1, y=X2), color="green") +
  # geom_line(data = gwst3, aes(x=X1, y=X2), color="blue") +
  geom_line(data = gwst4, aes(x=X1, y=X2, color="gwst4")) +
  geom_smooth(data = gwst4, aes(x=X1, y=X2, color="gwst4"), method = "lm") +
  # geom_line(data = gwst5, aes(x=X1, y=X2), color="yellow") +
  # geom_line(data = gwst6, aes(x=X1, y=X2), color="grey") +
  scale_x_date(date_labels="%y",date_breaks  ="1 year")

## von 1991 bis 2003 (nur gwst1 und gwst4)

ggplot()+
  geom_line(data = gwst1_1991_2003, aes(x=X1, y=X2, color="gwst1")) +
  geom_smooth(data = gwst1_1991_2003, aes(x=X1, y=X2, color="gwst1"),method="lm") +
  geom_line(data = gwst4_1991_2003, aes(x=X1, y=X2, color="gwst4")) +
  geom_smooth(data = gwst4_1991_2003, aes(x=X1, y=X2, color="gwst4"),method="lm") +
  scale_colour_discrete("Groundwater Station") +
  scale_x_date(date_labels="%m/%y",date_breaks  ="6 months")

## ab 2011 plotten (außer gwst1 + gwst4 fangen alle stationen frühestens 2011 an)

gwst1_2011 <- gwst1[265:length(gwst1$X2),]
gwst4_2011 <- gwst4[200:length(gwst4$X2),]

ggplot()+
  geom_line(data = gwst1_2011, aes(x=X1, y=X2, color="gwst1")) +
  geom_smooth(data = gwst1_2011, aes(x=X1, y=X2, color="gwst1"),method="lm") +
  # geom_line(data = gwst2, aes(x=X1, y=X2, color="gwst2")) +
  # geom_smooth(data = gwst2, aes(x=X1, y=X2, color="gwst2"),method="lm") +
  # geom_line(data = gwst3, aes(x=X1, y=X2, color="gwst3")) +
  # geom_smooth(data = gwst3, aes(x=X1, y=X2, color="gwst3"),method="lm") +
  # geom_line(data = gwst4_2011, aes(x=X1, y=X2, color="gwst4")) +
  # geom_smooth(data = gwst4_2011, aes(x=X1, y=X2, color="gwst4"),method="lm") +
  # geom_line(data = gwst5, aes(x=X1, y=X2, color="gwst5")) +
  # geom_smooth(data = gwst5, aes(x=X1, y=X2, color="gwst5"),method="lm") +
  # geom_line(data = gwst6, aes(x=X1, y=X2, color="gwst6")) +
  # geom_smooth(data = gwst6, aes(x=X1, y=X2, color="gwst6"),method="lm") +
  scale_colour_discrete("Groundwater Station") +
  scale_x_date(date_labels="%m/%y",date_breaks  ="3 months")

# ab 2004 
gwst1_2004 <- gwst1[181:length(gwst1$X2),]
gwst4_2004 <- gwst4[116:length(gwst4$X2),]

ggplot()+
  geom_line(data = gwst1_2004, aes(x=X1, y=X2, color="gwst1")) +
  geom_smooth(data = gwst1_2004, aes(x=X1, y=X2, color="gwst1"),method="lm") +
  geom_line(data = gwst2, aes(x=X1, y=X2, color="gwst2")) +
  geom_smooth(data = gwst2, aes(x=X1, y=X2, color="gwst2"),method="lm") +
  geom_line(data = gwst3, aes(x=X1, y=X2, color="gwst3")) +
  geom_smooth(data = gwst3, aes(x=X1, y=X2, color="gwst3"),method="lm") +
  geom_line(data = gwst4_2004, aes(x=X1, y=X2, color="gwst4")) +
  geom_smooth(data = gwst4_2004, aes(x=X1, y=X2, color="gwst4"),method="lm") +
  geom_line(data = gwst5, aes(x=X1, y=X2, color="gwst5")) +
  geom_smooth(data = gwst5, aes(x=X1, y=X2, color="gwst5"),method="lm") +
  geom_line(data = gwst6, aes(x=X1, y=X2, color="gwst6")) +
  geom_smooth(data = gwst6, aes(x=X1, y=X2, color="gwst6"),method="lm") +
  scale_colour_discrete("Groundwater Station") +
  scale_x_date(date_labels="%m/%y",date_breaks  ="3 months")


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


WT_ts <- xts(WT[,-1], order.by = WT$date)

WT_ts

# create time series
WT_ts_m <- apply.monthly(WT_ts,FUN=mean)

#plot (here: ggplot2)

WT_p <- ggplot()+
  geom_line(data=WT_ts_m, aes(x = Index, y=WTemp, color = "Streamwatertemp"))+ #quelle: https://stackoverflow.com/a/43345938
  scale_x_date(date_labels="%y",date_breaks  ="1 year") #https://stackoverflow.com/a/41856325

WT_p


############ ALLL TOOOGEEETHER NOW

WT_p+
  geom_line(data = gwst1_1991_2003, aes(x=X1, y=X2, color="Groundwatertemp")) +
  geom_line(data=ts_month_t, aes(x = Index, y=Temp, color="Airtemp"))

