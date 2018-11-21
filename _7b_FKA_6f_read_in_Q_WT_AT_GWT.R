library(tidyverse)
library(lubridate)
library(stringi)
library(timetk)
library(xts)

#detach("package:hydroGOF", unload=TRUE)

################################################################################################
###################                Einlesen der Q-Daten             #########################
################################################################################################

setwd("C:/Users/Russ/Desktop/master/daten/output")

############ CALIBRATION PERIOD ###########################

qfile_c <- "ha2summary.txt"

### dmmyyy AND ddmmyyy we have to do smt like this:
discharge <- read_table(qfile_c, col_names = T,
                        cols(TTMMYYYY = "c",
                             .default=col_double()))
stri_sub(discharge$TTMMYYYY,-6,0) <- "-"
stri_sub(discharge$TTMMYYYY,-4,0) <- "-"
discharge$TTMMYYYY <- as.Date(discharge$TTMMYYYY, "%d-%m-%Y")
discharge_c <- head(discharge, -1)

# now the dates are correctly read in
#calculate qsim and select output which is interesting for us
discharge_c <- discharge_c %>% mutate(qsim=linout + cascout) %>%
  select(., TTMMYYYY,qsim,slow=linout,fast=cascout)

# for plots see _6c_ !!

# aggregate months
Q_m_c <- discharge_c %>% select(., date=TTMMYYYY, qsim, slow, fast)%>%
  group_by(year = year(date),
           month = month(date))%>% ### calculate monthly mean temp
  summarise(qsim = mean(qsim),
            slow = mean(slow),
            fast = mean(fast))


############ VALIDATION PERIOD ################################

qfile_v <- "ha3summary.txt"

### dmmyyy AND ddmmyyy we have to do smt like this:
discharge <- read_table(qfile_v, col_names = T,
                        cols(TTMMYYYY = "c",
                             .default=col_double()))
stri_sub(discharge$TTMMYYYY,-6,0) <- "-"
stri_sub(discharge$TTMMYYYY,-4,0) <- "-"
discharge$TTMMYYYY <- as.Date(discharge$TTMMYYYY, "%d-%m-%Y")
discharge_v <- head(discharge, -1)

# now the dates are correctly read in
#calculate qsim and select output which is interesting for us
discharge_v <- discharge_v %>% mutate(qsim=linout + cascout) %>%
  select(., TTMMYYYY,qsim,slow=linout,fast=cascout)

# for plots see _6c_ !!

# aggregate months
Q_m_v <- discharge_v %>% select(., date=TTMMYYYY, qsim, slow, fast)%>%
  group_by(year = year(date),
           month = month(date))%>% ### calculate monthly mean temp
  summarise(qsim = mean(qsim),
            slow = mean(slow),
            fast = mean(fast))


################### merge the two datasets ##########
#source:https://www.statmethods.net/management/merging.html


Q_m <- rbind(Q_m_c, Q_m_v) # now we have the discharge data from 1991 to 2014

################################################################################################
###################                Einlesen der AirT-Daten             #########################
################################################################################################

setwd("C:/Users/Russ/Desktop/master/daten/output")

############ CALIBRATION PERIOD ###########################

file_c <- "ha2snowglaz01.txt"
#use ha2snowglaz01 to get the air temperature f calibration period
airtemp_c <- read_table(file_c, col_names = T,
                        cols(TTMMYYY = "c",
                             .default=col_double())) %>% slice(-1)

stri_sub(airtemp_c$TTMMYYY,-6,0) <- "-"
stri_sub(airtemp_c$TTMMYYY,-4,0) <- "-"
airtemp_c$TTMMYYY <- as.Date(airtemp_c$TTMMYYY, "%d-%m-%Y")
airtemp_c <- head(airtemp_c, -1) 
# now the dates are correctly read in

# for plots see _6c_ !!

# aggregate months
AT_m_c <- airtemp_c %>% select(., date=TTMMYYY,AirTemp=Temp)%>%
  group_by(year = year(date),
           month = month(date))%>% ### calculate monthly mean temp
  summarise(AirTemp = mean(AirTemp))


############ VALIDATION PERIOD ################################

file_v <- "ha3snowglaz01.txt"
#use ha3snowglaz01 to get the air temperature f validation period
airtemp_v <- read_table(file_v, col_names = T,
                        cols(TTMMYYY = "c",
                             .default=col_double())) %>% slice(-1)

stri_sub(airtemp_v$TTMMYYY,-6,0) <- "-"
stri_sub(airtemp_v$TTMMYYY,-4,0) <- "-"
airtemp_v$TTMMYYY <- as.Date(airtemp_v$TTMMYYY, "%d-%m-%Y")
airtemp_v <- head(airtemp_v, -1) 
# now the dates are correctly read in

# for plots see _6c_ !!

# aggregate months
AT_m_v <- airtemp_v %>% select(., date=TTMMYYY,AirTemp=Temp)%>%
  group_by(year = year(date),
           month = month(date))%>% ### calculate monthly mean temp
  summarise(AirTemp = mean(AirTemp))

################### merge the two datasets ##########
#source:https://www.statmethods.net/management/merging.html

AT_m <- rbind(AT_m_c, AT_m_v) # now we have the air temperature data from 1991 to 2014

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

#subset to 1991-2014

WT <- WT[as_date(WT$date) < as_date("2015-01-01"), ]
WT <- WT[as_date(WT$date) > as_date("1990-12-31"), ]

# aggregate months
WT_m <- WT %>%
  group_by(year = year(date),
           month = month(date))%>% ### calculate monthly mean temp
  summarise(WTemp = mean(WTemp))

#fehlende Daten

WT_m_NA <- WT_m[is.na(WT_m$WTemp),] # von 2014-6 bis 2016-10 (29 Datenpkt) fehlen und 2008-6
#View(WT_m_NA)  

plot(WT_m$WTemp, type="l")

################################################################################################
###################                  Einlesen der GWT-Daten            #########################
################################################################################################

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)

file1 <- "Grundwassertemperatur-Monatsmittel-322917.csv" #1.näheste Station


gwst1 <- read_csv2(file1, col_names = F, skip = 34, na = "Lücke",
                        cols(
                          X1 = col_date(format = "%d.%m.%Y %T"), 
                          X2 = col_double())) %>% select(.,date=X1,GWTemp=X2)



# subset to 1991-2014
gwst1
gwtemp_m <- gwst1[as_date(gwst1$date) < as_date("2015-01-01"), ]
GWT_m <- gwtemp_m[as_date(gwtemp_m$date) > as_date("1990-12-31"), ]
tail(gwtemp_m)
# jahre in denen NAs vorkommen:

GWT_m[is.na(GWT_m$GWTemp),]  # 2016 nicht mehr in subsetting bereich

gwtemp_m[is.na(gwtemp_m$GWTemp),]
plot(gwtemp_m,type="l")

#######################
GWT_m # from 1991 to 2014 without lag


############################# lags for 7c our model    #######################################
#function ein bisschen anders, gibt meherere aus wenn sie ähnlich groß sind

Find_Max_CCF <- function(a, b, e=0)
{
  d <- ccf(a, b, plot = FALSE, lag.max = length(a)/2, na.action = na.pass)
  cor = d$acf[,,1]
  abscor = abs(d$acf[,,1])
  lag = d$lag[,,1]
  res = data.frame(cor, lag)
  absres = data.frame(abscor, lag)
  maxcor = max(absres$abscor)
  absres_max = res[which(absres$abscor >= maxcor-maxcor*e &
                           absres$abscor <= maxcor+maxcor*e),]
  return(absres_max)
}

# Find_Max_CCF(GWT_m$GWTemp[1:161],WT_m$WTemp[1:161], 0.02) #(calibration period)
# cor       lag
# 0.8532170   3*
# 0.8502565   4
## lag for the validaton period (which is from month 191:288)

Find_Max_CCF(GWT_m$GWTemp[191:288],WT_m$WTemp[191:288], 0.02)
# cor       lag
# 0.8820232   3
# 0.8931690   4*

## lag for whole period

Find_Max_CCF(GWT_m$GWTemp,WT_m$WTemp,0.02)
# cor         lag
# 0.8522380   3
# 0.8564982   4*

###preparatioin for lag in _7c_
# for the we have to include 4 more months because we will lose three bc of lag 4)


path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)

file1 <- "Grundwassertemperatur-Monatsmittel-322917.csv"

gwst1 <- read_csv2(file1, col_names = F, skip = 34, na = "Lücke",cols(
  X1 = col_date(format = "%d.%m.%Y %T"),
  X2 = col_double()
)) %>% select(.,date=X1,GWTemp=X2)

#subset to 1991-2014 + 3 months
gwst1
gwtemp_m <- gwst1[as_date(gwst1$date) < as_date("2015-05-01"), ]
GWT_m2 <- gwtemp_m[as_date(gwtemp_m$date) > as_date("1990-12-31"), ]

GWT_m_lag <- GWT_m2
GWT_m_lag$GWTemp <- lead(GWT_m2$GWTemp, 4) #lead 3
GWT_m_lag <- GWT_m_lag[1:288,] # to remove the last three NAs

# end of preparation ########################




