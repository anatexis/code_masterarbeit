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


# subset to 1991-2014
gwst1
gwtemp_m <- gwst1[as_date(gwst1$date) < as_date("2015-01-01"), ]
GWT_m <- gwtemp_m[as_date(gwtemp_m$date) > as_date("1990-12-31"), ]
tail(gwtemp_m)
# jahre in denen NAs vorkommen:

gwst1[is.na(gwst1$GWTemp),]  # 2016 nicht mehr in subsetting bereich

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
View(WT_m_NA)  #

plot(WT_m$WTemp, type="l")

#########################################################################################
#########################################################################################
###################               CORRELATIONS                  #########################
#########################################################################################


## correlation matrix WT / AT / GWT

(cor(data.frame(WT_m$WTemp,AT_m$AirTemp,GWT_m$GWTemp), use = "pairwise.complete.obs"))
# pairwise.complete.obs just removes na when comparing pairs not whole row! (corr of WT&AT uneffected bc there are
# no NAs) Source: https://stackoverflow.com/a/18892108
## correlation between WT & AT is very high
## correlation between WT & GWT and AT & GWT is negative (shift!)


## cross correlation
WT <- WT_m$WTemp
AT <- AT_m$AirTemp
GWT <- GWT_m$GWTemp

ccWT_AT <- ccf(WT,AT,lag.max = 10, na.action=na.pass)
ccWT_AT

ccWT_GWT <- ccf(WT,GWT,lag.max = 10, na.action = na.pass)
ccWT_GWT

Find_Abs_Max_CCF<- function(a,b) # source https://stackoverflow.com/a/20133091
{
  d <- ccf(a, b, plot = FALSE, lag.max = length(a)-5, na.action = na.pass)
  cor = d$acf[,,1]
  abscor = abs(d$acf[,,1])
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  absres = data.frame(abscor,lag)
  absres_max = res[which.max(absres$abscor),]
  return(absres_max)
}

Find_Abs_Max_CCF(WT,GWT) # so with a lag of -4 (months) there is a correlation of 0.85! (with calibration period its a lag of -3)


### multiple regression 

input <- data.frame(WT,AT,GWT)
input1 <- input[1:161,]
input2 <- input[191:288,]


# Create the relationship model.
model <- lm(WT~AT+GWT, data = input1)

# Show the model
print(model)
summary(model)
#plot(model)

# getting the intercept and the coeff

a <- coef(model)[1] #interc
X1 <- coef(model)[2] #AT
X2 <- coef(model)[3] #GWT

#model: WT_m <- a+AT*X1+GWT*X2

WT_model <- a+input2$AT*X1+input2$GWT*X2
WT_model
WT
plot(WT_model, type="l")
lines(input2$WT, col="red")

cor(data.frame(WT_model,input2$WT),use = "pairwise.complete.obs")
plot(data.frame(WT_model,input2$WT))
