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

ccWT_AT <- ccf(WT,AT,lag.max = 10)
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

Find_Abs_Max_CCF(WT,GWT) # so with a lag of -3 (months) there is a correlation of 0.85!


### multiple regression

input <- data.frame(WT,AT,GWT)

# Create the relationship model.
model <- lm(WT~AT+GWT, data = input)

# Show the model
print(model)
summary(model)
#plot(model)

# getting the intercept and the coeff

a <- coef(model)[1] #interc
X1 <- coef(model)[2] #AT
X2 <- coef(model)[3] #GWT

#model: WT_m <- a+AT*X1+GWT*X2

WT_m <- a+AT*X1+GWT*X2
WT_m
WT
plot(WT_m, type="l")
plot(WT, type="l")

cor(data.frame(WT_m,WT),use = "pairwise.complete.obs")
plot(data.frame(WT_m,WT))
