#read data
library(tidyverse)
library(lubridate)
library(reshape)

#read path on windows or linux

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)



# Lufttemperatur
LTfile1 <- "LT107300.dat"
LTfile2 <- "LT115642.dat"

#in linux:"Lücke" in win: "L?cke" (is this a problem?)
#if yes, maybe this helps?: https://stackoverflow.com/a/28461726
# Station Frankenfels
LT1 <- read_table(LTfile1, col_names = F, skip = 20,na = "Lücke", 
                  cols(
        X1 = col_date(format = "%d.%m.%Y"), 
        X2 = col_time("%T"),
        X3 = col_double()
))
LT1 <- select(LT1,date = X1, Temp = X3)

# Station St. Poelten
LT2 <- read_table(LTfile2, col_names = F, skip = 24,na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y"), 
        X2 = col_time("%T"),
        X3 = col_double()
))
LT2 <- select(LT2,date = X1, Temp = X3)

# Wassertemperatur
WTfile1 <- "WT-Tagesmitte-Loich.dat"
WTfile2 <- "WT-Tagesmitte-Hofstetten(Bad).dat"


# Station Loich
WT1 <- read_table(WTfile1, col_names = F, skip = 26,na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y"), 
        X2 = col_time("%T"),
        X3 = col_double()
))
WT1 <- select(WT1,date = X1, Temp = X3)

# Station Hofstetten
WT2 <- read_table(WTfile2, col_names = F, skip = 31,na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y"), 
        X2 = col_time("%T"),
        X3 = col_double()
))
WT2 <- select(WT2,date = X1, Temp = X3)

############# Loich

LT1 <- LT1[as_date(LT1$date) > as_date("2012-11-19"), ]
LT1 <- LT1[as_date(LT1$date) < as_date("2017-02-28"), ]
WT1 <- WT1[as_date(WT1$date) < as_date("2017-02-28"), ] #last entry "Lücke"

LW1 <- add_column(LT1,WT1$Temp)

plot(LW1[2:3])

abline(lm(LW1$`WT1$Temp`~LW1$Temp), col="red")

summary(lm(LW1$`WT1$Temp`~LW1$Temp))

############# Hofstetten

LT2 <- LT2[as_date(LT2$date) < as_date("2017-02-28"), ]
WT2 <- WT2[as_date(WT2$date) > as_date("1990-12-31"), ]
WT2 <- WT2[as_date(WT2$date) < as_date("2017-02-28"), ] #last entry "Lücke"

LW2 <- add_column(LT2,WT2$Temp)

plot(LW2[2:3])

abline(lm(LW2$`WT2$Temp`~LW2$Temp), col="red")

summary(lm(LW2$`WT2$Temp`~LW2$Temp))

plot(LW1$`WT1$Temp`)
points(LW2[7988:9555,3], col="red")
