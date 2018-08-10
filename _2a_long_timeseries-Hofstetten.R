library(tidyverse)
library(lubridate)

################### runoff
path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)

file <- "Q207852-TM-Hofstetten(Bad).dat"

rff <- read_table(file, col_names = F, skip = 27, cols(
                X1 = col_date(format = "%d.%m.%Y"),
                X2 = col_time(format = ""),
                X3 = col_double()
))

# #subsetting
# rff <- rff[as_date(rff$X1)>as_date("2008-01-01"), ]
# mean(na.omit(rff$X3))
# plot

plot_rff<- ggplot(rff,aes(x =X1 , y = X3)) + 
        xlab("time [d]") + ylab("Runoff [m³/d]") +
        ylim(0,60)+
        ggtitle("Station Hofstetten")+
        geom_line(stat="identity") 
plot_rff


################## air temperature
path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)
file <- "LT107300.dat"

at <- read_table(file, col_names = F, skip = 20, cols(
        X1 = col_date(format = "%d.%m.%Y"),
        X2 = col_time(format = ""),
        X3 = col_double()
))

# 
# #subsetting
# at <- at[as_date(at$X1)>as_date("2008-01-01"), ]
# plot

plot_at<- ggplot(at,aes(x =X1 , y = X3)) + 
        xlab("time [d]") + ylab("air temperature") +
        expand_limits(y=c(-10,20))+
        ggtitle("Station 107300")+
        geom_point(stat="identity") 
plot_at


################## Water temperature

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)
file <- "WT-Tagesmitte-Hofstetten(Bad).dat"

wt <- read_table(file, col_names = F, skip = 31, cols(
        X1 = col_date(format = "%d.%m.%Y"),
        X2 = col_time(format = ""),
        X3 = col_double()
))

 
# #subsetting
# wt <- wt[as_date(wt$X1)>as_date("2008-01-01"), ]
# plot

plot_wt<- ggplot(wt,aes(x =X1 , y = X3)) + 
        xlab("time [d]") + ylab("water temperature") +
        ggtitle("Station Hofstetten")+
        geom_point(stat="identity") 
plot_wt # 864 missing observations


####################################################
# 2 years of data
path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)
file <- "LT107300.dat"

at <- read_table(file, col_names = F, skip = 20, cols(
        X1 = col_date(format = "%d.%m.%Y"),
        X2 = col_time(format = ""),
        X3 = col_double()
))


#subsetting
at2 <- at[as_date(at$X1)<as_date("2015-01-01"), ]
at2 <- at2[as_date(at2$X1)>as_date("2013-01-01"), ]
# plot

plot_at<- ggplot(at2,aes(x =X1 , y = X3)) + 
        xlab("time") + ylab("air temperature") +
        expand_limits(y=c(-10,30))+
        ggtitle("air temperature curve")+
        geom_point(stat="identity") 
plot_at

####

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)
file <- "WT-Tagesmitte-Hofstetten(Bad).dat"

wt <- read_table(file, col_names = F, skip = 31, cols(
        X1 = col_date(format = "%d.%m.%Y"),
        X2 = col_time(format = ""),
        X3 = col_double()
))


#subsetting
wt <- wt[as_date(wt$X1)<as_date("2015-01-01"), ]
wt <- wt[as_date(wt$X1)>as_date("2013-01-01"), ]
# plot

plot_wt<- ggplot(wt,aes(x =X1 , y = X3)) + 
        xlab("time") + ylab("water temperature") +
        expand_limits(y=c(-10,30))+
        ggtitle("water temperature curve")+
        geom_point(stat="identity") 
plot_wt

