#read data
library(tidyverse)
setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten")

#station 1
stat1 <- read.table("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten/LT107284.dat",header = F, sep="", skip = 22)
stat1 <- select(stat1,V1,V3)
names(stat1)[1] <- "date"
names(stat1)[2] <- "temp"
stat1$date <- as.Date(stat1$date, "%d.%m.%Y") # transform to date-format
stat1$temp <- as.numeric(as.character(stat1$temp))
head(stat1)

ggplot()+
        geom_line(data= stat1, aes(x=date, y=temp), color = "red")+
        xlab("date")+
        ylab("temp")
# bei stat1 fehlen daten!

#station 2
stat2 <- as.data.frame(read.table("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten/LT107300.dat",header= F, skip = 20))
stat2 <- select(stat2,V1,V3)
names(stat2)[1] <- "date"
names(stat2)[2] <- "temp"
stat2$date <- as.Date(stat2$date, "%d.%m.%Y") # transform to date-format
stat2$temp <- as.numeric(as.character(stat2$temp))
head(stat2)

ggplot()+
        geom_line(data= stat2, aes(x=date, y=temp), color = "red")+
        xlab("date")+
        ylab("temp")

#station 3
stat3 <- read.table("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten/LT107466.dat",header= F, skip = 20)
stat3 <- select(stat3,V1,V3)
names(stat3)[1] <- "date"
names(stat3)[2] <- "temp"
stat3$date <- as.Date(stat3$date, "%d.%m.%Y") # transform to date-format
stat3$temp <- as.numeric(as.character(stat3$temp))
head(stat3)

ggplot()+
        geom_line(data= stat3, aes(x=date, y=temp), color = "red")+
        xlab("date")+
        ylab("temp")

#station 4
stat4 <- read.table("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten/LT115642.dat",header= F, skip = 24)
stat4 <- select(stat4,V1,V3)
names(stat4)[1] <- "date"
names(stat4)[2] <- "temp"
stat4$date <- as.Date(stat4$date, "%d.%m.%Y") # transform to date-format
stat4$temp <- as.numeric(as.character(stat4$temp))
head(stat4)

ggplot()+
        geom_line(data= stat4, aes(x=date, y=temp), color = "red")+
        xlab("date")+
        ylab("temp")


ggplot()+
        geom_line(data= stat2, aes(x=date, y=temp), color = "red")+
        geom_line(data= stat3, aes(x=date, y=temp), color = "blue")+
        xlab("date")+
        ylab("temp")


        
               