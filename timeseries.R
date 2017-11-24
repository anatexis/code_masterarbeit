library(tidyverse)
library(lubridate)

################### runoff
setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten")
file <- "Q214312-TM-Loich.dat"

rff <- read_table(file, col_names = F, skip = 26, cols(
                X1 = col_date(format = "%d.%m.%Y"),
                X2 = col_time(format = ""),
                X3 = col_double()
))

#subsetting
rff <- rff[as_date(rff$X1)>as_date("2008-01-01"), ]
mean(na.omit(rff$X3))
# plot

plot_rff<- ggplot(rff,aes(x =X1 , y = X3)) + 
        xlab("time [d]") + ylab("Runoff [m³/d]") +
        ylim(0,60)+
        ggtitle("Station Loich")+
        geom_line(stat="identity") 
plot_rff


################## air temperature
setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten")
file <- "LT107300.dat"

at <- read_table(file, col_names = F, skip = 20, cols(
        X1 = col_date(format = "%d.%m.%Y"),
        X2 = col_time(format = ""),
        X3 = col_double()
))


#subsetting
at <- at[as_date(at$X1)>as_date("2008-01-01"), ]
# plot

plot_at<- ggplot(at,aes(x =X1 , y = X3)) + 
        xlab("time [d]") + ylab("air temperature") +
        expand_limits(y=c(-10,20))+
        ggtitle("Station 107300")+
        geom_point(stat="identity") 
plot_at


################## Water temperature

setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten")
file <- "WT-Tagesmitte-Loich.dat"

wt <- read_table(file, col_names = F, skip = 26, cols(
        X1 = col_date(format = "%d.%m.%Y"),
        X2 = col_time(format = ""),
        X3 = col_double()
))


#subsetting
wt <- wt[as_date(wt$X1)>as_date("2008-01-01"), ]
# plot

plot_wt<- ggplot(wt,aes(x =X1 , y = X3)) + 
        xlab("time [d]") + ylab("water temperature") +
        ggtitle("Station Loich")+
        geom_point(stat="identity") 
plot_wt

