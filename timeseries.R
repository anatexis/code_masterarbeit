library(tidyverse)
library(lubridate)

################### runoff
setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten")
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)
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
        xlab("Time [d]") + ylab("Runoff [m³/s]") +
        ylim(0,60)+
        ggtitle("Runof (Gauge Loich)")+
        geom_line(stat="identity") 
plot_rff


################## air temperature
setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten")
file <- "LT107300.dat"

loich_at <- read_table(file, col_names = F, skip = 20, cols(
        X1 = col_date(format = "%d.%m.%Y"),
        X2 = col_time(format = ""),
        X3 = col_double()
))


#subsetting
loich_at <- loich_at[as_date(loich_at$X1)>as_date("2008-01-01"), ]
# plot

plot_at<- ggplot(loich_at,aes(x =X1 , y = X3)) + 
        xlab("time [d]") + ylab("air temperature") +
        expand_limits(y=c(-10,20))+
        ggtitle("Station 107300")+
        geom_point(stat="identity") 
plot_at


################## Water temperature

setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten")
file <- "WT-Tagesmitte-Loich.dat"

loich_wt <- read_table(file, col_names = F, skip = 26, cols(
        X1 = col_date(format = "%d.%m.%Y"),
        X2 = col_time(format = ""),
        X3 = col_double()
))


#subsetting
loich_wt2 <- loich_wt[as_date(loich_wt$X1)<as_date("2015-01-01"), ]
loich_wt2 <- loich_wt2[as_date(loich_wt2$X1)>as_date("2013-01-01"), ]
# plot

plot_wt<- ggplot(loich_wt,aes(x =X1 , y = X3)) + 
        xlab("time [d]") + ylab("water temperature") +
        ggtitle("Station Loich")+
        geom_point(stat="identity") 
plot_wt


#################################################### 2 jhre

setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten")
file <- "LT107300.dat"

loich_at <- read_table(file, col_names = F, skip = 20, cols(
        X1 = col_date(format = "%d.%m.%Y"),
        X2 = col_time(format = ""),
        X3 = col_double()
))


#subsetting
loich_at2 <- loich_at[as_date(loich_at$X1)<as_date("2015-01-01"), ]
loich_at2 <- loich_at2[as_date(loich_at2$X1)>as_date("2013-01-01"), ]
# plot

loich_plot_at<- ggplot(loich_at2,aes(x =X1 , y = X3)) + 
        xlab("time") + ylab("air temperature") +
        expand_limits(y=c(-10,30))+
        ggtitle("air temperature curve")+
        geom_line(stat="identity") 
loich_plot_at

####

setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten")
file <- "WT-Tagesmitte-Loich.dat"

loich_wt <- read_table(file, col_names = F, skip = 26, cols(
        X1 = col_date(format = "%d.%m.%Y"),
        X2 = col_time(format = ""),
        X3 = col_double()
))


#subsetting
#subsetting
loich_wt2 <- loich_wt[as_date(loich_wt$X1)<as_date("2015-01-01"), ]
loich_wt2 <- loich_wt2[as_date(loich_wt2$X1)>as_date("2013-01-01"), ]
# plot

loich_plot_wt<- ggplot(loich_wt2,aes(x =X1 , y = X3)) + 
        xlab("time") + ylab("water temperature") +
        expand_limits(y=c(-10,30))+
        ggtitle("water temperature curve")+
        geom_point(stat="identity") 
loich_plot_wt


#### at und wt gemeinsam in einem plot

#### gemeinsam in einem plot
loich_plot_at + geom_line(data = loich_wt2, aes(x=X1, y=X3), stat="identity", colour="red")+
  xlab("Time [d]") + ylab("Temperature [°C]") +
  expand_limits(y=c(-10,30))+
  ggtitle("Air and Water Temperature (Gauge Loich)")

