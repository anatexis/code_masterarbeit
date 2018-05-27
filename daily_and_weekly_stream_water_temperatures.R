library(tidyverse)
library(lubridate)

############### daily stream water temperature
setwd("C:/Users/Russ/Desktop/master/daten/Stationsdaten")
file <- "WT-Tagesmitte-Loich.dat"

rff <- read_table(file, col_names = c("date","time","stream_water_temperature"),
                  skip = 68, cols(
                date = col_date(format = "%d.%m.%Y"),
                time = col_time(format = ""),
                stream_water_temperature = col_double()
))

#subsetting as we just want the year 2013 for Loich gauge
swt <- rff[as_date(rff$date)<as_date("2014-01-01"), ]
mean(na.omit(swt$stream_water_temperature))


# plot - line l and point p

plot_swt_l<- ggplot(swt,aes(x =date , y = stream_water_temperature)) + 
        xlab("time [d]") + ylab("Stream Water Temperature [C°]") +
        ggtitle("Station Loich")+
        geom_line(stat="identity") 
plot_swt_l


plot_swt_p<- ggplot(swt,aes(x =date , y = stream_water_temperature)) +
        xlab("time [d]") + ylab("water temperature") +
        ggtitle("Station Loich")+
        geom_point(stat="identity")
plot_swt_p

############ weekly aggregated data

# select output which is interesting for us
swtemp <- swt %>%
  select(., date, stream_water_temperature) 

### calculate weekly temperature
# dplyr and pipe ftw!
weekly_stream_temp <- swtemp %>%
  group_by(week = week(date))%>%
  summarise(
    stream_water_temperature = round(mean(stream_water_temperature),2))


plot_weekly_swt<- ggplot(weekly_stream_temp,aes(x =week , y = stream_water_temperature)) +
  xlab("week") + ylab("weekly stream water temperature") +
  ggtitle("Station Loich")+
  geom_point(stat="identity")+
  geom_smooth( method = "loess", level = .95) # i don't get the confidence intervall (too small)
plot_weekly_swt

### export plot??


### export .txt 

setwd ("C:/Users/Russ/Desktop/master/Daten/output_R/")

# commented out when written

write.table(weekly_stream_temp,file = paste(format(Sys.time(), "%Y-%m-%d"),
                                "_weekly_sreamwater_temperature", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = c("week", "streamwater_temperature"),
            eol = "\r\n", quote = F)
