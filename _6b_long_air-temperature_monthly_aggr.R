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

# do the same with tt2snowglaz01 to get the air temperature
airtemp <- read_table(file2, col_names = T,
                        cols(TTMMYYY = "c",
                             .default=col_double())) %>% slice(-1)
# (slice(-1) is used to get rid of the line with the measurement units)

stri_sub(airtemp$TTMMYYY,-6,0) <- "-"
stri_sub(airtemp$TTMMYYY,-4,0) <- "-"
airtemp$TTMMYYY <- as.Date(airtemp$TTMMYYY, "%d-%m-%Y")
airtemp <- head(airtemp, -1) 
# now the dates are correctly read in


#calculate qsim and select output which is interesting for us
discharge_ts <- discharge %>% mutate(qsim=linout + cascout) %>%
  select(., TTMMYYYY,Qobs)

#convert tp timeseries!! Quelle: https://cran.r-project.org/web/packages/timetk/timetk.pdf tk_xts

discharge_ts <- xts::xts(discharge_ts[,-1], order.by = discharge_ts$TTMMYYYY)

discharge_ts

# 2 möglichkeiten:
ts.month <- apply.monthly(discharge_ts,FUN=mean)

monthlyTS <- aggregate(discharge_ts, as.yearmon, mean)

plot(monthlyTS)
plot(ts.month)

ggplot(data=ts.month, aes(y=Qobs))
autoplot(as.zoo(ts.month), geom = "line")

### the same procedure with the temp
# select output which is interesting for us
airtemp_ts <- airtemp %>% select(., TTMMYYY,Temp)

airtemp_ts <- xts::xts(airtemp_ts[,-1], order.by = airtemp_ts$TTMMYYY)

airtemp_ts

# 2 möglichkeiten:
ts.month_t <- apply.monthly(airtemp_ts,FUN=mean)

monthlyTS_t <- aggregate(airtemp_ts, as.yearmon, mean)

plot(monthlyTS_t)
plot(ts.month_t)

autoplot(as.zoo(ts.month_t), geom = "line") #quelle:https://stackoverflow.com/a/26328426


### TO DO: NOCH JEWEILS DAS PLUS GW TEMP NEBENEINANDER PLOTTEN!!


setwd ("C:/Users/Russ/Desktop/master/Daten/output_R/")

# # commented out when written

# write.table(weekly_atemp,file = paste(format(Sys.time(), "%Y-%m-%d"),
#                                 "_weekly_air-temperature", ".txt", sep = "") ,sep=",",
#             row.names=FALSE,col.names = c("week", "air-temperature"),
#             eol = "\r\n", quote = F)

# 
# write.table(perc_weekly_dis,file = paste(format(Sys.time(), "%Y-%m-%d"),
#                                 "_percentage_of_fast_and_GW_weekly_discharge", ".txt", sep = "") ,sep=",",
#             row.names=FALSE,col.names = c("week", "percentage_Fast", "percentage_GW", "qsim"),
#             eol = "\r\n", quote = F)

#sources:

#copy files: https://stackoverflow.com/questions/2384517/using-r-to-copy-files#2384621
#rename files: https://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r
#scale_color_manual:https://stackoverflow.com/a/40181166
#weekly data: https://stackoverflow.com/questions/40554231/dplyr-lubridate-how-to-aggregate-a-dataframe-by-week#comment68346741_40554231
#slice(-1) https://stackoverflow.com/a/40261958