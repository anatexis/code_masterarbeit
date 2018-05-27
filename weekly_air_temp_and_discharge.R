library(tidyverse)
library(lubridate)
library(stringi)
detach("package:hydroGOF", unload=TRUE)
setwd("C:/Users/Russ/Desktop/master/daten/output")
file <- "tt2summary.txt"
file2 <- "tt2snowglaz01.txt"

### to get r to read in files with in the form of
### dmmyyy AND ddmmyyy we have to do smt like this:
discharge <- read_table(file, col_names = T,
                        cols(TTMMYYYY = "c",
                             .default=col_double()))
stri_sub(discharge$TTMMYYYY,-6,0) <- "-"
stri_sub(discharge$TTMMYYYY,-4,0) <- "-"
discharge$TTMMYYYY <- as.Date(discharge$TTMMYYYY, "%d-%m-%Y")
# now the dates are correctly read in

# do the same with tt2snowglaz01 to get the air temperature
airtemp <- read_table(file2, col_names = T,
                        cols(TTMMYYY = "c",
                             .default=col_double())) %>% slice(-1)
# (slice(-1) is used to get rid of the line with the measurement units)

stri_sub(airtemp$TTMMYYY,-6,0) <- "-"
stri_sub(airtemp$TTMMYYY,-4,0) <- "-"
airtemp$TTMMYYY <- as.Date(airtemp$TTMMYYY, "%d-%m-%Y")
# now the dates are correctly read in


#calculate qsim and select output which is interesting for us
discharge <- discharge %>% mutate(qsim=linout + cascout) %>% 
  select(., TTMMYYYY,Qobs,qsim,linout,cascout)

### calculate weekly discharge
# dplyr and pipe ftw!
weekly_dis <- discharge %>%
  group_by(week = week(TTMMYYYY))%>%
  summarise(
      Qobs = mean(Qobs),
      qsim = mean(qsim),
      linout = mean(linout),
      cascout = mean(cascout))

#calculate percentage of linout and cascout
perc_weekly_dis <- weekly_dis %>% mutate(perc_GW = round(linout / qsim,3),
                                         perc_Fast = round(cascout / qsim,3)) %>% 
  mutate(.,weeks=seq(1:53)) %>%
  select(.,week, qsim, perc_GW, perc_Fast) 


### the same procedure with the temp
# select output which is interesting for us
airtemp <- airtemp %>% select(., TTMMYYY,Temp)

### calculate weekly temperature
# dplyr and pipe ftw!
weekly_temp <- airtemp %>%
  group_by(week = week(TTMMYYY))%>%
  summarise(
    Temp = mean(Temp))
  

setwd ("C:/Users/Russ/Desktop/master/Daten/output_R/")

# # commented out when written

# write.table(weekly_temp,file = paste(format(Sys.time(), "%Y-%m-%d"),
#                                 "_weekly_air-temperature", ".txt", sep = "") ,sep=",",
#             row.names=FALSE,col.names = c("week", "air-temperature"),
#             eol = "\r\n", quote = F)

# 
# write.table(perc_weekly_dis,file = paste(format(Sys.time(), "%Y-%m-%d"),
#                                 "_percentage_of_fast_and_GW_weekly_discharge", ".txt", sep = "") ,sep=",",
#             row.names=FALSE,col.names = c("week","qsim","percentage_GW", "percentage_Fast"),
#             eol = "\r\n", quote = F)

#sources:

#copy files: https://stackoverflow.com/questions/2384517/using-r-to-copy-files#2384621
#rename files: https://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r
#scale_color_manual:https://stackoverflow.com/a/40181166
#weekly data: https://stackoverflow.com/questions/40554231/dplyr-lubridate-how-to-aggregate-a-dataframe-by-week#comment68346741_40554231
#slice(-1) https://stackoverflow.com/a/40261958