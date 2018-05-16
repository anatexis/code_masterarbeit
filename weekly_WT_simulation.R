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
perc_weekly_dis <- weekly_dis %>% mutate(perc_GW = round(linout / qsim,2),
                                         perc_Fast = round(cascout / qsim,2)) %>% 
  select(., perc_GW, perc_Fast, qsim)

### the same procedure with the temp
# select output which is interesting for us
airtemp <- airtemp %>% select(., TTMMYYY,Temp)

### calculate weekly temperature
# dplyr and pipe ftw!
weekly_temp <- airtemp %>%
  group_by(week = week(TTMMYYY))%>%
  summarise(
    Temp = mean(Temp))



# 
# library(hydroGOF)
# nse <- NSE(weekly_dis$qsim,weekly_dis$Qobs)
# kge <- KGE(weekly_dis$qsim,weekly_dis$Qobs)
# 
# 
# Q <- ggplot(data= weekly_dis)+
#   geom_line( aes(x=week, y=Qobs, color = "Qobs"))+
#   geom_line( aes(x=week, y=qsim, color = "Qsim"))+
# #  geom_line( aes(x=week, y=linout, color = "lin"))+
# #  geom_line( aes(x=week, y=cascout, color = "casc"))+
#   xlab("Date")+
#   ylab("mean weekly discharge [mm]")+
#   annotate("text", x=40, y=10,label="nse= ")+
#   annotate("text", x=45.5, y=10,label=as.character(round(nse,2)))+
#   annotate("text", x=40, y=9,label="kge= ")+
#   annotate("text", x=45.5, y=9,label=as.character(round(kge,2)))+
#   scale_color_manual(values=c("Qobs"="#00BFC4", "Qsim"="#C77CFF",
#                               "lin"="#7CAE00", "casc"="#F8766D"))
# 
# Q
# 
# setwd("C:/Users/Russ/Desktop/master/plotfiles_weekly")
# file = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),"_Q_weekly",".png",sep="")
# ggsave(file, height = 3.368173, width = 4.27, units = "in")
# 
# ##to trace my changes copy inputfile.txt to directories of plots
# 
# file <- list.files("C:/Users/Russ/Desktop/master/daten/input/",
#                    "inputmodna.txt", full.names = TRUE)
# file.copy(file,"C:/Users/Russ/Desktop/master/plotfiles_weekly")
# 
# ##rename files
# 
# # in plotfiles_weekly
# file.rename("inputmodna.txt",paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),
#                                    "_inputmodna", ".txt", sep = ""))


#sources:

#copy files: https://stackoverflow.com/questions/2384517/using-r-to-copy-files#2384621
#rename files: https://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r
#scale_color_manual:https://stackoverflow.com/a/40181166
#weekly data: https://stackoverflow.com/questions/40554231/dplyr-lubridate-how-to-aggregate-a-dataframe-by-week#comment68346741_40554231
#slice(-1) https://stackoverflow.com/a/40261958