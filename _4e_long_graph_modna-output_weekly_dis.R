library(tidyverse)
library(lubridate)
library(stringi)
detach("package:hydroGOF", unload=TRUE)

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/master/daten/output" #stimmt der pfAD?
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/output"
setwd(path)

file <- "ha2summary.txt"

### to get r to read in files with in the form of
### dmmyyy AND ddmmyyy we have to do smt like this:
discharge <- read_table(file, col_names = T,
                        cols(TTMMYYYY = "c",
                             .default=col_double()))
stri_sub(discharge$TTMMYYYY,-6,0) <- "-"
stri_sub(discharge$TTMMYYYY,-4,0) <- "-"
discharge$TTMMYYYY <- as.Date(discharge$TTMMYYYY, "%d-%m-%Y")
discharge <- head(discharge, -1) # delete last row because its a duplicat (fault lies in modna.f)
tail(discharge)
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


library(hydroGOF)
(nse <- NSE(weekly_dis$qsim,weekly_dis$Qobs))
(kge <- KGE(weekly_dis$qsim,weekly_dis$Qobs))


Q_weekly <- ggplot(data= discharge)+
  geom_line( aes(x=TTMMYYYY, y=cascout, color = "casc"))+
  geom_line( aes(x=TTMMYYYY, y=linout, color = "lin"))+
  geom_line( aes(x=TTMMYYYY, y=Qobs, color = "Qobs"))+
  geom_line( aes(x=TTMMYYYY, y=qsim, color = "Qsim"))+
  xlab("Date")+
  ylab("Discharge [mm]")+
  annotate("text", x=as.Date(10600), y=30,label="nse= ")+
  annotate("text", x=as.Date(11000), y=30,label=as.character(round(nse,3)))+
  annotate("text", x=as.Date(10600), y=28,label="kge= ")+
  annotate("text", x=as.Date(11000), y=28,label=as.character(round(kge,3)))+
  scale_color_manual(values=c("Qobs"="#00BFC4", "Qsim"="#F8766D",
                              "lin"="#7CAE00", "casc"="#C77CFF"))

Q_weekly



setwd("C:/Users/Russ/Desktop/master/plotfiles_Hofstetten/plots_weekly/")
file = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),"_Q_weekly",".png",sep="")
ggsave(file, height = 4.64, width = 9.28, units = "in")

##to trace my changes copy inputfile.txt to directories of plots

file <- list.files("C:/Users/Russ/Desktop/master/daten/input/",
                   "inputmodna.txt", full.names = TRUE)
file.copy(file,"C:/Users/Russ/Desktop/master/plotfiles_Hofstetten/plots_weekly/")

##rename files

# in plots_weekly
file.rename("inputmodna.txt",paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),
                                   "_inputmodna", ".txt", sep = ""))


#sources:

#copy files: https://stackoverflow.com/questions/2384517/using-r-to-copy-files#2384621
#rename files: https://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r
#scale_color_manual:https://stackoverflow.com/a/40181166
#weekly data: https://stackoverflow.com/questions/40554231/dplyr-lubridate-how-to-aggregate-a-dataframe-by-week#comment68346741_40554231