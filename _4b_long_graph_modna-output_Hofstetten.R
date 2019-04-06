library(tidyverse)
library(lubridate)
library(stringi)
detach("package:hydroGOF", unload=TRUE)
setwd("C:/Users/Russ/Desktop/master/daten/output")
file <- "ha2summary.txt" #"ha1summary.txt" #ha1 ist das mit den endwerten von loich, ha2 verändert sich

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


library(hydroGOF)
nse <- NSE(discharge$qsim,discharge$Qobs)
kge <- KGE(discharge$qsim,discharge$Qobs)

Q <- ggplot(data= discharge)+
  geom_line( aes(x=TTMMYYYY, y=qsim, color = "Qsim"))+
  geom_line( aes(x=TTMMYYYY, y=Qobs, color = "Qobs"))+
  geom_line( aes(x=TTMMYYYY, y=cascout, color = "casc"))+
  geom_line( aes(x=TTMMYYYY, y=linout, color = "lin"))+
  xlab("Date")+
  ylab("Discharge [mm]")+
  annotate("text", x=as.Date(10600), y=30,label="nse= ")+
  annotate("text", x=as.Date(11000), y=30,label=as.character(round(nse,3)))+
  annotate("text", x=as.Date(10600), y=28,label="kge= ")+
  annotate("text", x=as.Date(11000), y=28,label=as.character(round(kge,3)))+
  scale_color_manual(values=c("Qobs"="#00BFC4", "Qsim"="#F8766D",
                              "lin"="#7CAE00", "casc"="#C77CFF"))
  
Q
# setwd("C:/Users/Russ/Desktop/master/plotfiles_Hofstetten/plots_daily/")
# file = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),"_Q",".png",sep="")
# ggsave(file, height = 4.64, width = 9.28, units = "in")

Q_end <- ggplot(data= discharge)+
  geom_line( aes(x=TTMMYYYY, y=Qobs, color = "Qobs"))+
  geom_line( aes(x=TTMMYYYY, y=qsim, color = ".Qsim"))+
  xlab("Date")+
  ylab("Discharge [mm]")+
  annotate("text", x=as.Date(10600), y=30,label="nse= ")+
  annotate("text", x=as.Date(11000), y=30,label=as.character(round(nse,3)))+
  annotate("text", x=as.Date(10600), y=28,label="kge= ")+
  annotate("text", x=as.Date(11000), y=28,label=as.character(round(kge,3)))+
  scale_color_manual(values=c("Qobs"="#00BFC4", ".Qsim"="#F8766D"))

Q_end

# setwd("C:/Users/Russ/Desktop/master/plotfiles_Hofstetten/plots_daily_Q-END//")
# file = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),"_Q_END",".png",sep="")
# ggsave(file, height = 4.64, width = 9.28, units = "in")
# 
# ##to trace my changes copy inputfile.txt to directories of plots
# 
# file <- list.files("C:/Users/Russ/Desktop/master/daten/input/",
#                     "inputmodna.txt", full.names = TRUE)
# file.copy(file,"C:/Users/Russ/Desktop/master/plotfiles_Hofstetten/plots_daily/")
# file.copy(file,"C:/Users/Russ/Desktop/master/plotfiles_Hofstetten/plots_daily_Q-END/")
# 
# ##rename files
# 
# # in plots_daily_Q_END
# file.rename("inputmodna.txt",paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),
#                                    "_inputmodna", ".txt", sep = ""))
# # in plots_daily
# setwd("C:/Users/Russ/Desktop/master/plotfiles_Hofstetten/plots_daily/")
# file.rename("inputmodna.txt",paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),
#                                    "_inputmodna", ".txt", sep = ""))
#

#sources:

#copy files: https://stackoverflow.com/questions/2384517/using-r-to-copy-files#2384621
#rename files: https://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r
#scale_color_manual:https://stackoverflow.com/a/40181166

#------
#that I can compare nse kge better
nse
kge
