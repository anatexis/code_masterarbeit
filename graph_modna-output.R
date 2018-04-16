library(tidyverse)
library(lubridate)
library(stringi)
detach("package:hydroGOF", unload=TRUE)
setwd("C:/Users/Russ/Desktop/master/daten/output")
file <- "tt2summary.txt"

### to get r to read in files with in the form of
### dmmyyy AND ddmmyyy we have to do smt like this:
discharge <- read_table(file, col_names = T,
                        cols(TTMMYYYY = "c",
                             .default=col_double()))
stri_sub(discharge$TTMMYYYY,-6,0) <- "-"
stri_sub(discharge$TTMMYYYY,-4,0) <- "-"
discharge$TTMMYYYY <- as.Date(discharge$TTMMYYYY, "%d-%m-%Y")
# now the dates are correctly read in

#calculate qsim and select output which is interesting for us
discharge <- discharge %>% mutate(qsim=linout + cascout) %>% 
  select(., TTMMYYYY,Qobs,qsim,linout,cascout)


library(hydroGOF)
nse <- NSE(discharge$qsim,discharge$Qobs)
kge <- KGE(discharge$qsim,discharge$Qobs)

Q <- ggplot(data= discharge)+
  geom_line( aes(x=TTMMYYYY, y=Qobs, color = "Qobs"))+
  geom_line( aes(x=TTMMYYYY, y=qsim, color = "Qsim"))+
  geom_line( aes(x=TTMMYYYY, y=linout, color = "lin"))+
  geom_line( aes(x=TTMMYYYY, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("Discharge [mm]")+
  annotate("text", x=as.Date(16022), y=37,label="nse= ")+
  annotate("text", x=as.Date(16064), y=37,label=as.character(round(nse,2)))+
  annotate("text", x=as.Date(16022), y=34,label="kge= ")+
  annotate("text", x=as.Date(16064), y=34,label=as.character(round(kge,2)))+
  scale_color_manual(values=c("Qobs"="#00BFC4", "Qsim"="#C77CFF",
                              "lin"="#7CAE00", "casc"="#F8766D"))
  
Q
setwd("C:/Users/Russ/Desktop/master/plotfiles_neu")
file = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),"_Q",".png",sep="")
ggsave(file, height = 3.368173, width = 4.27, units = "in")

Q_end <- ggplot(data= discharge)+
  geom_line( aes(x=TTMMYYYY, y=qsim, color = ".Qsim"))+
  geom_line( aes(x=TTMMYYYY, y=Qobs, color = "Qobs"))+
  xlab("Date")+
  ylab("Discharge [mm]")+
  annotate("text", x=as.Date(16022), y=37,label="nse= ")+
  annotate("text", x=as.Date(16064), y=37,label=as.character(round(nse,2)))+
  annotate("text", x=as.Date(16022), y=34,label="kge= ")+
  annotate("text", x=as.Date(16064), y=34,label=as.character(round(kge,2)))+
  scale_color_manual(values=c("Qobs"="#00BFC4", ".Qsim"="#F8766D"))

Q_end

setwd("C:/Users/Russ/Desktop/master/plotfiles_nur_Q_END")
file = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),"_Q_END",".png",sep="")
ggsave(file, height = 3.368173, width = 4.27, units = "in")

##to trace my changes copy inputfile.txt to directories of plots

file <- list.files("C:/Users/Russ/Desktop/master/daten/input/",
                    "inputmodna.txt", full.names = TRUE)
file.copy(file,"C:/Users/Russ/Desktop/master/plotfiles_neu")
file.copy(file,"C:/Users/Russ/Desktop/master/plotfiles_nur_Q_END/")

##rename files

# in plotfiles_nur_Q_END
file.rename("inputmodna.txt",paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),
                                   "_inputmodna", ".txt", sep = ""))
# in plotfiles_neu
setwd("C:/Users/Russ/Desktop/master/plotfiles_neu")

setwd("C:/Users/Russ/Desktop/master/plotfiles_neu")
file.rename("inputmodna.txt",paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),
                                   "_inputmodna", ".txt", sep = ""))


#sources:

#copy files: https://stackoverflow.com/questions/2384517/using-r-to-copy-files#2384621
#rename files: https://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r
#scale_color_manual:https://stackoverflow.com/a/40181166

#------
#that I can compare nse kge better
nse
kge
