library(tidyverse)
library(lubridate)
library(stringi)
detach("package:hydroGOF", unload=TRUE)
setwd("C:/Users/Katrin/Desktop/master/daten/output")
file <- "tt1summary.txt"

### to get r to read in files with in the form of
### dmmyyy and ddmmyyy we have to do smt like this:
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
  annotate("text", x=as.Date(16064), y=34,label=as.character(round(kge,2)))
  
Q
setwd("C:/Users/Katrin/Desktop/master/plotfiles_neu")
file = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),"_Q",".png",sep="")
ggsave(file)

Q_end <- ggplot(data= discharge)+
  geom_line( aes(x=TTMMYYYY, y=qsim, color = ".Qsim"))+
  geom_line( aes(x=TTMMYYYY, y=Qobs, color = "Qobs"))+
  xlab("Date")+
  ylab("Discharge [mm]")+
  annotate("text", x=as.Date(16022), y=37,label="nse= ")+
  annotate("text", x=as.Date(16064), y=37,label=as.character(round(nse,2)))+
  annotate("text", x=as.Date(16022), y=34,label="kge= ")+
  annotate("text", x=as.Date(16064), y=34,label=as.character(round(kge,2)))

Q_end

setwd("C:/Users/Katrin/Desktop/master/plotfiles_nur_Q_END")
file = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),"_Q_END",".png",sep="")
ggsave(file)

### damit ich nachvollziehen kann was ich gemacht hab
casc <- "1"
kcasc <- "2"
klin <- "50"
split <- "0.45"
rootstor <- "70"
changes <- tibble(casc,kcasc,klin,split,rootstor)
write.table(changes,file = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),
                                "_Q", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = c("casc", "kcask", "klin", "split", "rootstor"),
            eol = "\r\n", quote = F)

# auch in plotfiles_neu schreiben
setwd("C:/Users/Katrin/Desktop/master/plotfiles_neu")
changes <- tibble(casc,kcasc,klin,split,rootstor)
write.table(changes,file = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),
                                 "_Q", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = c("casc", "kcask", "klin", "split", "rootstor"),
            eol = "\r\n", quote = F)

#that I can compare nse kge better
nse
kge