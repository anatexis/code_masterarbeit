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

### calculate monthly discharge
# dplyr and pipe ftw!
monthly_dis <- discharge %>%
  group_by(month = month(TTMMYYYY))%>%
  summarise(
      Qobs = mean(Qobs),
      qsim = mean(qsim),
      linout = mean(linout),
      cascout = mean(cascout))


library(hydroGOF)
nse <- NSE(monthly_dis$qsim,monthly_dis$Qobs)
kge <- KGE(monthly_dis$qsim,monthly_dis$Qobs)


Q <- ggplot(data= monthly_dis)+
  geom_line( aes(x=month, y=Qobs, color = "Qobs"))+
  geom_line( aes(x=month, y=qsim, color = "Qsim"))+
#  geom_line( aes(x=month, y=linout, color = "lin"))+
#  geom_line( aes(x=month, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("mean monthly discharge [mm]")+
  annotate("text", x=9, y=7,label="nse= ")+
  annotate("text", x=10.25, y=7,label=as.character(round(nse,2)))+
  annotate("text", x=9, y=6,label="kge= ")+
  annotate("text", x=10.25, y=6,label=as.character(round(kge,2)))+
  scale_color_manual(values=c("Qobs"="#00BFC4", "Qsim"="#C77CFF",
                              "lin"="#7CAE00", "casc"="#F8766D"))

Q

setwd("C:/Users/Russ/Desktop/master/plotfiles_monthly")
file = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),"_Q_monthly",".png",sep="")
ggsave(file, height = 3.368173, width = 4.27, units = "in")

##to trace my changes copy inputfile.txt to directories of plots

file <- list.files("C:/Users/Russ/Desktop/master/daten/input/",
                   "inputmodna.txt", full.names = TRUE)
file.copy(file,"C:/Users/Russ/Desktop/master/plotfiles_monthly")

##rename files

# in plotfiles_monthly
file.rename("inputmodna.txt",paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),
                                   "_inputmodna", ".txt", sep = ""))


#sources:

#copy files: https://stackoverflow.com/questions/2384517/using-r-to-copy-files#2384621
#rename files: https://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r
#scale_color_manual:https://stackoverflow.com/a/40181166
#monthly data: https://stackoverflow.com/questions/40554231/dplyr-lubridate-how-to-aggregate-a-dataframe-by-week#comment68346741_40554231