#install.packages("hydroTSM")
library(tidyverse)
library(lubridate)
library(stringi)
detach("package:hydroGOF", unload=TRUE)
setwd("C:/Users/Russ/Desktop/master/daten/output")
file <- "ha3summary.txt"

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

setwd("C:/Users/Russ/Desktop/master/plotfiles_Hofstetten/VAL_plot_each_year/")



for ( i in seq_len(2013-(2004)+2)){
  
  #prepare vectors for loop
  y <- seq_len(2013-2004+2)
  x <- c(0,365*y) 
  discharge_plot <- discharge[(x[i]+1):x[i+1],]
  
  nse <- NSE(discharge_plot$qsim,discharge_plot$Qobs)
  kge <- KGE(discharge_plot$qsim,discharge_plot$Qobs)
  
  year <- 2003 + i
  
  Q <- ggplot(data= discharge_plot)+
    geom_line( aes(x=TTMMYYYY, y=Qobs, color = "Qobs"))+
    geom_line( aes(x=TTMMYYYY, y=qsim, color = "Qsim"))+
    geom_line( aes(x=TTMMYYYY, y=linout, color = "lin"))+
    geom_line( aes(x=TTMMYYYY, y=cascout, color = "casc"))+
    ggtitle(paste("In the Year", year, "NSE is ",round(nse,2))) +
    xlab("Date")+
    ylab("Discharge [mm]")+
    scale_color_manual(values=c("Qobs"="#00BFC4", "Qsim"="#F8766D",
                                "lin"="#7CAE00", "casc"="grey"))
  print(Q) # zum anzeigen
  ggsave(Q,filename=paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),file,"_",year,".png",sep=""),
         height = 3.368173, width = 4.27, units = "in")
  
  
}


(nse <- NSE(discharge$qsim,discharge$Qobs))
(kge <- KGE(discharge$qsim,discharge$Qobs))
