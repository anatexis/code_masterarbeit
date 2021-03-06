# validation ts of daily values aggregated to monthly values
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


library(hydroGOF)

setwd("C:/Users/Russ/Desktop/master/plotfiles_Hofstetten/VAL_plot_monthly_each_year/")


# group_by was put in the loop
for ( i in seq_len(2013-(2004)+2)){
  
  #prepare vectors for loop
  y <- seq_len(2013-2004+2)
  x <- c(0,365*y) 
  discharge_plot <- discharge[(x[i]+1):x[i+1],]
  
  discharge_plot_w <- discharge_plot %>% 
    mutate(qsim=linout + cascout) %>% 
    select(., TTMMYYYY,Qobs,qsim,linout,cascout) %>%
    group_by(month = month(TTMMYYYY))%>% ### calculate monthly discharge
    summarise(
      Qobs = sum(Qobs),
      qsim = sum(qsim),
      linout = sum(linout),
      cascout = sum(cascout))
  
  nse <- NSE(discharge_plot_w$qsim,discharge_plot_w$Qobs)
  kge <- KGE(discharge_plot_w$qsim,discharge_plot_w$Qobs)
  
  year <- 2003 + i
  
  Q <- ggplot(data= discharge_plot_w)+
    geom_line( aes(x=month, y=Qobs, color = ".Qobs"))+
    geom_line( aes(x=month, y=qsim, color = ".Qsim"))+
    geom_line( aes(x=month, y=linout, color = "slow"))+
    geom_line( aes(x=month, y=cascout, color = "fast"))+
    ggtitle(paste("In the Year", year, "NSE is ",round(nse,2))) +
    xlab("Date")+
    ylab("Discharge [mm]")+
    scale_color_manual(values=c(".Qobs"="#00BFC4", ".Qsim"="#F8766D",
                                "slow"="#7CAE00", "fast"="grey"))
  print(Q) # zum anzeigen
  ggsave(Q,filename=paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),file,"_",year,".png",sep=""))#,
         #height = 3.368173, width = 4.27, units = "in")
  
  
}
