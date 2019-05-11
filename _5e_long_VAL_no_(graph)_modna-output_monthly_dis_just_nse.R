

#
# JUST NSE OF WEEKLY AGGREGATED VALUES NOTHING ELSE
# (if I can fix the graph in calibration _4e_ I will change it here as well)

library(tidyverse)
library(lubridate)
library(stringi)
detach("package:hydroGOF", unload=TRUE)

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/master/daten/output" #stimmt der pfAD?
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/output"
setwd(path)

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

### calculate monthly discharge 
monthly_dis <- discharge %>%
#  mutate(year_month = strftime(TTMMYYYY, format = "%Y-%W", tz = "CET")) %>% 
  group_by(year = year(TTMMYYYY), month = month(TTMMYYYY)) %>%
  summarise(
      Qobs = sum(Qobs),
      .qsim = sum(qsim),
      linout = sum(linout),
      cascout = sum(cascout)) # mean anstatt sum genommen....


library(hydroGOF)
(nse <- NSE(monthly_dis$.qsim,monthly_dis$Qobs))
(kge <- KGE(monthly_dis$.qsim,monthly_dis$Qobs))

#plot

Q_monthly_dis34 <- monthly_dis[3:4] %>% # I have tu subset the tibble like this, with select(.,fast,slow) it doesnt work
  rowid_to_column(.,"rowid") %>% 
  gather(.,Q_type,Q,-rowid)

(p <- ggplot(Q_monthly_dis34, aes(rowid, Q, color = Q_type)) +
    xlab("Time [Months]") + ylab("Discharge [mm]") +
    geom_line( stat = "identity")+
    
    annotate("text", x=115, y=230,label="nse= ")+
    annotate("text", x=128, y=230,label=as.character(round(nse,3)))+
    annotate("text", x=115, y=218,label="kge= ")+
    annotate("text", x=128, y=218,label=as.character(round(kge,3)))
)

setwd("C:/Users/Russ/Desktop/mt-master/used_pics//")
file = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),"_Q_MONTHLY_VALID",".png",sep="")
ggsave(file, height = 4.64, width = 9.28, units = "in")
