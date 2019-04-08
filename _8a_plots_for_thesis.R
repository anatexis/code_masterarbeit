
# first run _7b_FKA_6f_read_in_Q_WT_AT_GWT.R

setwd("C:/Users/Russ/Desktop/master/code_masterarbeit/")

source("_7b_FKA_6f_read_in_Q_WT_AT_GWT.R") #anscheinend funktionierts trozt den warnungen!


detach("package:hydroGOF", unload=TRUE)

######### fast&slow runoff! validation (for hydrological model!) period
Q_m_v34 <- Q_m_v[4:5] %>% # I have tu subset the tibble like this, with select(.,fast,slow) it doesnt work
  rowid_to_column(.,"rowid") %>% 
  gather(.,Q_type,Q,-rowid)

(p <- ggplot(Q_m_v34, aes(rowid, Q, fill = Q_type)) +
    xlab("time [Months]") + ylab("Runoff [mm]") +
    ggtitle("Runoff components (validation period)")+
    geom_bar( stat = "identity")
)

########## fast&slow runoff one year
Q_m_v34_year2005 <- Q_m_v[13:24,4:5] %>% 
  rowid_to_column(.,"rowid") %>% 
  gather(.,Q_type,Q,-rowid)

(p <- ggplot(Q_m_v34_year2005, aes(rowid, Q, fill = Q_type)) +
    xlab("Months") + ylab("Runoff [mm]") +
    ggtitle("Runoff components (year 2005)")+
    geom_bar( stat = "identity")+
    scale_x_continuous(breaks=1:12)
)

###sources:
#https://stackoverflow.com/a/6693427 (put in data long format)
#https://stackoverflow.com/questions/43228109/problems-converting-from-wide-to-long-using-reshape-tibble-issue#comment73526615_43228109 (use gather rather than melt)
# ?gather (how to use it)
       
