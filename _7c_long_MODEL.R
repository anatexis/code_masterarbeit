#########################################################################################
#########################################################################################
###################         READ IN DATA WITH SCRIPT            #########################
#########################################################################################


setwd("C:/Users/Russ/Desktop/master/code_masterarbeit/")

source("_7b_FKA_6f_read_in_Q_WT_AT_GWT.R") #anscheinend funktionierts trozt den warnungen!


detach("package:hydroGOF", unload=TRUE)




#########################################################################################
#########################################################################################
###################               CORRELATIONS                  #########################
#########################################################################################

#preparations for our model - AT and GWT are weighted with their discharge percentage 
# first without lag
Q_m_perc <- Q_m %>% mutate(slow_p=slow/qsim,
               fast_p=fast/qsim) %>% select(slow_p,fast_p)

AT_perc <- AT_m$AirTemp*Q_m_perc$fast_p
GWT_perc <- GWT_m$GWTemp*Q_m_perc$slow_p

mod_dis <- lm(WT_m$WTemp~AT_perc+GWT_perc)
summary(mod_dis) # viel schlechter als ohne discharge weighting

