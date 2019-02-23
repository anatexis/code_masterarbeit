
#########################################################################################
###################         READ IN DATA WITH SCRIPT (first: clear all)    ##############
#########################################################################################

# rm(list = ls()) #clear objects from workspace (uncomment it)
setwd("C:/Users/Russ/Desktop/master/code_masterarbeit/")

source("_7b_FKA_6f_read_in_Q_WT_AT_GWT.R") #anscheinend funktionierts trozt den warnungen!


detach("package:hydroGOF", unload=TRUE)




#########################################################################################
#########################################################################################
###################                 W-D model                   #########################
#########################################################################################


###   preparations  - weighting per discharge - calibration/validation period ####

# AT and GWT are weighted with their discharge percentage
Q_m_perc <- Q_m %>% mutate(slow_p=slow/qsim,
               fast_p=fast/qsim) %>% select(slow_p,fast_p)

AT_perc <- AT_m$AirTemp*Q_m_perc$fast_p
GWT_perc <- GWT_m$GWTemp*Q_m_perc$slow_p

WT <- WT_m$WTemp

#split time into calibration and validation intervall
input <- data.frame(WT,AT_perc,GWT_perc)
# calib <- input[1:161,]    # brauch ich nicht, weil ich ja kein modell "trainiere" 
valid <- input[191:288,]    # sondern schon habe?

######################      without lag   whole ts      #########################

model_WT <- input$AT_perc+input$GWT_perc

require(hydroGOF)
ggof(model_WT,input$WT,ylab=c("T, [°C]")) # die nichtvorhandenen daten in der 
                                          # beobachteten wassertemoeratur haben
                                          # keinen einfluss auf die GoF parameter

######################   without lag just validation period #########################

model_WT_va <- valid$AT_perc+valid$GWT_perc

ggof(model_WT_va,valid$WT,ylab=c("T, [°C]"))



###################################################################################
#################### with lag  (in _7b_ gemacht 207-267)  #########################


######### preparations ##############
Q_m_perc <- Q_m %>% mutate(slow_p=slow/qsim,
                           fast_p=fast/qsim) %>% select(slow_p,fast_p)

# Q_m_perc_lag <- Q_m_perc                            # I DIDN'T USE THE LAG FOR Q, JUST FOR T
# Q_m_perc_lag$slow_p <- lead(Q_m_perc_lag$slow_p,3)  # BECAUSE THE R2 WAS WAY BETTER WITHOUT IT

# Lag von GWT schon im _7b_ gemacht (4 months! nur calib is 3! aber hier ja nur whole und val)

AT_perc <- AT_m$AirTemp*Q_m_perc$fast_p
GWT_perc_lag <- GWT_m_lag$GWTemp*Q_m_perc$slow_p # only the gwtemp is lagged

input_lag <- data.frame(WT,AT_perc,GWT_perc_lag)
calib_lag <- input_lag[1:161,]
valid_lag <- input_lag[191:288,]

####### preparations end ###########

##################with lag whole ts #################### 

model_WT_lag <- input_lag$AT_perc+input_lag$GWT_perc_lag

ggof(model_WT_lag,input_lag$WT,ylab=c("T, [°C]")) 

########## wiht lag only validation period #############


model_WT_lag_va <- valid_lag$AT_perc+valid_lag$GWT_perc_lag

ggof(model_WT_lag_va,valid_lag$WT,ylab=c("T, [°C]")) 

#### to do: schauen wie die residuen sich verhalten - wo liegen sie wie weichen sie ab, sommer, winter etc?


################ model just with airtemperature and gw-temp without weighting ###########

####### without lag whole ts###########
uw_model <- AT_m$AirTemp/2+GWT_m$GWTemp/2
ggof(uw_model,WT_m$WTemp,ylab=c("T, [°C]"))


####### without lag validation period #######
uw_model_va <- uw_model[191:288]
ggof(uw_model_va,valid$WT,ylab=c("T, [°C]"))


####### with lag whole ts###########
uw_model_lag <- AT_m$AirTemp/2+GWT_m_lag$GWTemp/2
ggof(uw_model_lag,WT_m$WTemp,ylab=c("T, [°C]"))


####### without lag validation period #######
uw_model_lag_va <- uw_model_lag[191:288]
ggof(uw_model_lag_va,valid$WT,ylab=c("T, [°C]"))



#### zum vergleich (in section 4.3.5.2) !!!!! NUR VALIDATON PERIOD NEHMEN!!!
atdw <- (AT_m$AirTemp*Q_m_perc$fast_p)[191:288] #discharge weighted
atuw <- (AT_m$AirTemp/2)[191:288] # unweighted 

gwtdw <- (GWT_m_lag$GWTemp*Q_m_perc$slow_p)[191:288] # discharge weighted
gwtuw <- (GWT_m_lag$GWTemp/2)[191:288] # nuweighted


plot(model_WT_lag_va, type="l")
lines(uw_model_lag_va, col="red")

# boxplots
boxplot(atdw,atuw,names = c("discharge-weighted","unweighted"),
        ylab="[°C]", main ="Air temperature comparison", ylim=c(-3,18))
boxplot(gwtdw,gwtuw,names = c("discharge-weighted","unweighted"),
        ylab="[°C]", main ="Groundwater temperature comparison",ylim=c(-3,18))
