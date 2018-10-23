
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

####   preparations  - weighting per discharge - calibration/validation period ####

# AT and GWT are weighted with their discharge percentage 
Q_m_perc <- Q_m %>% mutate(slow_p=slow/qsim,
               fast_p=fast/qsim) %>% select(slow_p,fast_p)

AT_perc <- AT_m$AirTemp*Q_m_perc$fast_p
GWT_perc <- GWT_m$GWTemp*Q_m_perc$slow_p

WT <- WT_m$WTemp

#split time into calibration and validation intervall
input <- data.frame(WT,AT_perc,GWT_perc)
calib <- input[1:161,]
valid <- input[191:288,]

######################          without lag         #########################

mod_dis <- lm(WT~AT_perc+GWT_perc, data=calib)
summary(mod_dis) # viel schlechter als ohne discharge weighting...

a <- coef(mod_dis)[1] #interc
X1 <- coef(mod_dis)[2] #AT
X2 <- coef(mod_dis)[3] #GWT

#weighted model: mod_w <- a+AT*X1+GWT*X2

mod_w <- a+valid$AT_perc*X1+valid$GWT_perc*X2
mod_w
require(hydroGOF)
ggof(mod_w,valid$WT,ylab=c("T, [°C]"))

#####################          with lag           #########################

#preparations
Q_m_perc <- Q_m %>% mutate(slow_p=slow/qsim,
                           fast_p=fast/qsim) %>% select(slow_p,fast_p)

# Q_m_perc_lag <- Q_m_perc                            # I DIDN'T USE THE LAG FOR Q, JUST FOR T
# Q_m_perc_lag$slow_p <- lead(Q_m_perc_lag$slow_p,3)  # BECAUSE THE R2 WAS WAY BETTER WITHOUT IT


AT_perc <- AT_m$AirTemp*Q_m_perc$fast_p
GWT_perc_lag <- GWT_m_lag$GWTemp*Q_m_perc$slow_p # only the temp is lagged

input <- data.frame(WT,AT_perc,GWT_perc_lag)
calib_lag <- input[1:161,]
valid_lag <- input[191:288,]

mod_dis_lag <- lm(WT~AT_perc+GWT_perc_lag,data=calib_lag)
summary(mod_dis_lag) # schlechter als ohne gewichtung aber besser als mit gewichtung und ohne lag

a <- coef(mod_dis_lag)[1] #interc
X1 <- coef(mod_dis_lag)[2] #AT
X2 <- coef(mod_dis_lag)[3] #GWT

#weighted model: mod_w <- a+AT*X1+GWT*X2

mod_w <- a+valid_lag$AT_perc*X1+valid_lag$GWT_perc_lag*X2
mod_w

ggof(mod_w,valid$WT,ylab=c("T, [°C]"))


#### to do: schauen wie die residuen sich verhalten - wo liegen sie wie weichen sie ab, sommer, winter etc?


