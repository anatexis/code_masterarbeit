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


## correlation matrix discharge / WT / AT / GWT

# (cor(data.frame(Q_m$qsim, Q_m$slow, Q_m$fast, WT_m$WTemp,AT_m$AirTemp,GWT_m$GWTemp),
#      use = "pairwise.complete.obs"))

(cor(data.frame(WT_m$WTemp,AT_m$AirTemp,GWT_m$GWTemp),
     use = "pairwise.complete.obs"))
# pairwise.complete.obs just removes na when comparing pairs not whole row! (corr of WT&AT uneffected bc there are
# no NAs) Source: https://stackoverflow.com/a/18892108
## correlation between WT & AT is very high
## correlation between WT & GWT and AT & GWT is negative (shift!)


## cross correlation
WT <- WT_m$WTemp
AT <- AT_m$AirTemp
GWT <- GWT_m$GWTemp

ccWT_AT <- ccf(WT,AT,lag.max = 10, type="correlation", na.action=na.pass)
ccWT_AT

ccWT_GWT <- ccf(GWT,WT,lag.max = 10, na.action = na.pass)
ccWT_GWT

Find_Abs_Max_CCF<- function(a,b) # source https://stackoverflow.com/a/20133091
{
  d <- ccf(a, b, plot = FALSE, lag.max = length(a)-5, na.action = na.pass)
  cor = d$acf[,,1]
  abscor = abs(d$acf[,,1])
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  absres = data.frame(abscor,lag)
  absres_max = res[which.max(absres$abscor),]
  return(absres_max)
}

Find_Abs_Max_CCF(GWT,WT) # so with a lag of 4 (months) there is a correlation of 0.85! (with calibration period its a lag of 3)


### multiple regression 

input <- data.frame(WT,AT,GWT)
calib <- input[1:161,]
valid <- input[191:288,]

# 
pairs(calib, gap=0, cex.labels=0.9)


# Create the relationship model.
model <- lm(WT~AT+GWT, data = calib)

# Show the model
print(model)
summary(model)
mean(model$residuals)
acf(model$residuals)
plot(model)
# intepretation of plot (sources: https://stats.stackexchange.com/a/65864)
# 1st plot( residuals vs fitted): there is some nonlinearity!! (change model!)  (source: https://stats.stackexchange.com/a/76228)


# getting the intercept and the coeff

a <- coef(model)[1] #interc
X1 <- coef(model)[2] #AT
X2 <- coef(model)[3] #GWT

#model: WT_m <- a+AT*X1+GWT*X2

WT_model <- a+valid$AT*X1+valid$GWT*X2
WT_model
WT
plot(WT_model, type="l")
lines(valid$WT, col="red")

cor(data.frame(WT_model,valid$WT),use = "pairwise.complete.obs")
plot(data.frame(WT_model,valid$WT))
require(hydroGOF)

ggof(WT_model,valid$WT,ylab=c("T, [°C]"))

#############################################################################
### try model with a GWT with lag!
##############################################################
Find_Abs_Max_CCF(calib$GWT, calib$WT) #there is a lag of 3 so we have to use a lead of 3 to get to where we want to!
# The lag k value returned by ccf(x, y) estimates the correlation between x[t+k] and y[t]. (from ccf help)


###preparatioin for lag (we have to include 3 more months because we will lose three bc of lag 3)


    path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
    if( .Platform$OS.type == "windows" )
      path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
    setwd(path)
    
    file1 <- "Grundwassertemperatur-Monatsmittel-322917.csv"
    
    gwst1 <- read_csv2(file1, col_names = F, skip = 34, na = "Lücke",cols(
      X1 = col_date(format = "%d.%m.%Y %T"), 
      X2 = col_double()
    )) %>% select(.,date=X1,GWTemp=X2)
    
    #subset to 1991-2014 + 3 months
    gwst1
    gwtemp_m <- gwst1[as_date(gwst1$date) < as_date("2015-04-01"), ]
    GWT_m2 <- gwtemp_m[as_date(gwtemp_m$date) > as_date("1990-12-31"), ]
    tail(GWT_m2)
    
    
    GWT_m_lag <- lead(GWT_m2$GWTemp, 3) #lead 3
    GWT_m_lag <- GWT_m_lag[1:288] # to remove the last three NAs

    
# end of preparation ########################

    
# correlation table
(cor(data.frame(Q_m$qsim, Q_m$slow, Q_m$fast, WT_m$WTemp,AT_m$AirTemp,GWT_m_lag),
     use = "pairwise.complete.obs"))
# WT, AT, GWT_lag are highly correlated! (not inependent)
pairs(data.frame(Q_m$qsim, Q_m$slow, Q_m$fast, WT_m$WTemp,AT_m$AirTemp,GWT_m_lag))

### split again in calibration and validation
# calibration from 1991 to 2004-6 
# validation from 2008-10 to 2014
# 2004-6 to 2008-10 : period where there is no data of water temperature

input_lag <- data.frame(WT,AT,GWT_m_lag)
calib_lag <- input_lag[1:161,]
valid_lag <- input_lag[191:288,]

pairs(calib_lag, gap=0, cex.labels=0.9)


# Create the relationship model.
model_lag <- lm(WT~AT+GWT_m_lag, data = calib_lag)

# Show the model
print(model_lag)
summary(model_lag)
plot(model_lag)


# getting the intercept and the coeff

a <- coef(model_lag)[1] #interc
X1 <- coef(model_lag)[2] #AT
X2 <- coef(model_lag)[3] #GWT

#model_lag: WT_m <- a+AT*X1+GWT*X2

WT_model_lag <- a+valid_lag$AT*X1+valid_lag$GWT_m_lag*X2
WT_model_lag
WT
plot(WT_model_lag, type="l")
lines(valid_lag$WT, col="red")

cor(data.frame(WT_model_lag,valid_lag$WT),use = "pairwise.complete.obs")
plot(data.frame(WT_model_lag,valid_lag$WT))


ggof(WT_model_lag,valid_lag$WT,ylab=c("T, [°C]"))

############################################################
# our model

#model_wt <- 


