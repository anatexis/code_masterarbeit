

#######################

library(tidyverse)
library(lubridate)
library(reshape)



# Einlesen von Qobs und P_T_ET


path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_R/"
if( .Platform$OS.type == "windows" )
        path <- "C:/Users/Russ/Desktop/master/daten/output_R/"
setwd(path)

file1 <- "2018-08-21P_T_ET-Hofstetten.txt"
file2 <- "2018-08-12_Hofstetten_q_obs.txt"

PTET <- read_csv(file1, col_names = T, cols(Datum = col_date(format = "%d%m%Y"),
                                             .default=col_double()))

Qobs <- read_csv(file2, col_names = T, cols( Datum = col_date(format = "%d%m%Y"),
                                                Q = col_double()))
################### for R² on plot#####################

lm_eqn <- function(df,i){
  date0 <- df[[1]]-1990
  m <- lm(df[[i]] ~ date0, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}
# source:https://stackoverflow.com/a/7549819

###########################################################



#################### compare mean of years #################
### yearly Qobs
# dplyr and pipe ftw!
yearly_Qobs <- Qobs %>%
  group_by(year = year(Datum))%>%
  summarise(
    Qobs = mean(Q))


P_Qobs <- ggplot(data= yearly_Qobs,aes(x=year, y=Qobs))+
  geom_line()+
  geom_smooth(method="lm")+
  #  geom_line( aes(x=month, y=linout, color = "lin"))+
  #  geom_line( aes(x=month, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("mean yearly discharge [m³/s]") +
  geom_text(x = 2005, y = 8.5 ,label = lm_eqn(yearly_Qobs,2), parse = TRUE) # from above 
P_Qobs



# yearly PTET-stuff
yearly_PTET <- PTET %>%
  group_by(year = year(Datum))%>%
  summarise(
    t = mean(t),
    NSeff = mean(NSeff),
    ET = mean(ET)
    )

#temp
P_t <- ggplot(data = yearly_PTET,aes(x = year, y=t))+
  geom_line()+
  geom_smooth(method="lm")+
  #  geom_line( aes(x=month, y=linout, color = "lin"))+
  #  geom_line( aes(x=month, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("mean yearly Temperature") +
  geom_text(x = 2005, y = 7.5 ,label = lm_eqn(yearly_PTET,2), parse = TRUE) # from above 
P_t

#NSeff
P_NSeff <- ggplot(data = yearly_PTET,aes(x=year, y=NSeff))+
  geom_line()+
  geom_smooth(method="lm")+
  #  geom_line( aes(x=month, y=linout, color = "lin"))+
  #  geom_line( aes(x=month, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("mean yearly NSeff") +
  geom_text(x = 1998, y = 4.5 ,label = lm_eqn(yearly_PTET,3), parse = TRUE) # from above 
P_NSeff

P_ET <- ggplot(data = yearly_PTET,aes(x=year, y=ET))+
  geom_line()+
  geom_smooth(method="lm")+
  #  geom_line( aes(x=month, y=linout, color = "lin"))+
  #  geom_line( aes(x=month, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("mean yearly ET") +
  geom_text(x = 2005, y = 1.55 ,label = lm_eqn(yearly_PTET,4), parse = TRUE)
P_ET
#
#
#
#
#
#
################ compare mean of months of each year #############
### monthly mean per year Qobs
# dplyr and pipe ftw!
monthly_Qobs <- Qobs %>%
  group_by(year = month(Datum))%>%
  summarise(
    Qobs = mean(Q))


P_Qobs <- ggplot(data= monthly_Qobs,aes(x=year, y=Qobs))+
  geom_line()+
  geom_smooth(method="lm")+
  #  geom_line( aes(x=month, y=linout, color = "lin"))+
  #  geom_line( aes(x=month, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("mean yearly discharge [m³/s]")
P_Qobs


# yearly PTET-stuff
monthly_PTET <- PTET %>%
  group_by(year = year(Datum), month = month(Datum))%>%
  summarise(
    t = mean(t),
    NSeff = mean(NSeff),
    ET = mean(ET)
  )

#temp
P_tm <- ggplot(data = monthly_PTET,aes(x = month, y=t))+
  geom_point()+
  geom_smooth(method="lm")+
  #  geom_line( aes(x=month, y=linout, color = "lin"))+
  #  geom_line( aes(x=month, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("mean monthly Temperature each year")
P_tm

# 
# # cut timeseries to overlapping period from 01.01.1991 to 16.01.2014 
# 
# PTET_valid <- PTET[as_date(PTET$Datum) > as_date("2003-12-31"), ]
# PTET_calib <- PTET[as_date(PTET$Datum)  < as_date("2004-01-01"), ]
# 
# Qobs_valid <- Qobs[as_date(Qobs$Datum) > as_date("2003-12-31"), ]
# Qobs_calib <- Qobs[as_date(Qobs$Datum) < as_date("2004-01-01"), ]
# 
# 
# 
# 
# # change date format for input modna (müsste doch irgendwie mit listen gehen schas)
# 
# #function
# change_date_format<- function(x) {
#     x[1] <- format(x[[1]],"%d%m%Y")
# }
# 
# 
# PTET_valid$Datum <- change_date_format(PTET_valid)
# PTET_calib$Datum <- change_date_format(PTET_calib)
# Qobs_valid$Datum <- change_date_format(Qobs_valid)
# Qobs_calib$Datum <- change_date_format(Qobs_calib)
# 
# 
# 
# 
# 
# path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_R/"
# if( .Platform$OS.type == "windows" )
#   path <- "C:/Users/Russ/Desktop/master/daten/output_R/"
# setwd(path)
# 
# 
# # commented out when written
# # complete data
# 
# #PTET_valid
# write.table(PTET_valid,file = paste(format(Sys.time(), "%Y-%m-%d"),
#                                 "_PTET_VALID_Hofstetten", ".txt", sep = "") ,sep=",",
#             row.names=FALSE,col.names = c("Datum", "t", "NSeff", "ET"),
# #add if on linux:           eol = "\r\n",
#                                             quote = F)
# 
# #PTET_calib
# write.table(PTET_calib,file = paste(format(Sys.time(), "%Y-%m-%d"),
#                                     "_PTET_CALIB_Hofstetten", ".txt", sep = "") ,sep=",",
#             row.names=FALSE,col.names = c("Datum", "t", "NSeff", "ET"),
#             #add if on linux:           eol = "\r\n",
#             quote = F)
# 
# #Qobs_valid !!!!!!!!!!!!!!!ACHTUNG für input in modna noch eine dummyzeile einfügen!!!!!
# write.table(Qobs_valid,file = paste(format(Sys.time(), "%Y-%m-%d"),
#                                     "_Qobs_VALID_Hofstetten", ".txt", sep = "") ,sep=",",
#             row.names=FALSE,col.names = c("Datum", "Q"),
#             #add if on linux:           eol = "\r\n",
#             quote = F)
# 
# #Qobs_calib !!!!!!!!!!!!!!!!ACHTUNG für input in modna noch eine dummyzeile einfügen!!!!!
# write.table(Qobs_calib,file = paste(format(Sys.time(), "%Y-%m-%d"),
#                                     "_Qobs_CALIB_Hofstetten", ".txt", sep = "") ,sep=",",
#             row.names=FALSE,col.names = c("Datum", "Q"),
#             #add if on linux:           eol = "\r\n",
#             quote = F)
# 
