

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
  date0 <- df[[1]]-1991
  m <- lm(df[[i]] ~ date0, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(p(slope))~"="~p_slope, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        p_slope = format(summary(m)$coefficients[,4][2], digits = 5)))
  as.character(as.expression(eq));                 
}
# source:https://stackoverflow.com/a/7549819
# adapted with https://stackoverflow.com/a/14794775 (point 1) does this work?

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

# slope not significant!

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

# slope not significant!

P_ET <- ggplot(data = yearly_PTET,aes(x=year, y=ET))+
  geom_line()+
  geom_smooth(method="lm")+
  #  geom_line( aes(x=month, y=linout, color = "lin"))+
  #  geom_line( aes(x=month, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("mean yearly ET") +
  geom_text(x = 2005, y = 1.55 ,label = lm_eqn(yearly_PTET,4), parse = TRUE)
P_ET

# slope not significant! (but barely)

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
  group_by(year = year(Datum), month = month(Datum))%>%
  summarise(
    Qobs = mean(Q))


P_Qobs <- ggplot(data= monthly_Qobs,aes(x=month, y=Qobs))+
  geom_point(aes(colour = year))+
  scale_colour_gradientn(colours=rainbow(24)) +
 # geom_smooth(method="lm")+
  #  geom_line( aes(x=month, y=linout, color = "lin"))+
  #  geom_line( aes(x=month, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("mean yearly discharge [m³/s]")#+
 # geom_text(x = 2005, y = 20 ,label = lm_eqn(monthly_Qobs,2), parse = TRUE) 
  
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
  geom_point(aes(colour = year))+
  scale_colour_gradientn(colours=rainbow(24)) +
 # geom_smooth(method="lm")+
  #  geom_line( aes(x=month, y=linout, color = "lin"))+
  #  geom_line( aes(x=month, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("mean monthly Temperature each year") #+
  #geom_text(x = 2005, y = 20 ,label = lm_eqn(monthly_PTET,2), parse = TRUE) 
P_tm


#NSeff
P_NSeffm <- ggplot(data = monthly_PTET,aes(x = month, y=NSeff))+
  geom_point(aes(colour = year))+
  scale_colour_gradientn(colours=rainbow(24)) +
  # geom_smooth(method="lm")+
  #  geom_line( aes(x=month, y=linout, color = "lin"))+
  #  geom_line( aes(x=month, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("mean monthly NSeff each year") #+
#geom_text(x = 2005, y = 20 ,label = lm_eqn(monthly_PTET,2), parse = TRUE) 
P_NSeffm

#ET
P_ETm <- ggplot(data = monthly_PTET,aes(x = month, y=ET))+
  geom_point(aes(colour = year))+
  scale_colour_gradientn(colours=rainbow(24)) +
  # geom_smooth(method="lm")+
  #  geom_line( aes(x=month, y=linout, color = "lin"))+
  #  geom_line( aes(x=month, y=cascout, color = "casc"))+
  xlab("Date")+
  ylab("mean monthly ET each year") #+
#geom_text(x = 2005, y = 20 ,label = lm_eqn(monthly_PTET,2), parse = TRUE) 
P_ETm

########## I CONCLUDE THERE IS NO SIGNIFICANT TREND!! #######################
#############################################################################


