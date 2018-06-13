#read data
library(tidyverse)
library(lubridate)
library(stringi)
library(reshape)
detach("package:hydroGOF", unload=TRUE)

#read path on windows or linux

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/master/daten/output"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/output/"
setwd(path)

file <- "tt2snowglaz01.txt"

### to get r to read in files with in the form of
### dmmyyy AND ddmmyyy we have to do smt like this:
snowmelt <- read_table(file, col_names = F,
                        cols(X1 = "c",
                             .default=col_double()),skip=2)
stri_sub(snowmelt$X1,-6,0) <- "-"
stri_sub(snowmelt$X1,-4,0) <- "-"
snowmelt$X1 <- as.Date(snowmelt$X1, "%d-%m-%Y")
# now the dates are correctly read in

#calculate qsim and select output which is interesting for us
snowmelt1 <- snowmelt %>% 
  select(., date=X1,
         snowmelt=X7,
         snowmelt_nseff=X8) %>% 
  mutate(., perc_snowmelt=snowmelt/snowmelt_nseff)

ggplot(snowmelt1)+
  geom_line(aes(x=date,y=snowmelt))

snowmelt_weekly <- snowmelt1 %>% 
  group_by(week = week(date))%>%
  summarise(
    snowmelt = mean(snowmelt),
    snowmelt_nseff = mean(snowmelt_nseff),
    perc_snowmelt=snowmelt/snowmelt_nseff)

ggplot()+
  geom_line(data=snowmelt_weekly,aes(x=week,y=snowmelt)) ## schaut nicht so aus, als ob die schneeschmelze einfluss auf die model qualität hätte


snowmelt_weekly[10:20,]
