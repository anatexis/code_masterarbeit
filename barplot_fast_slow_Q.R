library(tidyverse)
library(lubridate)
require(reshape)

detach("package:hydroGOF", unload=TRUE)

#read path on windows or linux

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_R" #stimmt der pfAD?
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/output_R/"
setwd(path)

# load air_water, discharge files
file <- "2018-05-27_percentage_of_fast_and_GW_weekly_discharge.txt"

discharge_split <- read_csv(file, col_names = T) %>% 
  mutate(.,fast=percentage_Fast*qsim,
             slow=percentage_GW*qsim) %>% 
  select(., fast,slow) %>% 
  rowid_to_column(.,"rowid") %>% 
  gather(.,Q_type,Q,-rowid)

(p <- ggplot(discharge_split, aes(rowid, Q, fill = Q_type)) +
    geom_bar( stat = "identity")
)

(p <- ggplot() +
    geom_bar(data=discharge_split, aes(rowid, Q, fill = Q_type),
             position = "fill", stat = "identity")
)

# setwd("C:/Users/Russ/Desktop/Masterarbeit/presentation2/used_plots/hydro_mod/")
# file = paste("Q_fast_slow",".png",sep="")
# ggsave(file, height = 3.368173, width = 4.27, units = "in")

###sources:
#https://stackoverflow.com/a/6693427 (put in data long format)
#https://stackoverflow.com/questions/43228109/problems-converting-from-wide-to-long-using-reshape-tibble-issue#comment73526615_43228109 (use gather rather than melt)
# ?gather (how to use it)
