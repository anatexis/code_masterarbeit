

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
      Qobs = mean(Qobs),
      qsim = mean(qsim),
      linout = mean(linout),
      cascout = mean(cascout))


library(hydroGOF)
(nse <- NSE(monthly_dis$qsim,monthly_dis$Qobs))
(kge <- KGE(monthly_dis$qsim,monthly_dis$Qobs))

