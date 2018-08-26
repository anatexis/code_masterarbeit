# following https://cran.r-project.org/web/packages/hydroTSM/vignettes/hydroTSM_Vignette-knitr.pdf

#install.packages("hydroTSM")
library(hydroTSM)
library(tidyverse)
library(lubridate)
library(stringi)
detach("package:hydroGOF", unload=TRUE)
setwd("C:/Users/Russ/Desktop/master/daten/output")
file <- "ha1summary.txt" #"ha1summary.txt" #ha1 ist das mit den endwerten von loich, ha2 verändert sich

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
discharge_ts <- discharge %>% mutate(qsim=linout + cascout) %>% 
  select(., TTMMYYYY,Qobs,qsim,linout,cascout) %>% 
  select(., TTMMYYYY, qsim)


discharge_ts <- read.zoo(discharge_ts) #read.zoo loses the column names and then it works!

x <- window(discharge_ts, start=as.Date("1991-01-01"))

( m <- daily2monthly(x, FUN=sum) )

#######NICHT FERTIG########

