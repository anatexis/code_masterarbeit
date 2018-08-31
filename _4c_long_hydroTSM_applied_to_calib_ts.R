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

( m <- daily2monthly(x, FUN=mean) )

dates <- time(x)

( nyears <- yip(from=start(x), to=end(x), out.type="nmbr" ) )

smry(x)

hydroplot(x, var.type="Flow", main="at gauge Hofstetten",
          pfreq = "dm", from="1991-01-01")

# Seasonal Analysis
# 1. Average seasonal values of discharge
seasonalfunction(x, FUN=sum, na.rm=TRUE) / nyears

# 2. Extracting the seasonal values for each year
( DJF <- dm2seasonal(x, season="DJF", FUN=sum) )
( MAM <- dm2seasonal(m, season="MAM", FUN=sum) )
( JJA <- dm2seasonal(m, season="JJA", FUN=sum) )
( SON <- dm2seasonal(m, season="SON", FUN=sum) )

# 3. Plotting the time evolution of the seasonal discharge values
# didnt work used graphics.off(), par("mar") and par(mar=c(1,1,1,1))
# than it did work! (par("mar") was originally 5.1,4.1,4.1,2.1)
# source:https://stackoverflow.com/a/26074211
# ODER leichtere lösung, einfach das plot fenster größer machen und zwar nach oben! 
# (ich habs nur waagrecht aufgezogen...)
hydroplot(x, pfreq="seasonal", FUN=mean, stype="default")
