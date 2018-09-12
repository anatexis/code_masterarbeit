library(tidyverse)
library(lubridate)
library(stringi)
detach("package:hydroGOF", unload=TRUE)
setwd("C:/Users/Russ/Desktop/master/daten/output")
file <- "ha2summary.txt"

### to get r to read in files with in the form of
### dmmyyyy AND ddmmyyyy we have to do smt like this:
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
discharge <- discharge %>% mutate(qsim=linout + cascout,qsim_etp=qsim+ETP) %>% 
  select(., TTMMYYYY,NS,Qobs,ETP,ETA,liqwater,linout,cascout,qsim,qsim_etp)

sums <- discharge[2:length(discharge)] %>% summarise_all(funs(sum)) %>%
  mutate(exNS=Qobs+ETP) %>% 
  select(.,NS,exNS,qsim_etp,Qobs,qsim)

sumplot <- sums %>% 
  gather(.,data,mm_d,NS:qsim, factor_key = TRUE)

ggplot(data=sumplot, aes(x=data, y=mm_d))+
  geom_bar(stat="identity")

perct <- sums %>% mutate(Qperct=qsim/Qobs*100, NSperct=qsim_etp/NS*100, exNS=(Qobs+ETP)/NS) %>% 
  select(.,exNS,NSperct,Qperct) # exNS expected precipitation

sums
perct
(diff_etp <- sums$NS-sums$qsim_etp) # - sim too big ; 
(diff_q <- sums$Qobs-sums$qsim) # - sim too big

(diff_diffs <- abs(diff_etp)-abs(diff_q)) # when + : more etp ; when -: more discharge

  