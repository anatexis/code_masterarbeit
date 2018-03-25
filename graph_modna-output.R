library(tidyverse)
library(lubridate)

setwd("C:/Users/Katrin/Desktop/master/daten/output")
file <- "ku5summary.txt"


discharge <- read_table(file, col_names = T,
                        cols(TTMMYYYY = col_date(format = "%d%m%Y"),
                             .default=col_double()))
discharge <- select(discharge, TTMMYYYY,linout,cascout)
discharge %>% mutate(qsim=linout + cascout)
