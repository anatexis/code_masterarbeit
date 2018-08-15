library(tidyverse)
library(lubridate)
library(reshape)


path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_etpot_main/"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/output_etpot_main/"
setwd(path)

file3 <- "petout_hofstn.txt"

library(stringi)
### to get r to read in files with in the form of
### dmmyyy AND ddmmyyy we have to do smt like this:
PET <- read_table(file3, col_names = c("X1","X2"),col_types = c("c","d")) #schreit fehler aber passt!??

stri_sub(PET$X1,-6,0) <- "-"
stri_sub(PET$X1,-4,0) <- "-"
PET$X1 <- as.Date(PET$X1, "%d-%m-%Y")


## loop through it and then appen it after manipulation


# loop
# initialize vector with 1991 (where everything is allright)
petout <- PET[1:365,]

for ( i in seq_len(2014-(1991))){
  
  #prepare vectors for loop
  y <- seq_len(2014-1991+2)
  x <- 365*y 
  pet <- PET[(x[i]+1):x[i+1],]
  
  pet_lag <- pet %>% mutate(X1=lag(pet$X1[1:365],i)) %>% na.omit()
  
  pet_korr <- add_row(pet_lag,
                              X1=as_date((PET$X1[x[i]]-i+2):(PET$X1[x[i]]+1)),
                              X2=(PET$X2[x[i]+1]):(PET$X2[x[i]+i]),
                              .before=1) #da passt was noch nicht
  
  petout <- petout %>% add_row(.,X1=pet_korr$X1, X2=pet_korr$X2) 
}

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_epot_main/"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/output_etpot_main//"
setwd(path)


# commented out when written
# complete data
write.table(petout,file = paste(format(Sys.time(), "%Y-%m-%d"),
                             "petout_hofstn_korr", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = F,
            #add if on linux:           eol = "\r\n",
            quote = F)
