library(tidyverse)
library(lubridate)

# set working directory
setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten")

# list certain dat files from the current directory
list.files(pattern="N.......dat$") # use the pattern argument to define a common pattern  for import files with regex.

# create a list from these files
list.filenames<-list.files(pattern="N.......dat$")
list.filenames

# create an empty list that will serve as a container to receive the incoming files
list.data<-list()


# create a loop to read in your data
for (i in 1:length(list.filenames))
{
        list.data[[i]]<-read_table(list.filenames[i], col_names = F, skip = 25, cols(
                X1 = col_date(format = "%d.%m.%Y"),
                X2 = col_time(format = ""),
                X3 = col_double()
        ))
}

# add the names of your data to the list
names(list.data)<-list.filenames

# now you can index one of your tables like this
list.data

# plot

plot_P1<- ggplot(data = list.data$N107284.dat,aes(x =X1 , y = X3)) + 
        xlab("Zeit [d]") + ylab("Niederschlag [mm/d]") +
        geom_bar(stat="identity") 
plot_P1

plot_P2<- ggplot(data = list.data$N107300.dat,aes(x =X1 , y = X3)) + 
        xlab("time [d]") + ylab("Precipitation [mm/d]") +
        ggtitle("Station N107300")+
        geom_bar(stat="identity") 
plot_P2

plot_P3<- ggplot(data = list.data$N107466.dat,aes(x =X1 , y = X3)) + 
        xlab("Zeit [d]") + ylab("Niederschlag [mm/d]") +
        geom_bar(stat="identity") 
plot_P3

plot_P4<- ggplot(data = list.data$N115642.dat,aes(x =X1 , y = X3)) + 
        xlab("Zeit [d]") + ylab("Niederschlag [mm/d]") +
        geom_bar(stat="identity") 
plot_P4

# test if there are nas
for (i in 1:length(list.data)){
        print(sum(is.na(list.data[[i]][[3]])))
}

#where are the nas
for (i in 1:length(list.data)){
        print(which(is.na(list.data[[i]][[3]])))
        print(tail(list.data[[i]]))
}
# Nas at the end of the data: cut all timeseries at 2017-01-02
zeitr01 <- as_date(list.data[[1]][[1]])<as_date("2017-01-03")
N107284 <- list.data[[1]][[3]][zeitr01]

zeitr02 <- as_date(list.data[[2]][[1]])<as_date("2017-01-03")
N107300 <- list.data[[2]][[3]][zeitr02]

zeitr03 <- as_date(list.data[[3]][[1]])<as_date("2017-01-03")
N107466 <- list.data[[3]][[3]][zeitr03]

zeitr04 <- as_date(list.data[[4]][[1]])<as_date("2017-01-03")
N115642 <- list.data[[4]][[3]][zeitr04]

tibble(
        z = zeitr01,
        r = zeitr02
)
# unschön vi in loop?
#create tibble for 1971-1991 with two stations<>

zeitr1 <- as_date(list.data[[1]][[1]])<as_date("1992-01-01")
N107284[zeitr1]

zeitr2 <- as_date(list.data[[2]][[1]])<as_date("1992-01-01")
N107300[zeitr2]

P.d1991<- tibble(
        date = seq(from = as.Date("1971-01-01"), to = as.Date("1991-12-31"), by = 1),
        N107284 = N107284[zeitr1],
        N107300 = N107300[zeitr2]
)


#create tibble for 1992-2017 with 4 stations #schirchschirchschirch pipe?
zeitr12 <- as_date(list.data[[1]][[1]])>as_date("1991-12-31")
complete <- N107284[zeitr12]
N107284 <- complete[complete.cases(complete)]

zeitr22 <- as_date(list.data[[2]][[1]])>as_date("1991-12-31")
complete2 <- N107300[zeitr22]
N107300 <- complete2[complete.cases(complete2)]

zeitr3 <- as_date(list.data[[3]][[1]])>as_date("1991-12-31")
complete3 <- N107466[zeitr3]
N107466 <- complete3[complete.cases(complete3)]

zeitr4 <- as_date(list.data[[4]][[1]])>as_date("1991-12-31")
complete4 <- N115642[zeitr4]
N115642 <- complete4[complete.cases(complete4)]


P.d2017 <- tibble(
        date = seq(from = as.Date("1992-01-01"), to = as.Date("2017-01-02"), by = 1),
        N107284 = N107284,
        N107300 = N107300,
        N107466 = N107466,
        N115642 = N115642
        
)

#todo
#ds gleiche f 4 sttionen
#chtung sttion 1 nur bis 2017-01-02, geht dnch noch weiter irgendwie kürzen (wie unten?)
##### SOURCES
#source for multiple imports: https://blogazonia.wordpress.com/2014/05/08/import-multiple-files-to-r/

########################################################################################


zeitr <- list.data[[1]][[1]]>as_date("1991-12-31")
zeitr <- list.data[[1]][[1]]<as_date("2017-01-03")
