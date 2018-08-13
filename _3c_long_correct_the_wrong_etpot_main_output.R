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


y <- seq_len(2014-1991)

x <- 365*23 #von 1 bis 23 , ab 2 +1 dazugeben
PET <- add_row(PET,X1=as.Date(PET$X1[x])+1, X2=PET$X2[x+1],.after=x) 



path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_epot_main/"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/output_etpot_main//"
setwd(path)


# commented out when written
# complete data
write.table(PET,file = paste(format(Sys.time(), "%Y-%m-%d"),
                                "petout_hofstn_korr", ".txt", sep = "") ,sep=",",
            row.names=FALSE,col.names = F,
            #add if on linux:           eol = "\r\n", 
            quote = F)


df <- tibble(x = 1:3, y = 3:1)

add_row(df, x = 4:5, y = 0:-1, .af)