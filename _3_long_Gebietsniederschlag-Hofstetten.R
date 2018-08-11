

library(tidyverse)
library(sp)
library(gstat)
library(rgdal)
library(raster)
library(lubridate)
library(reshape)


# Einlesen der Niederschlagsdaten & Koordinaten, beginnend bei 1981

107177 #schon f Loich verwendet
107193 #Loich
107300 #Loich
107466 #Loich

107318 #neu dazu f Hofstetten
107334 #neu
109082 #neu
109074 #neu


path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/Stationsdaten/"
setwd(path)

file1 <- "N-Tagessummen-107177.csv"
file2 <- "N-Tagessummen-107193.csv"
file3 <- "N-Tagessummen-107300.csv"
file4 <- "N-Tagessummen-107466.csv"

file5 <- "N-Tagessummen-107318.csv"
file6 <- "N-Tagessummen-107334.csv"
file7 <- "N-Tagessummen-109082.csv"
file8 <- "N-Tagessummen-109074.csv"


pst1 <- read_csv2(file1, col_names = F, skip = 23, na = "Lücke",cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
))
# because of col_date it takes just the date not the time (specified with %T)

pst2 <- read_csv2(file2, col_names = F, skip = 25, na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
))
pst3 <- read_csv2(file3, col_names = F, skip = 21, na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
))
pst4 <- read_csv2(file4, col_names = F, skip = 21, na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
))


pst5 <- read_csv2(file5, col_names = F, skip = 21, na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
))
pst6 <- read_csv2(file6, col_names = F, skip = 22, na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
))
pst7 <- read_csv2(file7, col_names = F, skip = 21, na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
))
pst8 <- read_csv2(file8, col_names = F, skip = 21, na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y %T"), 
        X2 = col_double()
))


# last entry is "Lücke" so we cut it off

pst1 <- pst1[as_date(pst1$X1) < as_date("2016-01-01"), ]
pst2 <- pst2[as_date(pst2$X1) < as_date("2016-01-01"), ]
pst3 <- pst3[as_date(pst3$X1) < as_date("2016-01-01"), ]
pst4 <- pst4[as_date(pst4$X1) < as_date("2016-01-01"), ]
pst5 <- pst5[as_date(pst5$X1) < as_date("2016-01-01"), ]
pst6 <- pst6[as_date(pst6$X1) < as_date("2016-01-01"), ]
pst7 <- pst7[as_date(pst7$X1) < as_date("2016-01-01"), ]
pst8 <- pst8[as_date(pst8$X1) < as_date("2016-01-01"), ]


# last two stations start at 1981 so we have to cut the others to this date

pst1 <- pst1[as_date(pst1$X1) > as_date("1980-12-31"), ]
pst2 <- pst2[as_date(pst2$X1) > as_date("1980-12-31"), ]
pst3 <- pst3[as_date(pst3$X1) > as_date("1980-12-31"), ]
pst4 <- pst4[as_date(pst4$X1) > as_date("1980-12-31"), ]
pst5 <- pst5[as_date(pst5$X1) > as_date("1980-12-31"), ]
pst6 <- pst6[as_date(pst6$X1) > as_date("1980-12-31"), ]

# put all P-data in one data-frame

pst1 <- add_column(pst1,pst2$X2,pst3$X2,pst4$X2, pst5$X2,pst6$X2,pst7$X2,pst8$X2)
pst1 <- rename(pst1, c(X1="date",X2="N107177", 'pst2$X2'="N107193",'pst3$X2'="N107300",
                       'pst4$X2'="N107466",'pst5$X2'="N107318",'pst6$X2'="N107334",
                       'pst7$X2'="N109082",'pst8$X2'="N109074"))
pst1

############ koord


filexy <- "_Niederschlagstation_coord.csv" # hat mehr stationen drin als die gleichnamige .ods datei!! 
xy <- read_csv2(filexy, col_names = T, cols(
        ID = col_character(),
        x = col_double(),
        y = col_double(),
        elev = col_integer(),
        start = col_integer()
))

# get the stations we need
xy <- filter(xy, ID %in% c("N107177","N107193","N107300","N107466","N107318",
                           "N107334","N109082","N109074")) # not that good
#xy <- filter(xy, start == 1971) # we dont want all 1971s

######
#sum of p just to check
P_sum <- colSums(pst1[2:length(pst1)])

################

# Erstellung eines "data frame" P.int mit den Koordinaten, sowie Summe seit 1981 für die Interpolation

P.int <- as.data.frame(xy)

#P.int[1] <- sapply(P.int$ID, as.character )

P.int$P_sum <- P_sum

P.int <- rename(P.int, c(elev = "z")) %>% select_(., "ID","x","y","z","P_sum")

# Summe seit 1981 + Koordinaten
P.int

#' 
#' ### Plot: Niederschlag gegen Höhe inkl. Regressionsgerade und 95%-Konfidenzintervall
# mit mehr stationen wird es sicher besser

plot_h <- ggplot(data = P.int,aes(x = P_sum, y = z)) +
        geom_smooth(method = "lm") +
        geom_point(aes(color=ID))

plot_h

#' 
#' ### Plot: Koordinative Lage der Stationen
## ---- echo=F,warning=F---------------------------------------------------
plot_coo <- ggplot(data = P.int,aes(x = x, y = y)) +
        geom_point(aes(color=ID))
plot_coo

#' 
#' ## 2.2) Gebiet für räumliche Interpolation
#' ### Erzeugen eines SpatialPointsDataFrame
## ------------------------------------------------------------------------
P.int1 <- P.int # as backup 
coordinates(P.int) = ~x + y
class(P.int)

# Laden der Domain (räumliche Ausdehnung), für die die Interpolation durchgef?hrt wird
# max min long/lat heausfinden dann wie da:
# http://www.geo.ut.ee/aasa/LOOM02331/R_idw_interpolation.html

#make grid
x.range <- as.numeric(c(15.15, 15.57))  # min/max longitude of the interpolation area (aus gis)
y.range <- as.numeric(c(47.87, 48.13))  # min/max latitude of the interpolation area (aus gis)

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01), y = seq(from = y.range[1], 
                      to = y.range[2], by = 0.01))  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

#plot stions + grid
plot(grd, cex = 1.5, col = "grey")
points(P.int, pch = 1, col = "red", cex = 1)


##--------------------------------------------------------------------------
path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/EZG/output/"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/EZG/output/"
setwd(path)

##### HIER NOCH SHAPE VON HOFSTETTEN ERSTELLEN###########

File <- "hofstette_basin.shp" #rechtschreibfehler bei file-benennung
# DomInfo <- read.table(File, header = TRUE, sep=";", skip = 0, dec=".", stringsAsFactors=FALSE)
# str(DomInfo)
# DomInfo <- raster(File)



# Erzeugung SpatialPointsDataFrame
#Schliefau.grid <- DomInfo
# coordinates(Schliefau.grid) = ~x + y #converts to spatial class
# class(Schliefau.grid)
# # Schliefau.grid entspricht einem Raster
# gridded (Schliefau.grid) <- TRUE
# class(Schliefau.grid)

#' 
#' ## 2.3) Interpolation
#' ### Interpolation mittels Thiessen Polygone unter Verwendung des Pakets "gstat"
##
p.tp = krige(P_sum ~ 1, locations=P.int, newdata=grd, 
             nmax = 1) #for this search the neighborhood is set to nmax=1 (Thiessen polygon)

## 
spplot(p.tp["var1.pred"]) ### 
# Lesen von Geodaten: Einzugsgebietsgrenzen als Linie & als Polygon, 
# Pegel & Gewässernetz

path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/EZG/output/"
if( .Platform$OS.type == "windows" )
  path <- "C:/Users/Russ/Desktop/master/daten/EZG/output/"
setwd(path)

dsn <- getwd()
ezg <-readOGR(dsn=dsn,layer="hofstette_basin_polyline") # polyline
ezg.poly <-readOGR(dsn=dsn,layer="hofstette_basin") #ohne .shp extension
#pegel <- readOGR(dsn=dsn,layer="~/Dokumente/BOKU/2017_SS/UE_Hydrologie_WW/BSP2/Einzugsgebietsdaten/PegelSchliefau")
#riv <- readOGR(dsn=dsn,layer="Fluesse_Domain")

readOGR(dsn=dsn,layer="hofstette_basin_polyline")
ogrInfo(dsn=dsn,layer="hofstette_basin") #Infos zu Shape-file

summary(ezg)
class (ezg)
class (ezg.poly)

#' ### Plotten mit Gewässernetz, Pegel EZG-Grenzen und N-Stationen
## ---- echo=F-------------------------------------------------------------
# Definition: (1) colorramp, (2) Maßstabsbalken und dazugehöriger Text, (3) Nord-Pfeil, 
# (4) Shape-files - Gewässernetz, Pegel, Einzugsgebietsgrenzen und N-Stationen
cl <- rev(c("#00007F", "blue", "#007FFF", "cyan",
            "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
jet.colors <- colorRampPalette(cl)

scale = list("SpatialPolygonsRescale", layout.scale.bar(), 
             offset = c(15.238,47.847), scale = 1000, fill=c("transparent","black"),
             which = 1)
text1 = list("sp.text", c(521000,450500), "0", which = 1)
text2 = list("sp.text", c(526000,450500), "2500 m", which = 1)
arrow = list("SpatialPolygonsRescale", layout.north.arrow(type=1), 
             offset = c(526000,451500), scale = 1200, which = 1)
#rv = list("sp.lines", riv, col = "blue", lwd=1.5,zcol=1)
#peg = list("sp.points", pegel, col="red", cex=1.5, pch=19, zcol=1)
catch = list("sp.lines", ezg, col="black", lwd=2.5, fill="transparent") # ezg statt ezg.poly
P.stations <- list('sp.points', P.int, col='black', pch=17, cex=1.8)


spplot(p.tp["var1.pred"], main = "Gebietsniederschlag",
       sp.layout = list( arrow, catch, P.stations, scale, text1, text2),
       col.regions=jet.colors)

### funkt nicht:
plot(ezg)
plot(P.int, add = T)
ezgplot <- ggplot(ezg.poly, aes(x=long, y=lat, group=group)) + 
        geom_path() +
        coord_quickmap()
ezgplot

plot_coo <- ggplot(data = P.int1, aes(x=x,y=y )) +
        geom_point(aes(color=ID))
        
plot_coo

#plot stions + grid + polgon
plot(grd, cex = 1.5, col = "grey")
points(P.int, pch = 1, col = "red", cex = 1)
plot(ezg.poly, add = T)


#' 
#' ## 2.4) Ermittlung des Gebietsniederschlages
## -------------------------------------------------------------
# Definition der Projektionsparameter
my.projection <- crs("+proj=longlat +datum=WGS84 +no_defs")

# Festlegung der Projektion des interpolierten Niederschlagfeldes
proj4string(p.tp) <- my.projection

# Overlay, um den Gebietsniederschlag zu ermitteln
P.sum.jahr <- over(ezg.poly, p.tp["var1.pred"], fn=mean)
P.sum.jahr

#' 
#' # 3) Input für die N-A-Modellierung des Sommerniederschlags
#' ### Niederschläge im Zeitraum vom 01.06.2008 bis 30.09.2008
## ------------------------------------------------------------------------
#' P69 <- P.year[153:274,6:10]
#' 
#' #' 
#' #' ### Erzeugen einer Variablen, in die die Ergebnisse gespeichert werden
#' ## ------------------------------------------------------------------------
# setwd("~/Dokumente/BOKU/2017_SS/UE_Hydrologie_WW/BSP2/Zeitreihen und Koordinaten/")
# File <- "031_Beispiel_2_Daten_Angabe_031.csv"
# P.year1 <- read.table(File, header = TRUE, sep=";", skip = 0, dec=".", stringsAsFactors=FALSE)
P.input.NA <- as.data.frame(pst1[1])

P.input.NA$P<- vector("double",nrow(P.input.NA)) #Spalte P initialisieren

#' 
#' ### Schleife zur Ermittlung des Gebietsniederschlages für jeden Tag
P.d <- pst1[2:length(pst1)]

P.int.d <- as.data.frame(xy)[1:4]
P.int.d$P <- vector("double",nrow(xy))
colnames(P.int.d) <- c("ID","x","y","z","P")
coordinates(P.int.d) = ~x + y
#-----------------------------  Dauert ewig, d gibts doch sicher eine andere möglichkeit? ußerdem wird nur 
ptm <- proc.time()
for (j in 1:12783) {
        P.int.d$P <- unlist(P.d[j,]) #unlist: Umwandlung zu vector
        p.tp = krige(P ~ 1, locations=P.int.d, newdata=grd, 
                     nmax = 1)#  "Durchführen der Interpolation"
        proj4string(p.tp) <- my.projection # "Definition der Projektion"
        P.sum.tag <- over(ezg.poly, p.tp["var1.pred"], fn=mean) # "Gebietsniederschlag über "over()""
        P.input.NA$P[j] <- P.sum.tag$var1.pred #"Speichern des Gebietsniederschlages"
}
proctime()-ptm
#### herrichten für speichern
#P.input.save <- separate(P.input.NA, date, into = c("year", "month", "day"), sep="-") #stimmt nicht
#' 
#' ### Plotten der Zeitreihe der Gebietsniederschläge
## ---- echo=F, warning=F--------------------------------------------------
P.input.save <- P.input.NA
P.input.save$date <- format(P.input.NA$date, "%d%m%Y") # für input modna


plot_geb <- ggplot(data = P.input.NA,aes(x = date, y = P)) + 
      #  xlab("Zeit [d]") + ylab("Gebietsniederschlag [mm/d]") +
      #  scale_x_datetime(date_breaks = "1 month", date_labels = "%m") +
        geom_bar(stat="identity")
plot_geb

#' 
#' ### Summe des Sommerniederschlages
## ------------------------------------------------------------------------
#' sum(P.input.NA$P) # [mm]
#' 
#' #' 
#' #' ### Mittlerer Tagesniederschlag im Einzugsgebiet
#' ## ------------------------------------------------------------------------
#' sum(P.input.NA$P)/length(P.input.NA$P) # [mm/d]

#' 
#' ### Speichern der Zeitreihe der Sommerniederschläge als csv
## ------------------------------------------------------------------------
path <- "/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/EZG/output/"
if( .Platform$OS.type == "windows" )
        path <- "C:/Users/Russ/Desktop/master/daten/EZG/output/"
setwd(path)
# paste(format(Sys.time(), "%Y-%m-%d"),"_P-output", ".pdf", sep = "") to get searchable names
write.table(P.input.save,file = paste(format(Sys.time(), "%Y-%m-%d"),
        "_P-output_HOFSTETTEN", ".txt", sep = "") ,sep=" ", row.names=FALSE,
        col.names = F, quote = F)
#' todo
#' fehler finden warum die eine station genommen wird und nicht die andere? evetuell andere interpol methode?
#' mehr sttionen finden zum interpolieren
#' temoeratur wie interpolieren?

