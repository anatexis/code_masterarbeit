

library(tidyverse)
library(sp)
library(gstat)
library(rgdal)
library(lubridate)
library(reshape)
detach("package:raster", unload=TRUE) # detach if loaded for getting select to work


# Einlesen der Temperaturschlagsdaten & Koordinaten


setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/Stationsdaten")
#file1 <- "LT107284.dat"
file2 <- "LT107300.dat"
file3 <- "LT107466.dat"
file4 <- "LT115642.dat"

# pst1 <- read_table(file1, col_names = F, skip = 22,na = "Lücke", cols( # es fehlen 9 jahre
#         X1 = col_date(format = "%d.%m.%Y"), 
#         X2 = col_time("%T"),
#         X3 = col_double()
# ))
# pst1 <- select(pst1,date = X1, Temp = X3)

pst2 <- read_table(file2, col_names = F, skip = 20,na = "Lücke", cols(
        X1 = col_date(format = "%d.%m.%Y"), 
        X2 = col_time("%T"),
        X3 = col_double()
))
pst2 <- select(pst2,date = X1, Temp = X3)

pst3 <- read_table(file3, col_names = F, skip = 20, cols(
        X1 = col_date(format = "%d.%m.%Y"), 
        X2 = col_time("%T"),
        X3 = col_double()
))
pst3 <- select(pst3,date = X1, Temp = X3)

pst4 <- read_table(file4, col_names = F, skip = 24, cols(
        X1 = col_date(format = "%d.%m.%Y"), 
        X2 = col_time("%T"),
        X3 = col_double()
))
pst4 <- select(pst4,date = X1, Temp = X3)

library(raster) # load raster in just now, otherwise select() doesnt work

# last entry is "Lücke" so we cut it off

#pst1 <- pst1[as_date(pst1$date) < as_date("2017-09-05"), ]
pst2 <- pst2[as_date(pst2$date) < as_date("2017-01-01"), ]
pst3 <- pst3[as_date(pst3$date) < as_date("2017-01-01"), ]
pst4 <- pst4[as_date(pst4$date) < as_date("2017-01-01"), ] # nas von 4.6.7 - 16.06.17


# put all P-data in one data-frame

pst1 <- add_column(pst2,pst3$Temp,pst4$Temp)
pst1 <- rename(pst1, c(Temp="LT107300",'pst3$Temp'="LT107466",
                       'pst4$Temp'="LT115642"))
############ koord


File2 <- "_Temperaturstation_coord.csv"
xy <- read_csv2(File2, col_names = T, cols(
        ID = col_character(),
        x = col_double(),
        y = col_double(),
        elev = col_integer(),
        start = col_integer()
))

# get the stations we need
xy <- filter(xy, ID %in% c("LT115642","LT107300","LT107466")) # not that good
#xy <- filter(xy, start == 1971) # we dont want all 1971s

###________________________________________________darnuter alles löschen__________________________________________________________
###______________________________________________________________________________________________________________________
###_____________________________________________________________________________________________________________________
#' #sum of p just to check
 P_sum <- colMeans(pst1[2:length(pst1)])
#' 
#' ################
#' 
#' # Erstellung eines "data frame" P.int mit den Koordinaten, sowie Jahressummen für die Interpolation
#' 

P.int <- as.data.frame(xy)


#P.int[1] <- sapply(P.int$ID, as.character )

P.int$P_sum <- P_sum

P.int <- rename(P.int, c(elev = "z"))

# Jahressumme + Koordinaten
P.int
#' 
#' #' 
#' #' ### Plot: Niederschlag gegen Höhe inkl. Regressionsgerade und 95%-Konfidenzintervall
#' #
#' # bringt nix weil nur 2 punkte
#' 
#' plot_h <- ggplot(data = P.int,aes(x = P_sum, y = z)) +
#'         geom_smooth(method = "lm") +
#'         geom_point(aes(color=ID))
#' 
#' plot_h
#' 
#' #' 
#' ### Plot: Koordinative Lage der Stationen

plot_coo <- ggplot(data = P.int,aes(x = x, y = y)) +
        geom_point(aes(color=ID))
plot_coo
#' 
#' #' 
#' #' ## 2.2) Gebiet für räumliche Interpolation
#' #' ### Erzeugen eines SpatialPointsDataFrame
#' ## ------------------------------------------------------------------------
P.int1 <- P.int # as backup
coordinates(P.int) = ~x + y
class(P.int)

# Laden der Domain (räumliche Ausdehnung), für die die Interpolation durchgef?hrt wird
# max min long/lat heausfinden dann wie da:
# http://www.geo.ut.ee/aasa/LOOM02331/R_idw_interpolation.html
# einen raster machen 111111

#make grid
x.range <- as.numeric(c(15.15, 15.63))  # min/max longitude of the interpolation area (aus gis)
y.range <- as.numeric(c(47.87, 48.18))  # min/max latitude of the interpolation area (aus gis)

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01), y = seq(from = y.range[1],
                      to = y.range[2], by = 0.01))  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

#plot stions + grid
plot(grd, cex = 1.5, col = "grey")
points(P.int, pch = 1, col = "red", cex = 1)

#' 
#' ##--------------------------------------------------------------------------
#' 
#' setwd("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/EZG/output/")
#' File <- "basin-loich_thr17000.shp"
#' # DomInfo <- read.table(File, header = TRUE, sep=";", skip = 0, dec=".", stringsAsFactors=FALSE)
#' # str(DomInfo)
#' # DomInfo <- raster(File)
#' 
#' 
#' 
#' # Erzeugung SpatialPointsDataFrame
#' #Schliefau.grid <- DomInfo
#' # coordinates(Schliefau.grid) = ~x + y #converts to spatial class
#' # class(Schliefau.grid)
#' # # Schliefau.grid entspricht einem Raster
#' # gridded (Schliefau.grid) <- TRUE
#' # class(Schliefau.grid)
#' 
#' #' 
#' #' ## 2.3) Interpolation
#' #' ### Interpolation mittels Thiessen Polygone unter Verwendung des Pakets "gstat"
#' ##
p.tp = krige(P_sum ~ 1, locations=P.int, newdata=grd, 
              nmax = 1) #for this search the neighborhood is set to nmax=1 (Thiessen polygon)
#' 
#' ## 
#spplot(p.tp["var1.pred"]) ### klar nur 2 punkte111
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
#' 
#' #' #' ### Plotten mit Gewässernetz, Pegel EZG-Grenzen und N-Stationen
#' #' ## ---- echo=F-------------------------------------------------------------
#' # Definition: (1) colorramp, (2) Maßstabsbalken und dazugehöriger Text, (3) Nord-Pfeil,
#' # (4) Shape-files - Gewässernetz, Pegel, Einzugsgebietsgrenzen und N-Stationen
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


spplot(p.tp["var1.pred"], main = "Temperature",
       sp.layout = list( arrow, catch, P.stations, scale, text1, text2),
       col.regions=jet.colors)
#' #' 
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
#' 
#' #' 
#' #' ## 2.4) Ermittlung des Gebietsniederschlages
#' ## -------------------------------------------------------------
# Definition der Projektionsparameter
my.projection <- crs("+proj=longlat +datum=WGS84 +no_defs")

# Festlegung der Projektion des interpolierten Niederschlagfeldes
proj4string(p.tp) <- my.projection

#' # Overlay, um den Gebietsniederschlag zu ermitteln
#' P.sum.jahr <- over(ezg.poly, p.tp["var1.pred"], fn=mean)
#' P.sum.jahr
#' 
#' #' 
#' #' # 3) Input für die N-A-Modellierung des Sommerniederschlags
#' #' ### Niederschläge im Zeitraum vom 01.06.2008 bis 30.09.2008
#' ## ------------------------------------------------------------------------
#' #' P69 <- P.year[153:274,6:10]
#' #' 
#' #' #' 
###_______________________________________________________________________________________________________________
###_____________________________________löschen bis hier her______________________________________________________
###_______________________________________________________________________________________________________________
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
for (j in 1:9744) {
        P.int.d$P <- unlist(P.d[j,]) #unlist: Umwandlung zu vector
        p.tp = krige(P ~ 1, locations=P.int.d, newdata=grd, 
                     nmax = 1)#  "Durchführen der Interpolation"
        proj4string(p.tp) <- my.projection # "Definition der Projektion"
        P.sum.tag <- over(ezg.poly, p.tp["var1.pred"], fn=mean) # "Gebietsniederschlag über "over()""
        P.input.NA$P[j] <- P.sum.tag$var1.pred #"Speichern des Gebietsniederschlages"
}
proc.time()-ptm
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
setwd ("/home/christoph/Dokumente/BOKU/Masterarbeit/Daten/output_R")
# paste(format(Sys.time(), "%Y-%m-%d"),"_P-output", ".pdf", sep = "") to get searchable names
write.table(P.input.save,file = paste(format(Sys.time(), "%Y-%m-%d"),
        "_Temp-output", ".txt", sep = "") ,sep=" ", row.names=FALSE,
        col.names = F, quote = F)
#' todo
#' fehler finden warum die eine station genommen wird und nicht die andere? evetuell andere interpol methode?
#' mehr sttionen finden zum interpolieren
#' temoeratur wie interpolieren?

