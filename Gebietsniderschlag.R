
## ----setup, include=FALSE------------------------------------------------

library(tidyverse)
library(sp)
library(gstat)
library(rgdal)
library(raster)

#' # 1) Füllen von Lücken mitels linearer Regression
#' ## 1.1) Einlesen der Daten
#' 
## ------------------------------------------------------------------------
# Einlesen der Niederschlagsdaten & Koordinaten
setwd("~/Dokumente/BOKU/2017_SS/UE_Hydrologie_WW/BSP2/Zeitreihen und Koordinaten")
File <- "031_Beispiel_2_Daten_Angabe_031.csv"
P.year <- read.table(File, header = TRUE, sep=";", skip = 0, dec=".", stringsAsFactors=FALSE)
File2 <- "031_Beispiel_2_Koordinaten_Angabe_031.csv"
xy <- read.table(File2, header = TRUE, sep=";", skip = 0, dec=".", stringsAsFactors=FALSE)

#' 
#' ## 1.2) Lineare Regression
#' 
#' ### Berechnen der größten Korrelation
## ------------------------------------------------------------------------
# Füllen der Niederschlagslücken der ersten Station im Dezember mittels linearer Regression
# Korrelation der ersten Station (Spalte 6) mit den ?brigen Stationen (Spalten (6:ncol(P.year))
cor.P <- cor(P.year[,6], P.year[,6:ncol(P.year)], use="complete.obs", method="pearson")

# Vectoren für die lineare Regression / Station (Spalte) mit der größten Korrelation
P1.miss <- P.year[,6] # Zeitreihe mit fehlenen Daten (=abh. Var.)
P2 <- P.year[,which.max(cor.P[2:length(cor.P)])+5] # Zeitreihe mit der größten Korrelation (=unabh. Var.) 

#' 
#' ### Füllen der Lücken
## ---- results="hide"-----------------------------------------------------
# -1: ohne Achsenabschnitt (y=a+b*x, wobei a=0)
lm.P12 <- lm(P1.miss~P2-1)

summary(lm.P12)

#Ermittlung der fehlenden Zeitreihenwerte als Funktion der linearen Modells
P1.pred <- P2*lm.P12$coefficients

# Erzeugen eines Ergebnis-Vektors
P1 <- P1.miss
y <- which(is.na(P1.miss)==TRUE)
P1[y] <- P1.pred[y]
P.year.old <- P.year
P.year[6] <- P1

#' 
#' ### Plot des Niederschlags mit interpolierten Daten (rot)
## ---- echo = F, warning = F----------------------------------------------
#vorbereitung f plotten
plot.P.year.old <- P.year.old %>%
        transmute(Datum = ISOdate(yyyy,mm,dd),P=ID_108) 
plot.P.year.new <- P.year %>%
        transmute(Datum = ISOdate(yyyy,mm,dd),P=ID_108) 
plot.P.year.new <- plot.P.year.new[336:366,]

#plot
plot_int <- ggplot(data = plot.P.year.old,aes(x = Datum, y = P)) + 
        xlab("Zeit [d]") + ylab("Niederschlag ID_108 [mm/d]") +
        scale_x_datetime(date_breaks = "1 month", date_labels = "%m") +
        geom_bar(stat="identity") +
        geom_bar(data = plot.P.year.new,aes(x = Datum, y = P), fill="red",stat="identity")
plot_int

#' 
#' 
#' # 2) Ermittlung der Summe des Gebietsniederschlages für das EZG
#' ## 2.1) Jahressummen + Koordinaten
## ---- results="hide"-----------------------------------------------------
# Matrize mit Niederschlags-Zeitreihen ohne Lücken / Jahressummen für die Interpolation
P_sum <- colSums(P.year[6:length(P.year)])

# Erstellung eines "data frame" P.int mit den Koordinaten, sowie Jahressummen für die Interpolation

P.int <- as.data.frame(xy)

P.int[1] <- sapply(P.int$ID, as.character )

P.int$P_sum <- P_sum

P.int <- rename(P.int, z = Elev)

# Jahressumme + Koordinaten
P.int

#' 
#' ### Plot: Niederschlag gegen Höhe inkl. Regressionsgerade und 95%-Konfidenzintervall
## ---- echo=F,warning=F---------------------------------------------------
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
coordinates(P.int) = ~x + y
class(P.int)

# Laden der Domain (räumliche Ausdehnung), für die die Interpolation durchgef?hrt wird
setwd ("~/Dokumente/BOKU/2017_SS/UE_Hydrologie_WW/BSP2/Einzugsgebietsdaten")
File <- "Schliefau_Domain.csv"
DomInfo <- read.table(File, header = TRUE, sep=";", skip = 0, dec=".", stringsAsFactors=FALSE)
str(DomInfo)


# Erzeugung SpatialPointsDataFrame
Schliefau.grid <- DomInfo
coordinates(Schliefau.grid) = ~x + y #converts to spatial class
class(Schliefau.grid)
# Schliefau.grid entspricht einem Raster
gridded (Schliefau.grid) <- TRUE
class(Schliefau.grid)

#' 
#' ## 2.3) Interpolation
#' ### Interpolation mittels Thiessen Polygone unter Verwendung des Pakets "gstat"
## ------------------------------------------------------------------------
p.tp = krige(P_sum ~ 1, locations=P.int, newdata=Schliefau.grid, 
             nmax = 1) #for this search the neighborhood is set to nmax=1 (Thiessen polygon)

## ---- include=F----------------------------------------------------------
spplot(p.tp["var1.pred"])
# Lesen von Geodaten: Einzugsgebietsgrenzen als Linie & als Polygon, 
# Pegel & Gewässernetz
setwd ("~/Dokumente/BOKU/2017_SS/UE_Hydrologie_WW/BSP2/Einzugsgebietsdaten")
dsn <- getwd()
ezg <-readOGR(dsn=dsn,layer="EZG_Schliefau_line")
ezg.poly <-readOGR(dsn=dsn,layer="EZG_Schliefau")
pegel <- readOGR(dsn=dsn,layer="PegelSchliefau")
riv <- readOGR(dsn=dsn,layer="Fluesse_Domain")

ogrInfo(dsn=dsn,layer="EZG_Schliefau") #Infos zu Shape-file

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
             offset = c(521000,451000), scale = 5000, fill=c("transparent","black"),
             which = 1)
text1 = list("sp.text", c(521000,450500), "0", which = 1)
text2 = list("sp.text", c(526000,450500), "2500 m", which = 1)
arrow = list("SpatialPolygonsRescale", layout.north.arrow(type=1), 
             offset = c(526000,451500), scale = 1200, which = 1)
rv = list("sp.lines", riv, col = "blue", lwd=1.5,zcol=1)
peg = list("sp.points", pegel, col="red", cex=1.5, pch=19, zcol=1)
catch = list("sp.lines", ezg, col="black", lwd=2.5, fill="transparent")
P.stations <- list('sp.points', P.int, col='black', pch=17, cex=1.8)

spplot(p.tp["var1.pred"], main = "Gebietsniederschlag",
       sp.layout = list(catch, rv, peg, arrow, P.stations, scale, text1, text2), 
       col.regions=jet.colors)

#' 
#' ## 2.4) Ermittlung des Gebietsniederschlages
## ------------------------------------------------------------------------
# Definition der Projektionsparameter
my.projection <- crs("+proj=lcc +lat_1=46 +lat_2=49 +lat_0=47.5 +lon_0=13.33333333333333 +x_0=400000 
                     +y_0=400000 +ellps=bessel +units=m +no_defs")

# Festlegung der Projektion des interpolierten Niederschlagfeldes
proj4string(p.tp) <- my.projection

# Overlay, um den Gebietsniederschlag zu ermitteln
P.sum.jahr <- over(ezg.poly, p.tp["var1.pred"], fn=mean)
P.sum.jahr

#' 
#' # 3) Input für die N-A-Modellierung des Sommerniederschlags
#' ### Niederschläge im Zeitraum vom 01.06.2008 bis 30.09.2008
## ------------------------------------------------------------------------
P69 <- P.year[153:274,6:10]

#' 
#' ### Erzeugen einer Variablen, in die die Ergebnisse gespeichert werden
## ------------------------------------------------------------------------
setwd("~/Dokumente/BOKU/2017_SS/UE_Hydrologie_WW/BSP2/Zeitreihen und Koordinaten/")
File <- "031_Beispiel_2_Daten_Angabe_031.csv"
P.year1 <- read.table(File, header = TRUE, sep=";", skip = 0, dec=".", stringsAsFactors=FALSE)
P.input.NA <- as.data.frame(P.year1[153:274,1:3])
P.input.NA$P<- vector("double",nrow(P.input.NA)) #Spalte P initialisieren

#' 
#' ### Schleife zur Ermittlung des Gebietsniederschlages für jeden Tag
## ---- results="hide"-----------------------------------------------------
P.int.d <- as.data.frame(xy)
P.int.d$P <- vector("double",nrow(xy))
colnames(P.int.d) <- c("ID","x","y","z")
coordinates(P.int.d) = ~x + y
for (j in 1:122) {
        P.int.d$P <- unlist(P69[j,]) #unlist: Umwandlung zu vector
        p.tp = krige(P ~ 1, locations=P.int.d, newdata=Schliefau.grid, 
                     nmax = 1)#  "Durchführen der Interpolation"
        proj4string(p.tp) <- my.projection # "Definition der Projektion"
        P.sum.tag <- over(ezg.poly, p.tp["var1.pred"], fn=mean) # "Gebietsniederschlag über "over()""
        P.input.NA$P[j] <- P.sum.tag$var1.pred #"Speichern des Gebietsniederschlages"
}


#' 
#' ### Plotten der Zeitreihe der Gebietsniederschläge
## ---- echo=F, warning=F--------------------------------------------------
P.input.NA <- P.input.NA %>%
       transmute(Datum = ISOdate(yyyy,mm,dd),P=P) 


plot_geb <- ggplot(data = P.input.NA,aes(x = Datum, y = P)) + 
        xlab("Zeit [d]") + ylab("Gebietsniederschlag [mm/d]") +
        scale_x_datetime(date_breaks = "1 month", date_labels = "%m") +
        geom_bar(stat="identity")
plot_geb

#' 
#' ### Summe des Sommerniederschlages
## ------------------------------------------------------------------------
sum(P.input.NA$P) # [mm]

#' 
#' ### Mittlerer Tagesniederschlag im Einzugsgebiet
## ------------------------------------------------------------------------
sum(P.input.NA$P)/length(P.input.NA$P) # [mm/d]

#' 
#' ### Speichern der Zeitreihe der Sommerniederschläge als csv
## ------------------------------------------------------------------------
setwd ("~/Dokumente/BOKU/2017_SS/UE_Hydrologie_WW")
write.table(P.input.NA,file = "ZR_Sommer2015_Angabe031.csv",sep=";", row.names=FALSE)

#' 
#' # 4) Zusammenfassen der Ergebnisse
#' 
#' Nach dem Einlesen der Daten wird die Korrelation zwischen der Station 108 und den restlichen Stationen ermittelt. Die Station mit der höchsten Korrelation (110) wird als Basis verwendet um mittels linearer Regression die Lücken im Dezember der Station 108 zu füllen.  
#' Danach kann der aufsummierte Gebietsniederschlag berechnet werden. Dazu wird für jede Station der Jahresniederschlag ermittelt und das in Frage kommende Gebiet koordinativ abgegrenzt. Mittels Thiessen Polygone wird über das gesamte Gebiet räumlich interpoliert. Der Gebietsniederschlag wird dann mit der Function "over()" bestimmt. Der Gebietsniederschlag beträgt `r round(P.sum.jahr[[1]],2)` mm im Jahr 2008.  
#' Für Bsp. 3 wird der Gebietsniederschlag für jeden Tag von 01.06.2008 bis 30.09.2008 benötigt. Dafür extrahiert man die Niederschläge der Stationen für diesen Zeitraum und berechnet über eine Schleife für jeden Tag mit der Funktion "over()" den Gebietsniederschlag. Die Summe des Sommerniederschlages beträgt `r round(sum(P.input.NA$P),2)` mm. (Zur Kontrolle wurde auch der Gebietsniederschlag für jeden Tag im Jahr 2008 berechnet, die Summe ergibt wieder die `r round(P.sum.jahr[[1]],2)` mm.)
#' Der mittlere Tagesniederschlag im Teilzeitraum beträgt `r round(sum(P.input.NA$P)/length(P.input.NA$P),2)` mm, im Vergeleich dazu beträgt der mittlere Tagesniederschlag aufs ganze Jahr gesehen nur $3.24$ mm.
#' Der Sommerniederschlags-Vektor wird im .csv Format abgespeichert.
