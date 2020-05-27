# Codice per analisi dei point patterns

#impostare una working directory
setwd("C:/lab")  
setwd("C:/Users/asus/Desktop/Lezioni/Ecologia del paesaggio/lab")
# importare dati
#semplifichiamo il nome
covid <- covid_agg
covid<-read.table("covid_agg.csv",header=TRUE")
# per vedere la tabella
head(covid)

#creare un grafico per visualizzazione dei dati relazionando i paesi con i casi
plot(covid$country,covid$cases)
#oppure
attach(covid)
plot(country,cases)

#disposizione delle etichette
#las=0 etichette parallele all'asse x e y (parallel labels)
plot(covid$country,covid$cases,las=0) 
#las=1 etichette orizzontali (horizontal labels)
plot(covid$country,covid$cases,las=1)
#las=2 etichette perpendicolari all'asse x e y (perpendicular labels)
plot(covid$country,covid$cases,las=2)
#las=3 etichette verticali (vertical labels)
plot(covid$country,covid$cases,las=3)

#con il comando cex.axis si cambiano le dimensione delle etichette
plot(covid$country,covid$cases,las=3,cex.axis=0.3)


#installare la libreria ggplot2 e richiamarla
library(ggplot2)

#prendiamo i dati dalla libreria
data(mpg)

#mostriamo le prime sei righe
head(mpg)

#creare un plot del nuovo dataset mpg, modificare l'estetiza (aes) e il tipo di geometria (geom_)
#geom_point per visualizzare punti
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()
#geom_line per visualizzare linee
ggplot(mpg,aes(x=displ,y=hwy))+geom_line()
#geom_polygon per visualizzare poligoni
ggplot(mpg,aes(x=displ,y=hwy))+geom_polygon()

#per avere un grafco gg plot di covid
ggplot(covid,aes(x=lon,y=lat,size=cases))+geom_point()


#installare e richiamare la librery spatstat
library(spatstat)
attach(covid)

#con il comando ppp si crea un nuovo dataset utile per l'analisi spaziale
#ppp(x.coordinates,y.coordinates,x.range,y.range)
covids<-ppp(lon,lat,c(-180,180),c(-90,90))   

#calcoliamo la densità e la rinominiamo d
d<-density(covids)

#grafico della densità
plot(d)

#aggiungere i punti covids al grafico
points(covids) 

#salvare il file in .Rdata
#richiamare la work directory e caricare il file .RData salvato

#richiamare libraria spatstat
library(spatstat)
#cambiare  i colori, palette, e il numero di livelli specificandolo all'interno di una parentesi esterna al comando
cl <- colorRampPalette(c("yellow","orange","red"))(100)
plot(d,col=cl)


#EXERCISE: plot della mapa della densità dal verde al blu
cl2 <- colorRampPalette(c("green","turquoise","blue"))(100)
plot(d,col=cl2)

#completare la mappa aggiungendo i bordi dei paesi
#istallare e richiamare libreria rgdal
library(rgdal)

#scarichaimo un nuovo pacchetto di dati delle coste in vettore
coastlines<-readOGR("ne_10m_coastline.shp")

#per visualizzare il grafico completo
plot(coastlines,add=T)

#EXERCISE: plot della mappa di densità con una nuova colorazione e aggiunta delle coastlines
cl1<-colorRampPalette(c("blue","light blue","light green","yellow"))(100)
plot(d,col=cl1)
plot(coastlines,add=T,col="yellow")

cl2<-colorRampPalette(c("red","orange","yellow","green","blue"))(800)
plot(d,col=cl2)
plot(coastlines,add=T)

cl3<-colorRampPalette(c("green","violet","blue"))(200)
plot(d,col=cl3)
plot(coastlines,add=T)

cl4<-colorRampPalette(c("violet","yellow","green"))(100)
plot(d,col=cl4)
plot(coastlines,add=T)

cl5<-colorRampPalette(c("darkcyan","purple","red"))(200)
plot(d,col=cl5)
plot(coastlines,add=T)

cl6<-colorRampPalette(c("white","blue","green","red","orange","yellow"))(150)
plot(d,col=cl7)
plot(coastlines,add=T)

# selezionare la WD
setwd("C:/lab")

# caricare il file RData
load("sanmarino.RData")

# visualiziamo i dati 
ls()

# dT è la density map
# Tesi è un dataset che si trovava all'interno di "Tesi.RData"
# Tesippp è il point pattern, coordinate della tabella originale 
# a partire da Tesippp siamo riusciti a fare la dT, mappa di densità

# caricare la libreria
library(spatstat)
# visualizziamo la densità di campionamento
points(Tesippp, col="green")
 
head(Tesi)

# questa funzione associa i valori della variabile che vogliamo interpolare al point pattern (punti spaziali)
marks(Tesippp)<-Tesi$Species_richness
# ricordare che $ indica la colonna del dataset da considerare

#possiamo procedere con l'interpolazione, la stima. Creare una mappa continua partendo da valori discreti

#Smooth funzione che stima i valori dove questi non sono stati misurati
interpol<-Smooth(Tesippp)

# rappresentare con la mappa
plot(interpol)
points(Tesippp,col="blue")
# maggiore richchezza nella parte Sud-Est e nella parte centrale


setwd("C:/lab")
# libreria per utilizzare qualsiasi tipo di file vettoriale
library(rgdal)
# possiamo leggere il file .shp
sanmarino<-readOGR("San_Marino.shp")
# e ora plottarlo
plot(sanmarino) #visualizziamo il territorio di San Marino
plot(interpol,add=T) #add=T sovrappone la mappa dell'interpolazione alla mappa di San Marino
points(Tesippp,add=T) #sovrappone i punti alle mappe di prima
plot(sanmarino,add=T) #per far vedere di nuovo i confini di San Marino


## Exercise: plot multiframe di densità e interpolazione
par(mfrow=c(2,1))

plot(dT,main="Density of points")
points(Tesippp,col="blue")

plot(interpol,main="Estimate of species richness")
points(Tesippp,col="blue")

# esercizio: due colonne e una riga
par(mfrow=c(1,2))

plot(dT,main="Density of points")
points(Tesippp,col="blue")

plot(interpol,main="Estimate of species richness")
points(Tesippp,col="blue")



