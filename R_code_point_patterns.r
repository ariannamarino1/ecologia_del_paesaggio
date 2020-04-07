# Codice per analisi dei point patterns

setwd("C:/lab")  
setwd("C:/Users/asus/Desktop/Lezioni/Ecologia del paesaggio/lab")
# importare dati
covid<-read.table("covid_agg.csv",header=TRUE")
# per vedere la tabella
head(covid)

plot(covid$country,covid$cases)
#oppure
attach(covid)
plot(country,cases)

#disposizione delle etichette
plot(covid$country,covid$cases,las=0) #etichette parallele all'asse x e y (parallel labels)
plot(covid$country,covid$cases,las=1) #etichette orizzontali (horizontal labels)
plot(covid$country,covid$cases,las=2) #etichette perpendicolari all'asse x e y (perpendicular labels)
plot(covid$country,covid$cases,las=3) #etichette verticali (vertical labels)

plot(covid$country,covid$cases,las=3,cex.axis=0.3) #cex.axis comando per cambiare le dimensione delle etichette


#ggplot2
# richiamare la library ggplot2
library(ggplot2)
data(mpg)
head(mpg)

# data 
# aes
# tipo di geometria
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()
ggplot(mpg,aes(x=displ,y=hwy))+geom_line()
ggplot(mpg,aes(x=displ,y=hwy))+geom_polygon()


#ggplot di covid
ggplot(covid,aes(x=lon,y=lat,size=cases))+geom_point()


#richiamiamo la librery spatstat
library(spatstat)

#density
#create datased for spatstat
attach(covid)
covids<-ppp(lon,lat,c(-180,180),c(-90,90))   #>ppp(x.coordinates,y.coordinates,x.range,y.range)

d<-density(covids)

plot(d)
points(covids) 

plot(d)
#palette
cl<-colorRampPalette(c("yellow","orange","red"))(100)
plot(d,col=cl)

#EXERCISE: plot della mapa della densità dal verde al blu

points(covids)

library(rgdal)

coastlines<-readOGR("ne_10m_coastline.shp")
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
