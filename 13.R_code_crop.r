# settiamo la WD
# facciamo un crop sui dati di neve già usati in precedenza
# cartella snow come WD
setwd("C:/lab/snow")

# caricheremo dati da copernicus o qualsiasi dato di immagine satellitare

# Exercize: caricare tutte le immagini della cartella snow
library(raster)
rlist<-list.files(pattern="snow")

# la lista dei songoli file può essere importata con la funzione raster
list.rast<-lapply(rlist, raster)
list.rast

# funzione stack permette di impacchettare tutte le immagini in una unica
snow.multitemp <- stack(list.rast)

clb<-colorRampPalette(c('dark blue','blue','light blue'))(100)
# possiamo dunque plottare l'immagine
plot(snow.multitemp,col=clb)

# vediamo
snow.multitemp

plot(snow.multitemp$snow2010r, col=clb)
# notiamo che l'Italia si trova tra i 6 e i 20 gradi, e i 35 e i 50
# quindi impostiamo la nuova estenzione, per chiudere e oommare quindi l'Italia
extension <- c(6, 20, 35, 50)
zoom(snow.multitemp$snow2010r,ext=extension)

# se vogliamo fare uno zoom con la stessa ColorRampPalette usata per l'analisi basta aggiungere, in questo caso, col=clb
zoom(snow.multitemp$snow2010r,ext=extension,col=clb)

# rilanciamo il plot dell'immagine originale
plot(snow.multitemp$snow2010r, col=clb)

# possiamo definire l'estenzione tramite un disegno
zoom(snow.multitemp$snow2010r, ext=drawExtent())

# funzoone crop
# ritaglia una nuova immagne della zona definita
extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)
plot(snow2010r.italy, col=clb)

# in zoom dobbiamo specificare l'estenzione
# in crop basta mettere l'immagine e l'estenzione che vgliamo utilizzare per la nuova immagine

# possiamo applicare la funzione crop su tanti livelli con la funzione brick
# Esercizio, crop dell'estenzione dell'Italia dello stacj di tutto le immagini della copertura
extension <- c(6, 20, 35, 50)
snow.multitemp.Italy<-crop(snow.multitemp,extension)
plot(snow.multitemp.Italy,col=clb)

# mettiamo la legenda uguale per tutti considerando il mimino e il massimo
snow.multitemp.Italy
# minimo 20
# massimo 200
# aggiungimao il limite zlim=c(20,200)
plot(snow.multitemp.italy, col=clb, zlim=c(20,200))


# facciamo un'analisi con boxplot
boxplot(snow.multitemp.italy, horizontal=T,outline=F)
 
 

