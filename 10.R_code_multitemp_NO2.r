# codice per analisi dei dati NO2 da ESA - gennaio a marzo 2020

# useremo le seguenti librerie
library(raster)



# settiamo la WD
setwd("C:/lab")

# dobbiamo importare le singole immagini con la funzione raster
EN01<-raster("EN_0001.png")
EN02<-raster("EN_0002.png")
EN03<-raster("EN_0003.png")
EN04<-raster("EN_0004.png")
EN05<-raster("EN_0005.png")
EN06<-raster("EN_0006.png")
EN07<-raster("EN_0007.png")
EN08<-raster("EN_0008.png")
EN09<-raster("EN_0009.png")
EN10<-raster("EN_0010.png")
EN11<-raster("EN_0011.png")
EN12<-raster("EN_0012.png")
EN13<-raster("EN_0013.png")


# plottiamo la prima immagine e l'ultima, creando una color palette
library(raster)
cl<-colorRampPalette(c('red','orange','yellow'))(100)
plot(EN01,col=cl)
plot(EN13,col=cl)

par(mfrow=c(1,2))
plot(EN01,col=cl)
plot(EN13,col=cl)
dev.off()

# difference
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) #
plot(difno2, col=cldif)

# plottiamo tutte le immagini
par(mfrow=c(4,4))
plot(EN01,col=cl)
plot(EN02,col=cl)
plot(EN03,col=cl)
plot(EN04,col=cl)
plot(EN05,col=cl)
plot(EN06,col=cl)
plot(EN07,col=cl)
plot(EN08,col=cl)
plot(EN09,col=cl)
plot(EN10,col=cl)
plot(EN11,col=cl)
plot(EN12,col=cl)
plot(EN13,col=cl)

dev.off()


### DAY 2 ###

setwd("C:/lab")
load("EN.RData")
ls()

# per importare più immagini insieme
library(raster)
# bisogna creare una cartella contenente tutti le immagini EN e selezionare questa cartella come nuova WD
setwd("C:/lab/esa_no2")

# creiamo una rlist contenente la lista di file .png
rlist<-list.files(pattern=".png")
rlist #visualiziamo il contenuto del nuovo data creato

#save raster into list
# lapply applica una funzione su una lista o un vettore (serie di elementi). 
# In questo caso la funzione che vogliamo applicare per caricare i singoli dati sono brick (immagine satellitare con tutti i sensori) e raster (immagine con un solo sensore)
# applichiamo alla listra rlis la funzione raster
listafinale<-lapply(rlist, raster)

listafinale 
# visualizziamo i RasterLayer 

# funzione stack permette di impacchettare tutte le immagini in una unica
EN <- stack(listafinale)

cl<-colorRampPalette(c('red','orange','yellow'))(100)
# possiamo dunque plottare l'immagine
plot(EN,col=cl)


####

library(raster)
setwd("C:/lab/esa_no2")

rlist
listafinale<-lapply(rlist,raster)
listafinale
EN<-stack(listafinale)

# una volta caricate le immagini creiamo una differenza tra l'ultima immagine (EN 13 corrispondente a Marzo) e la prima immagine (EN 01 corrispondente a Gennaio) 
difEN<- EN$EN_0013-EN$EN_0001

# ci definizamo una colorRampPalette
cld<-colorRampPalette(c('blue','white','red'))(100)

# e plottiamo la differnza delle immagini con il colore creato così da visualizzare le differenze tra Marzo e Gennaio
plot(difEN,col=cld)

# boxplot
boxplot(EN,horizontal=T,outiline=F,axes=T)
# horizontal=T -> disposizione orizzontale
# outline=F -> senza linee esterne
# axes=T di defoult, mette gli assi (il contrario axes=F li toglie)




