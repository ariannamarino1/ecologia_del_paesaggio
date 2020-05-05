# Lezione 29 Aprile
# R code analisi multitemporale di variazione della land cover

# settiamo la WD
setwd("C:/lab")

# richiamiamo la libreria raster
library(raster)

# brick() è una funzione di raster che permette di caricare dei dati dall'esterno, caricando tutte le singole bande se si stratta di una immagine satellitare
defor1<-brick("defor1_.jpg")
defor2<-brick("defor2_.jpg")

defor1 # nel dataet abbiamo 3 bande
#names: defor1_.1, defor1_.2, defor1_.3 
# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green

# plottaggio RGB, associamo ogni singola banda ad una componente rgb
# banda del rosso alla componente NIR, banda del green alla componente red, banda del blu alla componente green

plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")

# Exercize plot della seconda data
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

#plot delle due immagini, confronto della foreste pluviale in due momenti diversi, prima e dopo la deforestazione
par(mfrow=c(2,1))
plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

dev.off()

# per la classificazione si usa unsuperClass che significa classificazione non supervisionata, cioè non diamo dei training set al pc
# per far ciò dobbiamo caricare un'atra libreria
library(RStoolbox)
d1c<-unsuperClass(defor1,nClasses=2)
# in d1c abbiamo la mappa
d1c$map
# facciamo il plot
plot(d1c$map)

cl1<-colorRampPalette(c('green','blue'))(100)
plot(d1c$map,col=cl1)

# Excersize: classificare con due classi l'immagine satellitare defor2
# consideriamo adesso la seconda immagine
d2c<-unsuperClass(defor2,nClasses=2)
# in d1c abbiamo la mappa
d2c$map
# facciamo il plot
plot(d2c$map)

cl2<-colorRampPalette(c('green','blue'))(100)
plot(d2c$map,col=cl2)

# mettiamo a confronte le due immagini
par(mfrow=c(2,1))
plot(d1c$map,col=cl1)
plot(d2c$map,col=cl2)

par(mfrow=c(1,2))
plot(d1c$map, col=cl1)
plot(d2c$map, col=cl2)

dev.off()

###########
# classificazione con tre classi l'immagine satellitare defor2
# consideriamo adesso la seconda immagine
d2c<-unsuperClass(defor2,nClasses=3)
# in d1c abbiamo la mappa
d2c$map
# facciamo il plot
plot(d2c$map)
cl3<-colorRampPalette(c('orange','green','blue'))(100)
plot(d2c$map,col=cl3)
###########


# quatificare la quantità di foresta che è stata persa
# area aperta = 306059
# foresta = 35233

# calcoliamo dapprima il totale
totd1<- 306059 + 35233
# totd1 = 341292
# possiamo calcolare la percentuale
percent1<-freq(d1c$map)*100/totd1
# percentuali
# foreste = 89.7
# aree aperte = 10.3

# per il defor2
freq(d2c$map)
# foreste = 179087
# aree aperte = 163639

totd2<-179087 + 163639
# totd2 = 342726
# percentuale
percent2<-freq(d2c$map)*100/totd2
# foreste = 52.2
# aree aperte = 47.8


# creare un nuovo dataset con i dati ricavati
cover <- c("Agriculture","Forest")
before<-c(10.3,89.7)
after<-c(47.8,52.2)
output <- data.frame(cover,before,after)
View(output)


#### DAY 2 ###
# riapriamo il file dell'utlima lezione
setwd("C:/lab")
load("multitemp.RData")
ls()

# rivediamo le immagini a confronto della foresta prima e dopo il disboscamento
library(raster)
par(mfrow=c(2,1))
plot(d1c$map,col=cl1)
plot(d2c$map,col=cl2)
dev.off()

# controliamo il dataframe contenete l'agricultura e la foresta prima e dopo il disboscamento
output


# possiamo quindi fare un plot della percentuale di foresta attuale e precedente

# plot della percentuale precedente
# carichiamo la libreria ggplot2
library(ggplot2)
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover))+geom_bar(stat="identity",fill="white")
grafico1
# faremo degli istogrammi del dataframe di output
# aes: sulla x agricoltura e foresta, sulla y la percentuale di copertura prima della deforestazione
# colore si baserà sulla cover (agricoltura e foresta)
# stat sono le statistiche che considera, in questo caso le identità
# fill dà il colore alla copertura

## Exercize: plot the histograms of the land cover after deforestation
grafico22<-ggplot(output,aes(x=cover,y=after,color=cover))+geom_bar(stat="identity",fill="white")
grafico2

# possiamo fare un plot dei due istogrammi per confrontarli
# dobbiamo però usare un'altra libreria, la gridExtra 
install.packages("gridExtra")
library(gridExtra)

# possiamo quindi procedere a fare un plot con la funzione grid.arrange. La funzione prende vari plot e li mette insieme all'interno di uno stesso grafico (funzione simile a par)
grid.arrange(grafico1,grafico2,nrow=1)
# la percentuale di agricoltura relativa a prima della deforestazione sale vertiginosamente fino a raggiungere quasi il livello della foresta dopo la deforestazione


