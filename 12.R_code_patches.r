# Settiamo innanzitutto la WD
setwd("C:/lab")

# richiamiamo la libreria raster
library(raster)

# carichiamo le immagini raster, la mappa classificata in questo caso, con la funzione raster
d1c<-raster("d1c.tif")
d2c<-raster("d2c.tif")

# facciamo un plot per vedere chi è la foresta e chi è la parte agricola
# con una colorRampPalette con due colori
cl<-colorRampPalette(c('green','black'))(100)
par(mfrow=c(1,2))
plot(d1c,col=cl)
plot(d2c,col=cl)
# mappa così sbagliata perchè la foresta corrisponde al colore due anzichè al colore uno, bisogna quindi invertire i colori
cl<-colorRampPalette(c('black','green'))(100)
par(mfrow=c(1,2))
plot(d1c,col=cl)
plot(d2c,col=cl)

dev.off()

# land cover 1= agriculture, land cover 2=forest

# funzione per annullare alcuni valori è la funzione cbind
# vogliamo eliminare tutti i valori corrisponndenti all'agrucoltura e così estraiamo solo i valori della foresta
# reclassify = riclassifichiamo i dati anullando i valori della classe della agricoltura associandogli il valore NA
d1c.for <- reclassify(d1c, cbind(1, NA))

# abbiamo quindi una nuova mappa e possiamo rifare il par mettendola a confronto le due situazioni
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for)
# abbiamo reso nullo il valore 1, abbiamo solo la classe delle foreste che ha valore due
# possiamo applicare la colorRampPalette anche alla senìconsa mappa
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for,col=cl)

# abbiamo riclassificato quindi la prima mappa e possiamo procedere per la seconda
# teniamo solo le foreste
d2c.for<-reclassify(d2c,cbind(1,NA))

# possiamo fare un plot di entrambe le mappe classificate solo con la foresta
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)

# scarichaimo il pacchetto igraph che dovrebbe essere incluso in raster
install.packages("igraph")
library(igraph)

# creiamo patches, uniamo e raggruppiamo tutti i pixel vicino per creare ogni patch
d1c.for.patches<-clump(d1c.for)
d2c.for.patches<-clump(d2c.for)

# abbiamo creato le nostre mappe con i patches
# con la funzione writerRster esportiamo il file delle due mappe in formato ".tif" direttamente all'interno della cartella lab
writeRaster(d1c.for.patches,"d1c.for.patches.tif")
writeRaster(d2c.for.patches,"d2c.for.patches.tif")

# raster (o brick) importa i file, writeRaster esporta i file

# Exercize: plottare entrambe le mappe una accanto all'altra
par(mfrow=c(1,2))
clp <- colorRampPalette(c('darkblue','blue','green','orange','yellow','red'))(100) # mettiamo quanti più colori possibili per vedere tutti i singoli patch di foresta
plot(d1c.for.patches,col=clp)
plot(d2c.for.patches,col=clp)

# qauntifichiamo il numero di patches che sono stati creati nelle mappe
d1c.for.patches 
# vediamo che nel valore patches max erano 301

d2c.for.patches
# vediamo che nel valore patches max erano 1212


# risultati dei plot
# time riferito a prima della deforestazione e dopo
time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)

# creiamo il dataframe 'output'
output <- data.frame(time,npatches)

# facciamo il plot finale con ggplot
# richiamiamo la libreria ggplot2
library(ggplot2)
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

