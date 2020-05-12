#telerilevamento parte due

#per riuscire ad usare le funzioni di telerilevamnento bisogna usare la libreria raster;
library(raster)

#settiamo la working directory
setwd("~/Desktop/lab")

#questa volta useremo un altra immagine per il nostro telerilevamento
#NB. abbiamo messo nella directory un set di dati che andremo ad usare durante tutto il nostro lavoro su R
#applichiamo la funzione brick per riuscire a rinominare e portare in R la nostra immagine di interesse

p224r63_1988 <- brick("p224r63_1988_masked.grd")

#ci creiamo un plot dell'oggetto del 1988
#il satellite lensat del 1988 vediamo le bande del blu, rosso, verde, e infrarosso.
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

plot(p224r63_1988)

#per vedere i nomi di interesse
names(p224r63_1988)

#per vedere i nostri grafici tutti assieme usiamo il multiframe
par(mfrow=c(2,2))

#blue
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_1988$B1_sre,col=clb)

#green
clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_1988$B2_sre,col=clg)

#red
clr <- colorRampPalette(c("red","orange","yellow"))(100)
plot(p224r63_1988$B3_sre,col=clr)

#infrared
clir <- colorRampPalette(c("purple","pink","light pink"))(100)
plot(p224r63_1988$B4_sre,col=clir)

#per chiudere la finestra attuale useremo:

dev.off()

#creiamo un plot RGB in True colours o natural

plotRGB(p224r63_1988,r=3,g=2,b=1,stretch="Lin")

#essendo il grafico non comprensibile useremo la scala cromatica 

plotRGB(p224r63_1988,r=3,g=2,b=1,stretch="Lin")

#exercise: creiamo il plot con la componente infrarosso nella componente rossa;
#in questo caso i colori vengono definiti False colours;

plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")

#vediamo come è cambiata nel corso del tempo la nostra immagine mettendo a confronto il 2011 con il 1988;

par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")

dev.off()
#l'agricolo è molto più sviluppata nel 2011
#infrarosso vicino sono le piante
#le zolle di terra sono bianche o celeste

#possiamo calcolare l'indice di salute della vegetazione, ricordiamo che è possibile in quanto le foglie sane riescono a riflettere l'infrarosso 
#DVI(Difference Vegetation Index) sarà l'indice che useremo noi; 

#DVI=NIR-RED, avremo dei risultati diversi in base alla salute delle piante; 
#sana = NIR alto
#Malata = RED alto 

#div1988=nir1988-red1988
#per legare si usa il simbolo $

dvi1988 <- p224r63_1988$B4_sre-p224r63_1988$B3_sre

#vediamo il plot del DVI
plot(dvi1988)

#facciamo lo stesso per quello del 2011

dvi2011 <- p224r63_2011$B4_sre-p224r63_2011$B3_sre
plot(dvi2011)

par(mfrow=c(2,1))
 #Per cambiare colore si userà il comando color ramp palette
cldvi11 <- colorRampPalette(c('dark green','green','light green'))(100)
plot(dvi2011,col=cldvi11)

#adesso abbiamo i DVI dei 2 anni, se facciamo la differenza tra i 2 anni vedremo quanto è stato il cambiamento della vegetazione, se il valore del 2011 è negativo vuol dire che la vegetazione stava meglio nel 1988
#multitemporal analysis
dfdvi <- dvi2011-dvi1988


# creiamo l' immagine che ci mostra le zone dove le piante sono state in maggiore stress
plot(dfdvi)
cldfdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(dfdvi,col=cldfdvi)

#visualizzare tutti i grafici assieme usando un multiframe di immagine di 1988. 2011 e differenza di indice 
par(mfrow=c(3,1))

plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")
plot(dfdvi,col=cldfdvi)

dev.off()

#cambiare la risoluzione di un immagine, la funione che si usa è aggregate
p224r63_2011lr <- aggregate(p224r63_2011,fact=10)
# se fact o factor viene settato uguale a 10 usiamo una scala 10 volte maggiore 

#vediamo le caratteriestiche dell'immagine originale
p224r63_2011

#vediamo le caratteristiche della nuova immagine
p224r63_2011lr

#mettiamo i 2 grafici a confronto;
par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011lr,r=4,g=3,b=2,stretch="Lin")
dev.off()

#cambiamo il fattore in 50
p224r63_2011lr2 <- aggregate(p224r63_2011,fact=50)
#visualizziamo le informazioni dell'immagine
p224r63_2011lr2

#creiamo il nostro grafico mettendoli sempre in correlazione;
par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011lr2,r=4,g=3,b=2,stretch="Lin")
dev.off()

#facciamo un dvi della nuova immagine 2011
dvi2011lr50 <- p224r63_2011lr2$B4_sre - p224r63_2011lr2$B3_sre
plot(dvi2011lr50)

#diminuiamo la risoluzione del 1988
p224r63_1988lr2 <- aggregate(p224r63_1988,fact=50)

#facciamo un dvi della nuava immagine 1988
dvi1988lr50 <- p224r63_1988lr2$B4_sre - p224r63_1988lr2$B3_sre
plot(dvi1988lr50)

#facciamo la differenza dei DVI dei due anni a bassa risoluzione
dfdvilr <- dvi2011lr50-dvi1988lr50

#creiamo la nostra immagine:
plot(dfdvilr,col=cldfdvi)

#multiframe del totale 
par(mfrow=c(2,1))
plot(dfdvi,col=cldfdvi)
plot(dfdvilr,col=cldfdvi)
dev.off()

#é importante usare la scala giusta per evitare di non riuscire a distinguere le microdivresità presente nel grafico;

