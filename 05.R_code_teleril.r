#telerilevamento


#istallare librerie che ci serviranno per lavorare con i nostri dati

install.packages("raster")

install.packages("RStoolbox")

#richiamare la libreria raster
library(raster)

#scaricare un pacchetto di dati e rinominare un file che ci servirà per il nostro lavoro
p224r63_2011 <- brick("p224r63_2011_masked.grd")

#creare il grafico dell'immagine satellitare
plot(p224r63_2011)

# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

#per cambiare la scala cromatica
cl <- colorRampPalette(c("black","grey","light grey"))(100) 

#con cl abbiamo chiamato il comando precedente che dobbiamo aggiungere al grafico
plot(p224r63_2011,col=cl)

#modifica delle scale cormatiche
cllow <- colorRampPalette(c("black","grey","light grey"))(5) 

#inserirle nel grafico
plot(p224r63_2011,col=cllow)

names(p224r63_2011)
#[1] "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"

#cambiamo in scala di blu la banda blu
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)

#attach data frame non funziona con raster 
#visualizzare solo la banda del blu all'interno del grafico
plot(p224r63_2011$B1_sre,col=clb)

#visualizzare solo la banda verde nel grafico
clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_2011$B2_sre,col=clg)

#exercise: fai infrarosso vicino con la scala cromatica da rosso a giallo 
clir <- colorRampPalette(c("purple","pink","light pink"))(100)
plot(p224r63_2011$B4_sre,col=clir)

#grafico multiframe in cui visualizzare le quattro diverse bande insieme
par(mfrow=c(2,2))

#blue
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_2011$B1_sre,col=clb)

#green
clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_2011$B2_sre,col=clg)

#red
clr <- colorRampPalette(c("red","orange","yellow"))(100)
plot(p224r63_2011$B3_sre,col=clr)

#infrared
clir <- colorRampPalette(c("purple","pink","light pink"))(100)
plot(p224r63_2011$B4_sre,col=clir)

#chiusura delle immagini plottate
dev.off()

#colori naturali, 3 componenti R G B, creiamo un plot con questa gamma di colore 
# 3 bande alla volta R= red G= green B= blue
#comando
plotRGB(p224r63_2011,r=3,g=2,b=1)

#essendo il risultato nero, usiamo la funzione stretch="lin"
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="lin")

#per visualizzare meglio il nostro grafico risultante cambiamo le bande inserendo l'infrarosso 
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="lin")

#essendo le piante riflettenti assumeranno il colore rosso
#il celeste indicherà le zone agricole non coltivata
#il rosa le zone agricole coltivate

#visualizzare i 2 grafici ottenuti usando la funzione multiframe
par(mfrow=c(2,1))

plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="lin")


#per salvare il grafico  in pdf
pdf("telerilevamento-1.pdf")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="lin")
dev.off()


par(mfrow=c(2,1))

plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="lin")
dev.off()


# exercise: infrarosso lo montiamo in alre componenti green
plotRGB(p224r63_2011,r=3,g=4,b=2, stretch="lin")

#infrarosso nella componente blu:
plotRGB(p224r63_2011,r=3,g=2,b=4, stretch="lin")
