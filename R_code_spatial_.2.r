# R spatial: funzioni spaziali in Ecologia del paesaggio

# richiamo il pacchetto "sp"
library(sp)


# ci interessa il dataset "meuse"
data("meuse")

head(meuse) #visualizzaimo i primi sei

#fissiamo il dataframe
attach(meuse)

#plot pre mettere in relazione cadmio e piombo
plot(cadmium,lead,main="Relazione cadmio piombo",col="blue",pch=19,cex=2,xlab="Cadmio",ylab="Piombo")

#cambiare le etichette
plot(cadmium,lead,col="blue",pch=19,cex=2,main="Relazione cadmio piombo",xlab="Cadmio",ylab="Piombo")


#exercise: plot del rame e zinco (copper e zinc) con carattere triangolo e colore verde
plot(copper,zinc,pch=17,col="green",cex=2,main="Relazione copper zinc",xlab="Copper",ylab="Zinc")
plot(cadmium,lead,col="blue",pch=19,cex=2)


#mostrare più di un grafico in una sola finestra con multiframe o multipanel
par(mfrow=c(1,2))
#mfrow=c(1,2) indica che creiamo una finestra con una riga e due colonne
plot(cadmium,lead,main="Relazione cadmio e piombo",col="blue",pch=19,xlab="Cadmio",ylab="Piombo")
plot(copper,zinc,main="Relazione tra Rame e Zinco",col="green",pch=17,xlab="Rame",ylab="Zinco")

#se fosse mfrow=c(2,1) si avrebbe una finestra con una colonna e due righe
par(mfrow=c(2,1))
plot(cadmium,lead,main="Relazione cadmio e piombo",col="blue",pch=19,xlab="Cadmio",ylab="Piombo")
plot(copper,zinc,main="Relazione tra Rame e Zinco",col="green",pch=17,xlab="Rame",ylab="Zinco")



#multiframe automatico
#installo il pacchetto "Ggally"
install.packages("GGally")
library(GGally)

#il comando ggpairs permette di avere un grafico con tutte le variabili
ggpairs(meuse)

#si crea un grafico con le variabili di interesse, quelle dalla terza alla sesta colonna [,3:6]
ggpairs(meuse[,3:6]) 


#Spatial!
head(meuse)

#dobbiamo specificare che i dati hanno delle coordinate
coordinates(meuse)=~x+y 

#mostrare il grafico
plot(meuse)

#funzioni ssplot per visualizare nel grafico i dati spazialmente
spplot(meuse,"zinc")
# il plot che è uscito è di tipo spaziale i punti gialli ci vanno ad indicare le zone più inquinate, stiamo analizzando le zone vicine ad un fiume.
