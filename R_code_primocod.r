# PRIMO CODICE R ECOLOGIA DEL PAESAGGIO

istall.packages("sp")
library(sp)
#require(sp) è un altro comando per far partire le librerie

data(meuse)
meuse

head(meuse)
names(meuse)

summary(mause)

paris(mouse)

pairs(~ cadmium + copper + lead , data = meuse)

# Exercize: cadmium cooper lead zinc

pairs(~ cadmium + copper + lead + zinc , data = meuse)

pairs(meuse[,3:6],col="blue")

pairs(meuse[,3:6],col="blue",pch=18)

pairs(meuse[,3:6],col="blue",pch=18,cex=3)

pairs(meuse[,3:6],col="blue",pch=18,cex=3,main="Primo pairs")

pairs(meuse[,3:6],lower.panel=panel.correlations,upper.panel=panel.smoothing,diag.panel=panel.histograms)

# EXERCISE: mettere come lower panel lo smoothing, come diagonal panel gli istogrammi e come upper panel le correlazioni

pairs(meuse[,3:6],lower.panel=panel.smoothing,upper.panel=panel.correlations,diag.panel=panel.histograms)

## funzione plot

plot(meuse$cadmium,meuse$copper)

attach(meuse)

plot(cadmium,copper)

plot(cadmium,copper,pch=17,col="green",main="Primo plot")

plot(cadmium,copper,pch=17,col="green",main="Primo plot",xlab="cadmio",ylab="copper")
