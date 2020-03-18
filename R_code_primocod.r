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
