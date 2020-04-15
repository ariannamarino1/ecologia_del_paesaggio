# PRIMO CODICE R ECOLOGIA DEL PAESAGGIO

#istallare il pacchetto "sp" per la nuova libreria
istall.packages("sp")
library(sp)
#require(sp) è un altro comando per far partire le librerie

#con data vengono richiamati i dati contenuti nella libreria
data(meuse)
meuse #visualizzare i dati

#
head(meuse)
#
names(meuse)

#con summary si visualizzano i principali indici statistici dei dati in considerazione
summary(mause)

#creazione di un grafico 
paris(mouse)

#
pairs(~ cadmium + copper + lead , data = meuse)

# Exercize: cadmium cooper lead zinc

pairs(~ cadmium + copper + lead + zinc , data = meuse)

#[3:6] significa che vengono presi i dati relativi alla riga 3, 4, 5 e 6 che corrispondono a cadmium, copper, lead e zinc
pairs(meuse[,3:6])

#per cambiare il colore di visualizzazione nel grafico si usa col="colore"
pairs(meuse[,3:6],col="blue")

#con pch=numero si sceglie la forma dei punti
pairs(meuse[,3:6],col="blue",pch=18)

#con cex=numero si ridimensione la grandezza del punti nel grafico
pairs(meuse[,3:6],col="blue",pch=18,cex=3)

#con main="..." si titola il grafico
pairs(meuse[,3:6],col="blue",pch=18,cex=3,main="Primo pairs")

#prendiamo delle funzioni esterne;
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  
  
  
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}



panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                             cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}




panel.histograms <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}


#mandate le tre funzioni esterne si avrà la possibilità di creare grafici diversi nello stesso grafico rappresentanti situazioni diverse
pairs(meuse[,3:6],lower.panel=panel.correlations,upper.panel=panel.smoothing,diag.panel=panel.histograms)

# EXERCISE: mettere come lower panel lo smoothing, come diagonal panel gli istogrammi e come upper panel le correlazioni

pairs(meuse[,3:6],lower.panel=panel.smoothing,upper.panel=panel.correlations,diag.panel=panel.histograms)



#riprendiamo il set di dati "meuse"
library(sp)
data("meuse")
head(meuse)

# funzione plot per creazione di un grafico
plot(meuse$cadmium,meuse$copper)
#il $ indica quale colonna del dataset si vuole prendere

#mettere il evidenza il pacchetto di dati
attach(meuse)

#evidenziando il dataset meuse possiamo eviare di riscrivere meuse$... e scrivere solo i nomi delle colonne 

plot(cadmium,copper)

plot(cadmium,copper,pch=17,col="green",main="Primo plot")

plot(cadmium,copper,pch=17,col="green",main="Primo plot",xlab="cadmio",ylab="copper")
