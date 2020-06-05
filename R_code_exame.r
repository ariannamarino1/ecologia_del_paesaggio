# R_code_exame.r

# 1. R_code_first.r   
# 2. R_code_spatial.r   
# 3. R_code_spatial2.r
# 4. R_code_point_pattern   
# 5. R_code_teleril.r   
# 6. R_code_landcover.r   
# 7. R_code_multitemp.r   
# 8. R_code_multitemp_NO2.r   
# 9. R_code_snow.r   
# 10. R_code_patches.r   
# 11. R_code_crop.r

# Dati Copernicus: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html


##################################################
##################################################
##################################################

### 1. R code first

# PRIMO CODICE R ECOLOGIA DEL PAESAGGIO

# AM: librerie
library(sp) 
# AM: libreria sp è utile per lavorare sulle classi e i metodi dei dati spaziali


# AM: bisogna installare il pacchetto di dati "sp" e successivamente richiamarlo con la libreria
istall.packages("sp")
library(sp)
#AM require(sp) è un altro comando per far partire le librerie

# AM: con data vengono richiamati i dati contenuti e disponibili nella libreria
# AM: "meuse" è un set di dati sulle posizioni e concentrazioni di metalli pesanti nel terreno, con una serie di variabili del suolo e del paesaggio nei punti di psservazione, raccolte nel fiume Mose in un paese del Lussemburgo.
data(meuse)
# AM: per visualizzare i dati contenuti in meuse basta mandare il comando
meuse 

# AM: con la funzione head() è possibile vedere le prime sei righe del dataset
head(meuse)
# AM: la funzione names() ci permette di visualizzare i nomi delle colonne del dataset
names(meuse)

# AM: la funzione summary() è una funzione generica utilizzata per fornire riepiloghi dei risultati delle varie funzioni di adattamento del modello
summary(meuse)

# AM: la funzione pairs() produce una matrice di scatterplots 
pairs(meuse)

# AM: selezionando le variabili dal dataset si ottiene una matrice di scatterplots relativa alle variabili selezionate
pairs(~ cadmium + copper + lead , data = meuse)

# EXERCISE: cadmium cooper lead zinc
pairs(~ cadmium + copper + lead + zinc , data = meuse)


# AM: riprendiamo il set di dati "meuse"
library(sp)
data("meuse")
meuse
head(meuse)

# AM: si uta la funzione plot() per la creazione di un grafico
plot(meuse$cadmium,meuse$copper)
# AM: il $ seguito dal nome di una colonna sta ad indicare quale colonna del dataset si decide di prendere in analisi

# AM: se si deve lavorare con dati all'interno del dataset "meuse" basta usare la funzione attach() che collega il dataset alle successive funzioni
attach(meuse)

# AM: una volta legato il dataset meuse alle funzioni si può evitare di usare il "meuse$..." per selezionare le colonne. Basta a questo punto scrivere solo il nome delle colonne da esaminare
plot(cadmium,copper)

# AM: in un grafico è possibile modificare il colore e/o il simbolo, è possibile mettere un titolo al grafico, titolare le ascisse e le ordinate
# AM: pch=n , dove 'n' rappresenta un numero intero, da 1 a 25. Ad ogni numero corrisponde un simbolo
# AM: col="colore" , dove 'colore' deve essere sostituito dal nome inglese del colore che si vuole utilizzare. In alternativa al nome del colore si può usare un numero [1;657].
# AM: main="..." è il comand usato per dare il titolo al grafico.
plot(cadmium,copper,pch=17,col="green",main="Primo plot",xlab="cadmio",ylab="copper")

# AM: quando si vogliono prendere dei dati relativi a determinate righe si può usare la [..:..]. 
# AM: Nel caso [3:6] significa che vengono presi i dati relativi alla riga 3, 4, 5 e 6 che corrispondono a cadmium, copper, lead e zinc
pairs(meuse[,3:6])

# AM: cambio colore col="blue"
pairs(meuse[,3:6],col="blue")

# AM: cambio simbolo pch=18 (18= rombo)
pairs(meuse[,3:6],col="blue",pch=18)

# AM: se si vuole ridimensionare la grandezza del punti nel grafico si usa cex=n, dove 'n' viene sostituito con il numero
pairs(meuse[,3:6],col="blue",pch=18,cex=0.5)

# AM: titolare il grafico con main="..."
pairs(meuse[,3:6],col="blue",pch=18,cex=0.5,main="Primo pairs")

# AM: prendiamo delle funzioni esterne
# AM: panel.correlation è una funzione definita che indica il pannello del coefficiente di correlazione per la coppia di funzioni
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

# AM: panel.smoothing è una funzione di esempio di una semplice funzione utile del pannello da usare come argomento in pairs
panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                             cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}

# AM: panel.histograms è una funzione relativa alla realizzazione di un pannello del grafico che indicano un istogramma e una curva di densità
panel.histograms <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}


# AM: una volta inviate le tre funzioni esterne si avrà la possibilità di diversificare i grafici all'interno dello stesso grafico a rappresentanza di situazioni diverse
# AM: lower.panel si riferisce ai pannelli al di sotto dei pannelli diagonali
# AM: upper.panel si riferisce ai pannelli al di dopra dei pannelli diagonali
# AM: diag.panel si riferisce ai pannelli diagonali
pairs(meuse[,3:6],lower.panel=panel.correlations,upper.panel=panel.smoothing,diag.panel=panel.histograms)

# EXERCISE: mettere come lower panel lo smoothing, come diagonal panel gli istogrammi e come upper panel le correlazioni
pairs(meuse[,3:6],lower.panel=panel.smoothing,upper.panel=panel.correlations,diag.panel=panel.histograms)



##################################################
##################################################
##################################################



### 2. R code spatial
# R spatial: funzioni spaziali in Ecologia del paesaggio

# AM: librerie utili per il codice
library(sp)
library(GGally)

# AM: richiamare il pacchetto "sp" con il comando:
library(sp)

# AM: si prende in esame il dataset "meuse"
data("meuse")

# AM: visualizzare le prime sei righe
head(meuse) 

# AM: dato che verrà usato questo dataset lo fissiamo 
attach(meuse)

# AM: mettere in relazione cadmio e piombo attraverso il plot, specificando titolo, colore, simbolo, ascisse e ordinate
plot(cadmium,lead,main="Relazione cadmio piombo",col="blue",pch=19,cex=2,xlab="Cadmio",ylab="Piombo")


# EXERCIZE: plot del rame e zinco (copper e zinc) con simbolo triangolo (17) e colore verde
plot(copper,zinc,pch=17,col="green",cex=2,main="Relazione copper zinc")

# AM: mettere le etichette relative alle ascisse e alle ordinate
plot(copper,zinc,pch=17,col="green",cex=2,main="Relazione copper zinc",xlab="Copper",ylab="Zinc")

# EXERCISE: mettere in relazione cadmium e lead
plot(cadmium,lead,col="blue",pch=19,cex=2)


# AM: è possibile mostrare più grafici in una sola finestra con multiframe o multipanel, specificando il numero di righe e di colonne
# AM: par(mfrow) è la funzione che mermette di gestire l'aspetto dei grafici, per creare un semplice diagramma a più riquadri
# AM: mfrow=c(1,2) indica che creiamo una finestra con una riga e due colonne
par(mfrow=c(1,2))
plot(cadmium,lead,main="Relazione cadmio e piombo",col="blue",pch=19,cex=2,xlab="Cadmio",ylab="Piombo")
plot(copper,zinc,main="Relazione tra Rame e Zinco",col="green",pch=17,cex=2,xlab="Rame",ylab="Zinco")

# AM: se fosse mfrow=c(2,1) si avrebbe una finestra con una colonna e due righe
par(mfrow=c(2,1))
plot(cadmium,lead,main="Relazione cadmio e piombo",col="blue",pch=19,cex=2,xlab="Cadmio",ylab="Piombo")
plot(copper,zinc,main="Relazione tra Rame e Zinco",col="green",pch=17,cex=2,xlab="Rame",ylab="Zinco")


# AM: multiframe automatico
# AM: installare il pacchetto "Ggally". (Bisogna prestare attenzione alla scrittura con le lettere maiuscore perchè R è case sensitive). Queso pacchetto estende 'ggplot2' aggiungendo diverse funzioni per ridurre la complessità della combinazione di oggetti geometrici con dati trasformato.
install.packages("GGally")
library(GGally)

library(sp)
data(meuse)

# AM: il comando ggpairs crea una matrice di grafici con un determinato set di dati
ggpairs(meuse)

# AM: creare un grafico con le variabili di interesse, quelle dalla terza alla sesta colonna [,3:6] del datase meuse
ggpairs(meuse[,3:6]) 


# Spatial!
head(meuse)

# AM: bisogna specificare che i dati hanno delle coordinate e questo viene fatto usando il comando coordinates() che recupera le coordinate spaziali di un oggetto spaziale
coordinates(meuse)=~x+y 

# AM: si può dunque mostrare il grafico
plot(meuse)

# AM: la funzione sp plot traccia spazialmente i dati nel grafico 
spplot(meuse,"zinc")
# AM: dal grafico spaziale risultante notiamo che ci sono punti di colori diversi (dal giallo al nero). 
# AM: il giallo indica le zone più inquinate dell'area vicino al fiume Mose.




##################################################
##################################################
##################################################


### 3. R code spatial.2

#SPATIAL 2

# AM: librerie usate
library(sp)


# AM: richiamare la libreria "sp"
library(sp)

# AM: selezionate il dataset di interesse
data(meuse)

# AM: leggere le prime sei righe
head(meuse)

# AM: fissare il dataframe in uso 
attach(meuse)

# AM: specificare le coordinate del dataset
coordinates(meuse)=~x+y

# AM: creare un grafico sp plot con i dati dello zinco
spplot(meuse,"zinc")

# EXERCIZE: sp plot dei dati di rame
head(meuse)
# AM: un altro comando che permette di visualizzare i nomi delle colonne è names()
names(meuse) 
spplot(meuse,"copper")

# AM: il comando bubble crea un grafico a bolle di dati spaziali, con opzioni per i grafici residui bicolori
bubble(meuse,"zinc")

# EXERCIZE: grafico bubble del rame, colorato di rosso
bubble(meuse,"copper",col="red",main="Indice spaziale Rame")


# AM: creare nuovo array con nuovi dati rappresentanti foraminiferi e carbon capture
foram<-c(10,20,35,55,67,80)
carbon<-c(5,10,30,70,85,99)

# AM: visualizzare i nuovi arrays in un grafico
plot(foram,carbon,col="green",cex=2,pch=19)


# AM: analisi dei dati provenienti dall'esterno sul covid-19
# AM: essendo che i dati da analizzare cono all'interno della cartella 'lab', selezionare la working directory
setwd("C:/lab")

# AM: leggere un file in formato di tabella e creare un nuovo dataframe da questo
# AM: la funzione head=TRUE indica che quando si importa la tabella, bisogna considerare la prima riga come una stringa di testo
covid<-read.table("covid_agg.csv",head=TRUE)



##################################################
##################################################
##################################################


### 4. R codde point patterns


# Codice per analisi dei point patterns (pattern legati ai punti)

# AM: librerie usate
library(ggplot2)
library(spatstat)
library(rgdal)


# AM: impostare la working directory della cartella lab
setwd("C:/lab")  

# AM: importare la tabella dei dati relativi al covid
covid<-read.table("covid_agg.csv",header=TRUE)
# AM: per vedere la tabella si usa head() 
head(covid)

# AM: creare un grafico per visualizzare i dati delli paesi in relazione ai dati dei casi
plot(covid$country,covid$cases)

#AM: metodo alternativo tramite fissazione del dataset
attach(covid)
plot(country,cases)

# AM: disposizione delle etichette, las=n , dove n è un numero da 1 a 4

# AM: las=0 etichette parallele all'asse x e y (parallel labels)
plot(covid$country,covid$cases,las=0) 

# AM: las=1 etichette orizzontali (horizontal labels)
plot(covid$country,covid$cases,las=1)

# AM: las=2 etichette perpendicolari all'asse x e y (perpendicular labels)
plot(covid$country,covid$cases,las=2)

# AM: las=3 etichette verticali (vertical labels)
plot(covid$country,covid$cases,las=3)

# AM: per cambiare le dimensioni delle etichette degli assi si usa il comando cex.axis
plot(covid$country,covid$cases,las=3,cex.axis=0.3)


# AM: installare la libreria ggplot2 e richiamarla
# AM: ggplot2 è un sistema di creazione grafica basato sulla 'grammatica della grafica'. ggplot2 mappa le variabili per l'estetica e cura i dettagli
install.packages(ggplot2)
library(ggplot2)

# AM: si prende il dataframe 'mpg' dalla libreria ggplot2, contenente osservazioni raccolte dalla US Environmental Protection Agency su 38 modelli di auto.
data(mpg)

# AM: si mostrano le prime sei righe
head(mpg)

# AM: creare un plot del nuovo dataset mpg, modificare l'estetica (aes) e il tipo di geometria (geom_)
# AM: aes() è una funzione di quotazione. Ciò signiica che i suoi imput sono quotati per essere valutati nel contesto dei dati.

# AM: per visualizzare dei punti si usa geom_point 
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()

# AM: per visualizzare delle linee si usa geom_line 
ggplot(mpg,aes(x=displ,y=hwy))+geom_line()

# AM: per visualizzare dei poligoni si usa geom_polygon 
ggplot(mpg,aes(x=displ,y=hwy))+geom_polygon()


# AM: si applica la libreria ggplot2 ai dati relativi al covid e si ottine un grafco gg plot
ggplot(covid,aes(x=lon,y=lat,size=cases))+geom_point()


# AM: installare e richiamare la libreria spatstat
# AM: la libreria 'spatstat' mostra anlisi dei modelli dei punti spaziali, modellazione, simulazione e test. Contiene oltre 2000 funzioni per tracciare i dati spaziali, analisi esplorativa dei dati, modellazione, simulazione, campionamento spaziale, diagnostica dei modelli e inferenza formale.
install.packages(spatstat)
library(spatstat)
attach(covid)

# AM: il comando ppp crea un oggetto di classe 'ppp' che rappresenta un insieme di dati del pattern puntiforme nel piano bidimensionale
# AM: ppp(x.coordinates,y.coordinates,x.range,y.range), bisogna specificare cosa rappresentano le x e le y e definirle in un range
covids<-ppp(lon,lat,c(-180,180),c(-90,90))   

# AM: calcolare la densità con il comando density()
d<-density(covids)

# AM: una volta assegnato un nome alla dentità (d), si può procedere alla sua rappresentazione grafica
plot(d)

# AM: aggiungere i punti del dataset covids al grafico
points(covids) 

# salvare il file all'interno della cartella lab

# richiamare la work directory e caricare il file salvato
setwd("C:/lab")
load("point_pattern.RData")

# AM: con ls() si può vedere il contenuto di un vettore e i nomi degli oggetti presenti
ls()

# AM: rimandare il grafico della densità relativa ai dati del covid
plot(d)

# AM: plottare la stessa immagine della denistà con la libreria spatstat
# AM: richiamare quindi la libreria
library(spatstat)
# AM: si può cambiare la palette di colori da usare nel grafico e il numero di intervalli con il comando colorRampPalette
cl <- colorRampPalette(c("yellow","orange","red"))(100)
# AM: plottiamo di nuovo la densità aggiungendo i nuovi colori
plot(d,col=cl)


# EXERCISE: plot della mappa della densità dal verde al blu
cl2 <- colorRampPalette(c("green","turquoise","blue"))(100)
plot(d,col=cl2)

# completare la mappa aggiungendo i bordi dei paesi
# AM: installare e richiamare libreria rgdal che fornisce collegamenti alla 'Geospatial' Data Abstraction Library e l'accesso alle operazioni di proiezione e di trasformazione della libreria 'PROJ.4'
install.packages("rgdal")
library(rgdal)

# AM: nella cartella lab c'è una sottocartella 'coastlines' contenete i confini di paesi che si devono caricare per essere inseriti nel plot
setwd("C:/lab/coastlines")
# AM: usare la funzione readOGR che legge un'origine dati OGR e un layer in un oggetto vettoriale spaziale adatto.
coastlines<-readOGR("ne_10m_coastline.shp")

# AM: si può procedere alla visualizzazione del grafico completo
plot(coastlines,add=T)

#EXERCISE: plot della mappa di densità con una nuova colorazione e aggiunta delle coastlines
cl1<-colorRampPalette(c("blue","light blue","light green","yellow"))(100)
plot(d,col=cl1)
plot(coastlines,add=T,col="yellow")

cl2<-colorRampPalette(c("red","orange","yellow","green","blue"))(800)
plot(d,col=cl2)
plot(coastlines,add=T)

cl3<-colorRampPalette(c("green","violet","blue"))(200)
plot(d,col=cl3)
plot(coastlines,add=T)

cl4<-colorRampPalette(c("violet","yellow","green"))(100)
plot(d,col=cl4)
plot(coastlines,add=T)

cl5<-colorRampPalette(c("darkcyan","purple","red"))(200)
plot(d,col=cl5)
plot(coastlines,add=T)

cl6<-colorRampPalette(c("white","blue","green","red","orange","yellow"))(150)
plot(d,col=cl7)
plot(coastlines,add=T)

cl7<-colorRampPalette(c('white','blue','green','red','orange','yellow')) (150)
plot(d, col=cl7)
plot(coastlines, add=T)


# INTERPOLATION


# EXERCIZE: usiamo il vecchio sript sul covid e andiamo a plottare la mappa di densità;
# AM: si carica lo script relativo ai dati del covid 
setwd("C:/lab")
load("point_pattern.RData")

point_pattern <- read.table("C:/lab/point_pattern.RData", quote="\"")
ls()
covid <- read.table("covid_agg.csv",header = T)
attach(covid)

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)

library(spatstat)
covids <- ppp(lon,lat,c(-180,180),c(-90,90))
d <- density(covids)
plot(d)

setwd("C:/lab/coastlines")
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)

### interpolazione

head(covid)

# AM: bisogna creare dei valori per interpolazione in base alle etichette per ogni paese
# AM: si usa il point pattern di ppp, e si associa alla colonna cases del dataset covid
# AM: se si manda prima il comando attach(covid) non si deve scrivere covid$cases ma solo cases
marks(covids) <- covid$cases

# AM: viene creata la mappa con la funzione Smooth.
s <- Smooth(covids)
# AM: successivamente la si può plottare
plot(s)

# EXERCIZE: plot(s) with points and coastlines
cl4 <- colorRampPalette(c('violet', 'yellow', 'green')) (100) 
plot(s,col=cl4,main="Covid cases estimate")

points(covids)


setwd("C:/lab/coastlines")
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)

# AM: viene fatta una stima non della densità di punti ma di casi nel mondo
# AM: con la funzione text si aggiunge testo a un grafico. text disegna le stringhe fornite nel vettore labels alle coordinate fornite da x e y.
text(covid)

# AM: si possono rappresentare le mappe finale in un unico grafico con la funzione rappresentatrice del multiframe
par(mfrow=c(2,1))

# AM: il primo plot rappresenta la densità 
library(spatstat)
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)

library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)

# AM: il secondo plot rappresenta l'interpolazione del numero di casi
cl4 <- colorRampPalette(c('violet', 'yellow', 'green')) (100) 
plot(s,col=cl4,main="Covid cases estimate")
points(covids)

library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)

# AM: con dev.off() vengono chiusi i multiframe
dev.off()



# AM: si lavora con un nuovo set di dati scaricati all'interno della cartella lab riguardanti i dati della tesi su San Marino
setwd("C:/lab")
load("Tesi.RData")
# AM: visualizzare gli elementi all'interno
ls()
# AM: lavorare con 'Tesi', quindi fissare i dati 
attach(Tesi)

# AM: visualizzare le prime 6 righe della tabella
head(Tesi)

# AM: utilizzare la libreria spatstat
library(spatstat)

# AM: si procede alla rappresentazione del point pattern

# AM: con la funzione summary riepilogo dei risultati delle varie funzioni di adattamento del modello
summary(Tesi)
# AM: la longitudine x varia da un minimo di 12.42 ad un massimo di 12.46
# AM: la latitudine y varia da un minimo di 43.91 ad un massimo di 43.94
# AM: ricordare che il point pattern ha bisogno di specifiche riguardo la x, la y e i range delle due 
Tesippp <- ppp(Longitude, Latitude, c(12.41,12.47), c(43.9,43.95))

# AM: calcolare la densità
dT <- density(Tesippp)
# AM: plottare il grafico relativo alla densità
plot(dT)
# AM: impostare una nuova colorRampPalette e utilizzarla nel plottaggio della densità
cl2 <- colorRampPalette(c("light blue","blue","violet"))
plot(dT,col=cl2,main="Density map of San Marino data")

points(Tesippp,col="blue")

# AM: la funzione colors permette di visionare i colori associati ai numeri così da inserirli correttamente nelle funzioni
colors() 

####

setwd("C:/lab")
load("sanmarino.RData")
# visualiziamo i dati 
ls()

# AM: dT è la density map
# AM: Tesi è un set di dati ricavato e salvato all'interno di "Tesi.RData"
# AM: Tesippp rappresenta il point pattern, con coordinate della tabella originale 
# AM: partendo da Tesippp si è arrivati a costruire la dT, mappa di densità

cl2 <- colorRampPalette(c("light blue","blue","violet"))
plot(dT,col=cl2,main="Density map of San Marino data")
points(Tesippp,col="blue")

# AM: richiamare la libreria spatstat
library(spatstat)

# AM: points permette di visualizzare la densità di campionamento
points(Tesippp, col="blue")
 
head(Tesi)

# AM: la funzione marks associa i valori della variabile che si vuole interpolare al point pattern (punti spaziali)
marks(Tesippp)<-Tesi$Species_richness
# AM: ricordare che $ indica la colonna del dataset che si vuole prendere in considerazione

# AM: si può dunque procedere con l'interpolazione, ovvero la stima. 
# AM: creare una mappa continua partendo da valori discreti

# AM: Smooth è la funzione che stima i valori dove questi non sono stati misurati
interpol<-Smooth(Tesippp)

# AM: plottare la interpol
plot(interpol)
points(Tesippp,col="blue")
# AM: dal plot risualtante si può dedurre che si ha una maggiore ricchezza di dati nella parte Sud-Est e nella parte centrale del territorio di analisi

# AM: si prodegue metendo i confini del territorio di San Marino, caricando l'immagine vettoriale con la libreria rgdal
setwd("C:/lab")
# AM: libreria per utilizzare qualsiasi tipo di file vettoriale
library(rgdal)
# AM: è possibile ora convertire il file .shp 
sanmarino<-readOGR("San_Marino.shp")

# AM: e ora plottarlo
# AM: visualizzare il territorio si San Marino
plot(sanmarino)
# AM: add=T sta a significare che la mappa di interpolazione viene sovrapposta alla mappa di San Marino
plot(interpol,add=T) 
# AM: con points vengono aggiunti i punti alle mappe
points(Tesippp,col="blue") 
# AM: si rimanda il comando plot per far in modo che i confini di San Marino siano ben visibili anche al di sopra della mappa di interpolazione
plot(sanmarino,add=T) #per far vedere di nuovo i confini di San Marino


# EXERCIZE: plot multiframe di densità e interpolazione strutturato in due righe e una colonna
par(mfrow=c(2,1))

plot(dT,main="Density of points")
points(Tesippp,col="blue")

plot(interpol,main="Estimate of species richness")
points(Tesippp,col="blue")

# EXERCIZE: plot multiframe di densità e interpolazione strutturato in due colonne e una riga
par(mfrow=c(1,2))

plot(dT,main="Density of points")
points(Tesippp,col="blue")

plot(interpol,main="Estimate of species richness")
points(Tesippp,col="blue")



##################################################
##################################################
##################################################


### 5. R code teleril
# telerilevamento

# codice R per le immagini satellitari

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

# save RData

###

# day 2
setwd("C:/lab") # windows

load("teleril.RData")

ls()
# [1] "p224r63"      "p224r63_2011"

library(raster)
plot(p224r63_2011)

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
# $ colonna legato al dataset

# Exercise: plottare la banda dell'infrarosso vicino con colorRampPalette che varia dal rosso, all'arancione, al giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

# multiframe
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

# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4 

#essendo il risultato nero, usiamo la funzione stretch="lin"
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="lin")

#per visualizzare meglio il nostro grafico risultante cambiamo le bande inserendo l'infrarosso 
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="lin")
#essendo le piante riflettenti assumeranno il colore rosso
#il celeste indicherà le zone agricole non coltivata
#il rosa le zone agricole coltivate

#salvare l'immagine della mappa ottenuta
pdf("primografico.pdf") # png("primografico.png")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()


#visualizzare i 2 grafici ottenuti usando la funzione multiframe
par(mfrow=c(2,1))

plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="lin")

dev.off()

# nir nella compnente R(Red)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# Exercise: infrarosso nella componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")

# Exercise: infrarosso nella componente blue
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")


#Landcover

#settiamo la nostra directory
setwd("c:/lab")

#richiamiamo la libreria raster
library(raster)

#recuperiamo le immagini che sono di nostro interesse contenute nella nostra working directory
p224r63_2011 <- brick("p224r63_2011_masked.grd")

#chiamiamo la libreria RStoolbox
library(RStoolbox)

plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")

#unsuperClass, l'mmagine di partenza e numero di classi 

p224r63_2011c <- unsuperClass(p224r63_2011,nClasses = 4)

#visualizziamo ciò che abbiamo svolto, informazione sull'immagine; si unisce la mappa al modello 
p224r63_2011c

#plottiamo la nostra mappa, i colori interi ci mostrano le nostre 4 classi;
plot(p224r63_2011c$map)

#cambiamo i colori del nostro grafico così da avere una migliore interpretazione del grafico

clclass <- colorRampPalette(c('green',"red","blue","black"))(100)

plot(p224r63_2011c$map,col=clclass)



###

#telerilevamento parte due

#per riuscire ad usare le funzioni di telerilevamnento bisogna usare la libreria raster;
library(raster)

#settiamo la working directory
setwd("C:/lab")

#questa volta useremo un altra immagine per il nostro telerilevamento
#NB. abbiamo messo nella directory un set di dati che andremo ad usare durante tutto il nostro lavoro su R
#applichiamo la funzione brick per riuscire a rinominare e portare in R la nostra immagine di interesse

load("teleril.RData")

# list
ls()

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

#nir
clnir <- colorRampPalette(c("purple","pink","light pink"))(100)
plot(p224r63_1988$B4_sre,col=clnir)

#per chiudere la finestra attuale useremo:

dev.off()

#creiamo un plot RGB in True colours o natural

plotRGB(p224r63_1988,r=3,g=2,b=1,stretch="Lin")

#essendo il grafico non comprensibile useremo la scala cromatica dei colori falsi
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
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
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

#cambiamo il fattore in 50, lower resolution
p224r63_2011lr50 <- aggregate(p224r63_2011,fact=50)
#visualizziamo le informazioni dell'immagine
p224r63_2011lr50

#creiamo il nostro grafico mettendoli sempre in correlazione;
par(mfrow=c(3,1))
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011lr,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011lr50,r=4,g=3,b=2,stretch="Lin")
dev.off()

#facciamo un dvi della nuova immagine 2011
dvi2011lr50 <- p224r63_2011lr2$B4_sre - p224r63_2011lr2$B3_sre
plot(dvi2011lr50)

#diminuiamo la risoluzione del 1988
p224r63_1988lr50 <- aggregate(p224r63_1988,fact=50)

#facciamo un dvi della nuava immagine 1988
dvi1988lr50 <- p224r63_1988lr2$B4_sre - p224r63_1988lr2$B3_sre
plot(dvi1988lr50)

#facciamo la differenza dei DVI dei due anni a bassa risoluzione
dfdvilr50 <- dvi2011lr50-dvi1988lr50

#creiamo la nostra immagine:
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(dfdvilr50,col=cldfdvi)

#multiframe del totale 
par(mfrow=c(2,1))
plot(dfdvi,col=cldfdvi)
plot(dfdvilr50,col=cldfdvi)
dev.off()

#é importante usare la scala giusta per evitare di non riuscire a distinguere le microdivresità presente nel grafico;



##################################################
##################################################
##################################################

### 7. R code multitemp
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

cl1<-colorRampPalette(c('green','blue'))(100)
plot(d2c$map,col=cl1)

# mettiamo a confronte le due immagini
par(mfrow=c(2,1))
plot(d1c$map,col=cl1)
plot(d2c$map,col=cl1)

par(mfrow=c(1,2))
plot(d1c$map, col=cl1)
plot(d2c$map, col=cl1)

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

freq(d1c$map)
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
load("defor.RData")
ls()

# rivediamo le immagini a confronto della foresta prima e dopo il disboscamento
library(raster)
par(mfrow=c(1,2))
cl1<-colorRampPalette(c('green','blue'))(100)
plot(d1c$map,col=cl1)
plot(d2c$map,col=cl1)

dev.off()

# controliamo il dataframe contenete l'agricultura e la foresta prima e dopo il disboscamento
output


# possiamo quindi fare un plot della percentuale di foresta attuale e precedente


# carichiamo la libreria ggplot2
library(ggplot2)
# plot della percentuale precedente la deforestazione
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover))+geom_bar(stat="identity",fill="white")
grafico1
# faremo degli istogrammi del dataframe di output
# aes: sulla x agricoltura e foresta, sulla y la percentuale di copertura prima della deforestazione
# colore si baserà sulla cover (agricoltura e foresta)
# stat sono le statistiche che considera, in questo caso le identità
# fill dà il colore alla copertura

## Exercize: plot the histograms of the land cover after deforestation
grafico2<-ggplot(output,aes(x=cover,y=after,color=cover))+geom_bar(stat="identity",fill="white")
grafico2

# possiamo fare un plot dei due istogrammi per confrontarli
# dobbiamo però usare un'altra libreria, la gridExtra 
install.packages("gridExtra")
library(gridExtra)

# possiamo quindi procedere a fare un plot con la funzione grid.arrange. La funzione prende vari plot e li mette insieme all'interno di uno stesso grafico (funzione simile a par)
grid.arrange(grafico1,grafico2,nrow=1)
# la percentuale di agricoltura relativa a prima della deforestazione sale vertiginosamente fino a raggiungere quasi il livello della foresta dopo la deforestazione

# mettiamo la scala dell'asse delle y da 0 a 100 così i due grafici possono essere confrontati meglio
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover))+geom_bar(stat="identity",fill="white")+ylim(0,100)
grafico2<-ggplot(output,aes(x=cover,y=after,color=cover))+geom_bar(stat="identity",fill="white")+ylim(0,100)
grid.arrange(grafico1,grafico2,nrow=1)




##################################################
##################################################
##################################################


# 8. R code multitemp NO2

# codice per analisi dei dati NO2 da ESA - gennaio a marzo 2020

# useremo le seguenti librerie
library(raster)



# settiamo la WD
setwd("C:/lab")

EN01<-raster("EN_0001.png")
plot(EN01)

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
cldif <- colorRampPalette(c('blue','black','yellow'))(100)
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




##################################################
##################################################
##################################################


### 9. R code Snow

# settiamo la WD
setwd("C:/lab")

# istalliamo la libreria 
install.packages("ncdf4")
#richiamiamo la libreria appena istallata e la libreria raster
library(ncdf4)
library(raster)

#importiamo con la funzione raster l'immagine scaricata da Copernicus
snowmay<-raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

#visualizziamo dunque l'immagne ed essendo neve possiamo fare una ColorRampPalette 
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100) 
#e plottiamo l'immagine con il colore 
plot(snowmay,col=cl)

#settiamo la nuova WD che è la cartella snow
setwd("C:/lab/snow")

#importiamo tutti i file
library(raster)
rlist=list.files(pattern="snow",full.names=T)

#usiamo lapply
list_rast=lapply(rlist,raster)
snow.multitemp<-stack(list_rast)
#plottiamo le immagini
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100) 
plot(snow.multitemp, col=cl)

par(mfrowc=(1,2)
plot(snow.multitemp$snow2000r,col=cl)
plot(snow.multitemp$snow2020r,col=cl)

#mettiamo il limite delle assi delle y uguali per entrambe le mappe
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))

dev.off()

# vediamo la differenza tra le due mappe
diffsnow=snow.multitemp$snow2020r - snow.multitemp$snow2000r
#nuova colorramppalette
cldiff<-colorRampPalette(c('blue','white','red'))(100)
plot(diffsnow,col=cldiff)

#funzione source ci permette di girare un codice dall'esterno
source("prediction.r")

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm,col=cl) 




##################################################
##################################################
##################################################

### 10. R code patches

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


    
    
##################################################
##################################################
##################################################
    

### 11. R_code_crop.r
    
    
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
# come variano le immagini nel tempo, attraverso una stima quantitativa della copertura nevosa in più o in meno
    
    
##################################################
##################################################
##################################################
    


