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
# 12. R_code_Species_Distribution_modeling
# 13. R_code_Exam

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


# EXERCISE: plot del rame e zinco (copper e zinc) con simbolo triangolo (17) e colore verde
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

# EXERCISE: sp plot dei dati di rame
head(meuse)
# AM: un altro comando che permette di visualizzare i nomi delle colonne è names()
names(meuse) 
spplot(meuse,"copper")

# AM: il comando bubble crea un grafico a bolle di dati spaziali, con opzioni per i grafici residui bicolori
bubble(meuse,"zinc")

# EXERCISE: grafico bubble del rame, colorato di rosso
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

# AM: metodo alternativo tramite fissazione del dataset
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
load("C:/lab/point_pattern.RData")

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


# EXERCISE: usiamo il vecchio sript sul covid e andiamo a plottare la mappa di densità;
# AM: si carica lo script relativo ai dati del covid 
setwd("C:/lab")
load("C:/lab/point_pattern.RData")

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

# EXERCISE: plot(s) with points and coastlines
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
load("C:/lab/Tesi.RData")
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
load("C:/lab/sanmarino.RData")
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


# EXERCISE: plot multiframe di densità e interpolazione strutturato in due righe e una colonna
par(mfrow=c(2,1))

plot(dT,main="Density of points")
points(Tesippp,col="blue")

plot(interpol,main="Estimate of species richness")
points(Tesippp,col="blue")

# EXERCISE: plot multiframe di densità e interpolazione strutturato in due colonne e una riga
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

# AM: librerie
library(raster)
library(RStoolbox)

# AM: installare le librerie che serviranno per lavorare con i dati
# AM: pacchetto raster per la lettura, la scrittura, la manipolazone, l'analisi e la modellizzazione di dati spaziali gridded
install.packages("raster")
# AM: pacchetto R Stool box per l'analisi dei dati mediante il telerilevamento, come il calcolo di indici spettrali, la trasformazione dei componeneti principali, la classificazione non supervisionata e supervisionata o l'analisi di coperture frazionate. 
install.packages("RStoolbox")

# AM: selezionare la WD. Lavorare con i dati all'interno della cartella p224r63 che sono relativi all'anno 1988 e all'anno 2011
setwd("C:/lab/p224r63")
# AM: richiamare la libreria raster
library(raster)

# AM: rinominare il file che ci servirà per il nostro lavoro
# AM: la funzione brick crea un oggetto RasterBrick, ovvero un oggetto multistrato. Sono in genere creati da un file multi-layer (banda)
p224r63_2011 <- brick("p224r63_2011_masked.grd")

# AM: una volta creto l'oggetto RasterBrick, lo si può plottare
plot(p224r63_2011)

# AM: risualtano 7 plot diversi, corrispondenti a 7 bande diverse:
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
setwd("C:/lab") 
load("C:/lab/teleril.RData")

ls()

library(raster)
plot(p224r63_2011)

# AM: impostare una sola scala cromatica per tutto il plot
cl <- colorRampPalette(c("black","grey","light grey"))(100) 
# AM: e lo si aggiunge al comando del grafico
plot(p224r63_2011,col=cl)

# AM: modifica delle scale cormatiche. Non più 100 intervalli ma 5
cllow <- colorRampPalette(c("black","grey","light grey"))(5) 
# AM: scala cromatica diversificata nel grafico
plot(p224r63_2011,col=cllow)

# AM: si possiono visionare i nomi di un oggetto con la funzione names
names(p224r63_2011)
#[1] "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"


# AM: si ricorda che le bande rappresentano i colori:
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# AM: cambiare la banda blu (banda 1) con una color palette in scala di blu
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
# AM: la funzione attach  relativa al fissaggio di un data frame non funziona con il pacchetto raster ma si deve usare $ 
# AM: con la nuova palette si visualizza solo la banda del blu all'interno del grafico
plot(p224r63_2011$B1_sre,col=clb)


# EXERCISE: plottare la banda dell'infrarosso vicino con colorRampPalette che varia dal rosso, all'arancione, al giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

# AM: multiframe dei plot in diverse bande di colore
par(mfrow=c(2,2))
# blue
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_2011$B1_sre,col=clb)
# green
clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_2011$B2_sre,col=clg)
# red
clr <- colorRampPalette(c("red","orange","yellow"))(100)
plot(p224r63_2011$B3_sre,col=clr)
# infrared (nir)
clir <- colorRampPalette(c("purple","pink","light pink"))(100)
plot(p224r63_2011$B4_sre,col=clir)

# AM: chiusura delle immagini plottate
dev.off()


# AM: la funzione plotRGB crea un plot rosso-verde-blu basato su tre livelli. Quindi tre strati sono combinati in modo tale da rappresentare il canale rosso, verde e blu. 
# AM: Questa funzione può essere utilizzata per creare "immagini a colori vere (o false)" da Landsat e altre immagini satellitari multi-banda.
# 3 bande alla volta R= red G= green B= blue
plotRGB(p224r63_2011,r=3,g=2,b=1)

# B1: blue = 1
# B2: green = 2
# B3: red = 3
# B4: near infrared (nir) = 4 

# AM: essendo il risultato nero, usiamo la funzione stretch="lin", che permette di aumentare il contrasto dell'immagine
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="lin")

# AM: affinchè il grafico risuli più leggible, si aggiunge la banda dell'infrarosso
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="lin")
# AM: le piante sono molto riflettenti e assumono il colore rosso
# AM: il celeste indica le zone agricole non coltivata
# AM: il rosa rappresenta le zone agricole coltivate

# AM: salvare l'immagine della mappa ottenuta
pdf("primografico.pdf") # png("primografico.png")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()


# AM: con il multiframe possono essere confrontati i due grafici ottenuti con le bande diverse
par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="lin")

dev.off()

# EXERCISE: nir nella compnente R(Red)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# EXERCISE: infrarosso nella componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")

# EXERCISE: infrarosso nella componente blue
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")


# Landcover

# AM: impostare la work directory
setwd("C:/lab/p224r63")

# AM: usare la libreria raster
library(raster)

# AM: recuperare e importare le immagini che di interesse contenute nella working directory
p224r63_2011 <- brick("p224r63_2011_masked.grd")

# AM: richiamre libreria RStoolbox
library(RStoolbox)

# AM: plottare l'immagine in R G B
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")

# AM: classificazione senza supervisione dei dati raster con unsuperClass, specificando il numero di classi
p224r63_2011c <- unsuperClass(p224r63_2011,nClasses = 4)

# AM: visualizzando il nuovo data si hanno informazioni sull'immagine. A questo modello viene aggiunta anche la mappa 
p224r63_2011c

# AM: plottando la mappa, specificata con il $, si avranno 4 colori a dimostrazione delle quattro classi specificate 
plot(p224r63_2011c$map)

# AM: impostare una nuova palette che permette una migiore visualizzazione e interpretazione del grafico
clclass <- colorRampPalette(c('green',"red","blue","black"))(100)
plot(p224r63_2011c$map,col=clclass)



###

#telerilevamento parte due

#per riuscire ad usare le funzioni di telerilevamnento bisogna usare la libreria raster;
library(raster)

#settiamo la working directory
setwd("C:/lab/p224r63")

# AM: lavoro di telerilevamento con l'immagine relativa al 1988 
# AM: importare l'immagine con la funzione brick

load("C:/lab/teleril.RData")

# AM: visualizzare gli oggetti 
ls()
p224r63_2011 <- brick("p224r63_2011_masked.grd") 

p224r63_1988 <- brick("p224r63_1988_masked.grd")

# AM: creare il plot dell'oggetto del 1988
# AM: questa immagine è stata fornita dal satellite lensat del 1988
# AM: come per l'immagine del 2011, anche questa ha 7 bande corrispondenti a 7 colori
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# AM: plottaggio dell'oggetto del 1988
plot(p224r63_1988)

# AM: visualizzare i campi di interesse
names(p224r63_1988)

# AM: usare il multiframe per visualizzare i grafici della banda del blu (1), del verde (2), del rosso (3) e del vicino all'infrarosso (4) iniseme
par(mfrow=c(2,2))
# blue
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_1988$B1_sre,col=clb)
# green
clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_1988$B2_sre,col=clg)
# red
clr <- colorRampPalette(c("red","orange","yellow"))(100)
plot(p224r63_1988$B3_sre,col=clr)
# nir
clnir <- colorRampPalette(c("purple","pink","light pink"))(100)
plot(p224r63_1988$B4_sre,col=clnir)

# AM: per chiudere la finestra attuale 
dev.off()


# AM: creare il plot RGB in True colours o natural
plotRGB(p224r63_1988,r=3,g=2,b=1,stretch="Lin")

# AM: essendo il grafico non comprensibile si usa la scala cromatica dei colori falsi
# EXERCISE: creiamo il plot con la componente infrarosso nella componente rossa
# AM: in questo caso i colori vengono definiti False colours
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")

p224r63_2011 <- brick("p224r63_2011_masked.grd")
# AM: mettere a confronto le immagini del 1988 e del 2011 per veder come è cambiata la situazione nel tempo
par(mfrow=c(2,1))
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
# AM: chiudere il multiframe
dev.off()

# AM: paragonando le due situazioni si nota che il territorio agricolo è molto più sviluppato nel 2011
# AM: il colore della banda 4 (near infrared) è rappresentato dalle piante
# AM: e le zolle di terra sono bianche o celeste


# AM: si può calcolare calcolare l'indice di salute della vegetazione
# AM: ricordare che calcolare tale indice è possibile in quanto le foglie sane riescono a riflettere l'infrarosso 
# AM: DVI(Difference Vegetation Index) sarà l'indice che useremo noi

# AM: DVI=NIR-RED, si avranno dei risultati diversi in base alla salute delle piante
# AM: pianta sana = NIR alto
# AM: pianta malata = RED alto 

# 1988
# div1988=nir1988-red1988
# AM: per prendere la colonna relativa si usa il $
dvi1988 <- p224r63_1988$B4_sre-p224r63_1988$B3_sre

# AM: plot del DVI 1988
plot(dvi1988)


# 2011
# div2011=nir2011-red2011
dvi2011 <- p224r63_2011$B4_sre-p224r63_2011$B3_sre
plot(dvi2011)

# AM: è possibile cambiare il colore sempre utilizzando la color Ramp Palette
cldvi11 <- colorRampPalette(c('dark green','green','light green'))(100)
plot(dvi2011,col=cldvi11)


# AM: si hanno gli indici DVI relativi ai due differenti anni
# AM: facendo la differenza tra i 2 anni si vedrà di quanto è stato il cambiamento della vegetazione
# AM: se il valore del 2011 è negativo vuol dire che la vegetazione è peggiorata rispetto al 1988
# AM: multitemporal analysis
dfdvi <- dvi2011-dvi1988

# AM: si crea l'immagine realtiva alle zone dove le piante hanno subito maggiore stress
plot(dfdvi)
cldfdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(dfdvi,col=cldfdvi)

# AM: multiframe per visualizzare tutti i grafici dell'immagine del 1988, dell'immagine del 2011 e della differenza di indice DVI
par(mfrow=c(3,1))
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plot(dfdvi,col=cldfdvi)

dev.off()

# AM: cambiare la risoluzione di un immagine, la funione che si usa è aggregate che crea un nuovo RasterLayer o RasterBrick con una risoluzione inferiore, quindi celle più grandi
p224r63_2011lr <- aggregate(p224r63_2011,fact=10)
# AM: se fact o factor viene settato uguale a 10 usiamo una scala 10 volte maggiore 

# AM: caratteriestiche dell'immagine originale
p224r63_2011

# AM: caratteristiche della nuova immagine
p224r63_2011lr

# AM: multiframe per confronto dei due grafici
par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011lr,r=4,g=3,b=2,stretch="Lin")
dev.off()

# AM: cambiamo il fattore in 50, lower resolution
p224r63_2011lr50 <- aggregate(p224r63_2011,fact=50)
# AM: visualizzare le informazioni dell'immagine
p224r63_2011lr50

# AM: multiframe comparativo dei grafici ottenuti
par(mfrow=c(3,1))
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011lr,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011lr50,r=4,g=3,b=2,stretch="Lin")
dev.off()

# AM: calcolare l'indice DVI della nuova immagine a bassa risoluzione del 2011
dvi2011lr50 <- p224r63_2011lr$B4_sre - p224r63_2011lr$B3_sre
plot(dvi2011lr50)

# AM: cambiare la risoluzione dell'immagine del 1988
p224r63_1988lr <- aggregate(p224r63_1988,fact=10)
# AM: caratteriestiche dell'immagine originale
p224r63_1988
# AM: caratteristiche della nuova immagine
p224r63_1988lr
# AM: diminuire la risoluzione di 50 dell'immagine del 1988 sempre con aggregate
p224r63_1988lr50 <- aggregate(p224r63_1988,fact=50)

# AM: creare l'indice DVI della nuova immagine 1988
dvi1988lr50 <- p224r63_1988lr$B4_sre - p224r63_1988lr$B3_sre
plot(dvi1988lr50)

# AM: ottenere la differenza dei DVI dei due anni a bassa risoluzione
dfdvilr50 <- dvi2011lr50-dvi1988lr50

# AM: impostare la color Ramp Palette per l'immagine
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(dfdvilr50,col=cldfdvi)

# AM: multiframe conforntando i DVI con le diverse risoluzioni 
par(mfrow=c(2,1))
plot(dfdvi,col=cldfdvi)
plot(dfdvilr50,col=cldfdvi)
dev.off()

# AM: é importante usare la scala giusta per evitare di non riuscire a distinguere le microdivresità presente nel grafico



##################################################
##################################################
##################################################

### 7. R code multitemp

# R code analisi multitemporale di variazione della land cover

# AM: librerie
library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)


# AM: impostare la Working Directory
setwd("C:/lab")

# AM: richiamare la libreria raster
library(raster)

# AM: brick() è una funzione del pacchetto raster che permette di caricare dati dall'esterno, caricando tutte le singole bande se si stratta di una immagine satellitare
# AM: la funzione crea un oggetto RasterBrick, ossia un oggetto raster multistrato, in genere creati da un file multi-layer
defor1<-brick("defor1_.jpg")
defor2<-brick("defor2_.jpg")

# AM: controllare le informazioni di defor1
defor1
# AM: all'interno del dataset, nella riga 'names' si hanno tre bande
# AM: names: defor1_.1, defor1_.2, defor1_.3 
# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green

# AM: si procede con il plot RGB, associando ogni banda ad una componente rgb
# AM: banda del rosso alla componente NIR (r=1)
# AM: banda del green alla componente red (g=2)
# AM: banda del blu alla componente green (b=3)

plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")

# EXERCISE: plot della seconda data
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

# AM: multiframe per comparare le due immagini, confronto della foreste pluviale in due momenti diversi, prima e dopo la deforestazione
par(mfrow=c(2,1))
plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

dev.off()

# AM: per la classificazione viene usato il comando unsuperClass della libreria RStoolbox
# AM: unsuperClass è la classificazione non supervisionata, cioè non vengono date dei training set al pc
# AM: caricare dunque la libreria utile
library(RStoolbox)

# AM: classificazione
d1c<-unsuperClass(defor1,nClasses=2)
# AM: tra i risultati della classificazione si ha il valore 'map'
# AM: si valuta quindi questo valore
d1c$map
# AM: e si procede con il plottaggio
plot(d1c$map)

# AM: viene creata una color Ramp palette per definire meglio le variazioni, e la si inserisce nel plot
cl1<-colorRampPalette(c('green','blue'))(100)
plot(d1c$map,col=cl1)

# EXERCISE: classificare con due classi l'immagine satellitare defor2
# AM: si lavora con defor2, relativa al tempo successivo la deforestazione
d2c<-unsuperClass(defor2,nClasses=2)
# AM: si valuta il valore 'map' e si procede al plottaggio 
d2c$map
plot(d2c$map)
# AM: vengono cambiati i colori di rappresentazione nel plot
cl1<-colorRampPalette(c('green','blue'))(100)
plot(d2c$map,col=cl1)

# AM: una volta completata la classificazione per i due momenti relativi alla deforestazione si procede con un multiframe per un confronto
# AM: multiframe due righe e una colonna
par(mfrow=c(2,1))
plot(d1c$map,col=cl1)
plot(d2c$map,col=cl1)
# AM: multiframe due colonne e una riga
par(mfrow=c(1,2))
plot(d1c$map, col=cl1)
plot(d2c$map, col=cl1)

dev.off()

###########
# AM: prova di classificazione con tre classi dell'immagine satellitare defor2
d2c3<-unsuperClass(defor2,nClasses=3)
d2c3$map
plot(d2c3$map)
cl3<-colorRampPalette(c('orange','green','blue'))(100)
plot(d2c3$map,col=cl3)
# AM: confronto con multiframe tra la classificazione di defor2 con 2 e 3 classi
par(mfrow=c(1,2))
plot(d2c$map, col=cl1)
plot(d2c3$map,col=cl3)

dev.off()
###########

# AM: si prcede con il generare delle frequenze da una variabile con percentuali e opzioni di formattazione
freq(d1c$map)
# AM: viene quantificata la quantità di foresta che è stata persa
# area aperta = 35233
# foresta = 306059

# AM: prima di operare al fine di una percentuale si deve ricavare il totale
totd1<- 35233 + 306059 
# totd1 = 341292

# AM: successivamente si può calcolare la percentuale
percent1<-freq(d1c$map)*100/totd1

# AM: percentuali risultanti
# aree aperte = 10.3 
# foresta = 89.7


# AM: si procede allo stesso modo con defor2
freq(d2c$map)
# aree aperte = 178053
# foresta = 164673

totd2<- 178053 + 164673 
# totd2 = 342726

# percentuale
percent2<-freq(d2c$map)*100/totd2
# aree aperte = 51.95
# foreste =  48.05

# AM: creare un nuovo dataset con i dati ottenuti
cover <- c("Agriculture","Forest")
before<-c(10.3,89.7)
after<-c(51.95,48.05)

output <- data.frame(cover,before,after)
View(output)


#### DAY 2 ###
# riapriamo il file dell'utlima lezione
setwd("C:/lab")
load("C:/lab/defor.RData")
ls()

# AM: ricostruire il multiframe delle due immagini relative a prima e dopo il disboscamento
library(raster)
par(mfrow=c(1,2))
cl1<-colorRampPalette(c('green','blue'))(100)
plot(d1c$map,col=cl1)
plot(d2c$map,col=cl1)

dev.off()

# AM: controllo del dataframe contenete l'agricultura e la foresta prima e dopo il disboscamento
output


# AM: si procede a creare un plot della percentuale di foresta attuale e precedente

# AM: servirà la libreria ggplot2 
# AM: richiamare tale libreria
library(ggplot2)
# AM: plot della percentuale precedente la deforestazione
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover))+geom_bar(stat="identity",fill="white")
grafico1
# AM: verranno fatti istogrammi del dataframe di output
# AM: sulle ascisse viene rappresentato aes, quindi agricoltura e foresta, sulle ordinate la percentuale di copertura prima della deforestazione
# AM: il colore si baserà sulla cover (agricoltura e foresta)
# AM: stat sono le statistiche che considera, in questo caso le identità
# AM: fill dà il colore alla copertura

# EXERCISE: plot the histograms of the land cover after deforestation
grafico2<-ggplot(output,aes(x=cover,y=after,color=cover))+geom_bar(stat="identity",fill="white")
grafico2

# AM: si possono confrontare i due istogrammi risultanti, ma c'è bisogno della libreria gridExtra 
# AM: gridExtra: Funzioni varie per la grafica "Grid"
# AM: il pacchetto fornisce una serie di funzioni per lavorare con la grafica "griglia", in particolare per organizzare più grafici basati sulla griglia in una sola pagina, e disegnare tabelle.
install.packages("gridExtra")
library(gridExtra)

# AM: si può dunque procedere al confronto con la funzione grid.arrange. 
# AM: La funzione prende vari plot e li mette insieme all'interno di uno stesso grafico (funzione simile a par)
grid.arrange(grafico1,grafico2,nrow=1)
# AM: dalla comparazione degli istogrammi emerge che la percentuale relativa alla agricoltura prima della deforestazione sale vertiginosamente fino a raggiungere quasi il livello della foresta dopo la deforestazione

# AM: per un contronto migliore si uniformano le scale dei due istogrammi
# AM: mettiamo la scala dell'asse delle y da 0 a 100 per entrambi gli istogrammi
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover))+geom_bar(stat="identity",fill="white")+ylim(0,100)
grafico2<-ggplot(output,aes(x=cover,y=after,color=cover))+geom_bar(stat="identity",fill="white")+ylim(0,100)
grid.arrange(grafico1,grafico2,nrow=1)




##################################################
##################################################
##################################################


# 8. R code multitemp NO2

# AM: codice per analisi dei dati NO2 da ESA - gennaio a marzo 2020

# AM: librerie
library(raster)

# AM: selezionare la WD
setwd("C:/lab")
 # AM: caricare la prima immagine e successivamente plottarla
EN01<-raster("EN_0001.png")
plot(EN01)

# AM: importare le singole immagini con la funzione raster
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


# AM: confronto tra la prima e l'ultima immagine, impostando una color palette in un analisi con multiframe
library(raster)
cl<-colorRampPalette(c('red','orange','yellow'))(100)
par(mfrow=c(1,2))
plot(EN01,col=cl)
plot(EN13,col=cl)

dev.off()

# AM: fare la difference tra l'immagine relativa all'ultima analisi e l'immagine relativa alla prima
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('royalblue','powderblue','black'))(100)
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
load("C:/lab/EN.RData")
ls()

# AM: invece di importare una immagine per volta, si usa una funzione della libreria raster
library(raster)
# AM: si crea una cartella nella cartella lab. La nuova cartella conterrà tutti le immagini EN 
# AM: selezionare questa cartella come nuova WD
setwd("C:/lab/esa_no2")

# AM: creare una lista che seleziona le immagini con pattern=".png"
# AM: list.files produce un vettore comprendete la lista di file nella directory indicata.
# AM: impostando il pattern si avranno solo i nomi di file che corrispondono all'espressione regolare.
rlist<-list.files(pattern=".png")
# AM: si studia il contenuto del nuovo vettore
rlist 


# AM: lapply applica una funzione su una lista o un vettore (serie di elementi). 
# AM: in questo caso la funzione che si vuole applicare per caricare i singoli dati sono brick (immagine satellitare con tutti i sensori) e raster (immagine con un solo sensore)
# AM: si applica la funzione raster alla lista di file '.png' precedentemente creata
listafinale<-lapply(rlist, raster)

# visualizzare i RasterLayer 
listafinale 

# AM: invece di fare un multiframe, c'è la possibilità di includere tutte le immagini in una unica
# AM: la funzione stack permette di impilare più vettori in un singolo vettore 
EN <- stack(listafinale)

# AM: reimpostazione della color Ramp palette
cl<-colorRampPalette(c('red','orange','yellow'))(100)
# AM: si procede dunque con il plottaggio dell'immagine
plot(EN,col=cl)

# AM: riepilogo
library(raster)
setwd("C:/lab/esa_no2")
rlist
listafinale<-lapply(rlist,raster)
listafinale
EN<-stack(listafinale)

# AM: una volta caricate le immagini si può calcolare la differenza tra l'ultima immagine (EN 13 corrispondente a Marzo) e la prima immagine (EN 01 corrispondente a Gennaio) 
difEN<- EN$EN_0013-EN$EN_0001

# AM: definire una colorRampPalette da inserire nel plottaggio di questa differenza
cld<-colorRampPalette(c('blue','white','red'))(100)
plot(difEN,col=cld)

# AM: si possono confrontare le varie immagini con la funzione boxplot, che crea un diagramma a riquadri per ogni vettore
# AM: si impostano
# AM: la disposizione orizzontale
# AM: l'assenza delle linee esterne
# AM: la presenza degli assi (di default axes=T, se si vogliono togliere si mette axes=F)
boxplot(EN,horizontal=T,outiline=F,axes=T)



##################################################
##################################################
##################################################


### 9. R code Snow

# AM: settare la WD
setwd("C:/lab")

# AM: librerie
library(raster)
library(ncdf4)

# AM: si installi la libreria ncdf4
# AM: questa libreria fornisce un'interfaccia R ad alto livello per i file di dati binari che sono portatili tra piattaforme e includono informazioni sui metadati in aggiunta ai set di dati
install.packages("ncdf4")
# AM: mandare la libreria appena istallata e la libreria raster
library(ncdf4)
library(raster)

# AM: con la funzione raster si importa l'immagine scaricata da Copernicus
snowmay<-raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

# AM: visualizzare dunque l'immagne 
# AM: essendo copertura nevosa, si può impostare una ColorRampPalette che rispecchi i colori relativi
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100) 
plot(snowmay,col=cl)

# AM: settiare la nuova WD che è la cartella snow, contenente le immagini relative alla copertura in più momenti
setwd("C:/lab/snow")

# AM: si importano tutti i file creando una rlist
library(raster)
rlist<-list.files(pattern="snow",full.names=T)

# AM: e successivamente la funzione lapply che applica la funzione raster alla lista 
list_rast<-lapply(rlist,raster)
# AM: impilare più vettori in un singolo vettore 
snow.multitemp<-stack(list_rast)
# AM: successivamente plottare le immagini
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100) 
plot(snow.multitemp, col=cl)

# AM: confronto multiframe tra la prima immagine 'snow2000r' e l'ultima 'snow2020r'
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r,col=cl)
plot(snow.multitemp$snow2020r,col=cl)

# AM: impostare il limite delle ordinate uguali per entrambe le mappe
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))

dev.off()

# AM: creazione e plottaggio della differenza tra le due mappe
diffsnow<-snow.multitemp$snow2020r - snow.multitemp$snow2000r
# AM: con una nuova color Ramp palette
cldiff<-colorRampPalette(c('blue','white','red'))(100)
plot(diffsnow,col=cldiff)

# AM: funzione source ci permette di girare un codice dall'esterno
source("prediction.r")

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm,col=cl) 




##################################################
##################################################
##################################################

### 10. R code patches

# AM: settare innanzitutto la WD
setwd("C:/lab")

# AM: librerie
library(raster)
library(igraph)
library(ggplot2)

# AM: richiamare la libreria raster
library(raster)

# AM: caricare le immagini raster, la mappa classificata in questo caso, con la funzione raster
d1c<-raster("d1c.tif")
d2c<-raster("d2c.tif")

# AM: con un plot si può vedere qual è la foresta e quale la parte agricola
# AM: impostare una colorRampPalette con due colori
cl<-colorRampPalette(c('green','black'))(100)
# AM: multiframe
par(mfrow=c(1,2))
plot(d1c,col=cl)
plot(d2c,col=cl)
# AM: la mappa risultante è così sbagliata, perchè la foresta corrisponde al colore due anzichè al colore uno, bisogna quindi invertire i colori
cl<-colorRampPalette(c('black','green'))(100)
par(mfrow=c(1,2))
plot(d1c,col=cl)
plot(d2c,col=cl)

dev.off()

# land cover 1 = agriculture
# land cover 2 = forest

# AM: si volgiono eliminare tutti i valori corrispondenti alla agricoltura così da lasciare solo quelli relativi alla foresta
# AM: funzione per annullare alcuni valori è la funzione cbind che prende i valori 1 corrispondenti alla foresta e ai valori della agricoltura si assegra NA
# reclassify = riclassifichiamo i dati anullando i valori della classe della agricoltura associandogli il valore NA
d1c.for <- reclassify(d1c, cbind(1, NA))

# AM: si ha quindi una nuova mappa 
# AM: si procede con il multiframe per il confronto
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for)
# AM: è stato reso nullo il valore 1, abbiamo solo la classe delle foreste che ha valore due
# AM: si può impostare la stessa colorRampPalette anche per la seconda mappa
# AM: multiframe della prima immagine d1c, mettento a confronto la mappa che ha anche i valori della agricoltura, con la mappa senza quei valori
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c,col=cl)
plot(d1c.for,col=cl)


# AM: si procede in egual modo per la seconda immagine d2c 
# AM: solo le foreste
d2c.for<-reclassify(d2c,cbind(1,NA))

# AM: plot di entrambe le mappe classificate solo con la foresta
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)


# AM: per la creazione di patches c'è bisogno del pacchetto igraph che dovrebbe già essere incluso in raster
install.packages("igraph")
library(igraph)

# AM: creare patches, unendo e raggruppando tutti i pixel vicino per creare ogni patch
d1c.for.patches<-clump(d1c.for)
d2c.for.patches<-clump(d2c.for)

# AM: una volta create le mappe con i patches
# AM: con la funzione writerRster si esportano il file delle due mappe in formato ".tif" direttamente all'interno della cartella lab
writeRaster(d1c.for.patches,"d1c.for.patches.tif")
writeRaster(d2c.for.patches,"d2c.for.patches.tif")

# AM: raster (o brick) importa i file
# AM: writeRaster esporta i file

# EXERCISE: plottare entrambe le mappe una accanto all'altra
par(mfrow=c(1,2))
# AM: impostare una color Ramp palette con quanti più coloro si riescono così sarà più facile visualizzare i singoli patch della foresta
clp <- colorRampPalette(c('darkblue','blue','green','orange','yellow','red'))(100) 
plot(d1c.for.patches,col=clp)
plot(d2c.for.patches,col=clp)

# AM: quantificare il numero di patches che sono stati creati nelle mappe
d1c.for.patches 
# AM: il valore massimo corrisponde a 301 nella prima immagine d1c

d2c.for.patches
# AM: il valore massimo corrisponde a 1212 nella immagine d2c

# AM: risultati dei plot in un nuovo dataframe
# AM: creare time che comprende i dati prima della deforestazione e dopo
# AM: e npatches che definisce il numero di patches
time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)

# AM: creare il dataframe 'output'
output <- data.frame(time,npatches)

# AM: plottaggio finale con ggplot
# AM: richiamare la libreria ggplot2
library(ggplot2)
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")


    
    
##################################################
##################################################
##################################################
    

### 11. R_code_crop.r
    
    
# AM: selezionare la WD
# AM: procedere nel fare un crop sui dati di neve già usati in precedenza
# AM: selezionare quindi la cartella snow come WD
setwd("C:/lab/snow")

# AM: verranno caricati dati da copernicus o qualsiasi dato di immagine satellitare

# EXERCISE: caricare tutte le immagini della cartella snow
library(raster)
rlist<-list.files(pattern="snow")

# AM: la lista dei songoli file può essere importata con la funzione raster
list.rast<-lapply(rlist, raster)
list.rast

# AM: funzione stack permette di impacchettare tutte le immagini in una unica
snow.multitemp <- stack(list.rast)

clb<-colorRampPalette(c('dark blue','blue','light blue'))(100)
# AM: impostata la color Ramp palette si può plottare l'immagine
plot(snow.multitemp,col=clb)

# AM: analisi delle immagini multitemporali
snow.multitemp

# AM: plot relativo all'immagine multitemporale del 2010
plot(snow.multitemp$snow2010r, col=clb)
# AM: si noti che l'Italia si trova tra i 6 e i 20 gradi, e i 35 e i 50
# AM: si imposta la nuova estensione, per circoscrivere e zoommare l'Italia
extension <- c(6, 20, 35, 50)
zoom(snow.multitemp$snow2010r,ext=extension)

# AM: se si vuole fare uno zoom con la stessa ColorRampPalette usata per l'analisi basta aggiungere, in questo caso, col=clb
zoom(snow.multitemp$snow2010r,ext=extension,col=clb)

# AM: riplottare l'immagine originale
plot(snow.multitemp$snow2010r, col=clb)

# AM: è possibile anche definire l'estensione tramite un disegno
zoom(snow.multitemp$snow2010r, ext=drawExtent())

# funzoone crop
# AM: la funzione crop permette di ritagliare una nuova immagne della zona definita
extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r,extension)
plot(snow2010r.italy, col=clb)

# AM: in zoom dobbiamo specificare l'estenzione
# AM: in crop basta mettere l'immagine e l'estenzione che vgliamo utilizzare per la nuova immagine

# AM: si può applicare la funzione crop su tanti livelli con la funzione brick

# EXERCISE: crop dell'estenzione dell'Italia dello stacj di tutto le immagini della copertura
extension <- c(6, 20, 35, 50)
snow.multitemp.Italy<-crop(snow.multitemp,extension)
plot(snow.multitemp.Italy,col=clb)

# AM: impostare la legenda uguale per tutti considerando il mimino e il massimo
snow.multitemp.Italy
# minimo 20
# massimo 200
# aggiungere il limite zlim=c(20,200)
plot(snow.multitemp.italy, col=clb, zlim=c(20,200))


# AM: procedere con un'analisi con boxplot
boxplot(snow.multitemp.italy, horizontal=T,outline=F)

    
    
##################################################
##################################################
##################################################
    
# 12. R_code_Species_Distribution_modeling
# Species Distribution Modeling

# AM: librerie
library(sdm)
library(raster)
library(rgdal)

# AM: per il seguente lavoro non c'è bisogno di selezionare una WD perchè si lavorerà con i dati presenti nel pacchetto sdm
# AM: lavorare con il pacchetto sdm
# AM: installare e richiamare tale pacchetto
install.packages(sdm)
library(sdm)

# AM: nel pacchetto sdm si trova il file che verrà usato e biosgna caricarlo con la funzione system.file
file <- system.file("external/species.shp", package="sdm")

# AM: richiamare la libreria raster per usare la funzione shapefile
library(raster)
species <- shapefile(file)

# AM: si esamina il dataset
species
# AM: si controllano i valori della colonna occurrence
species$Occurrence
# AM: ogni punto è relazionato al fatto se la specie è stata vista o meno
# AM: occurrence ha valori 0 (assente) e 1 (presente)

# AM: plottiamo il dataframe species
plot(species)
# AM: nel plot sono visibili sia le presenze che le assenze 
# AM: quindi si procede con il modificare il comando in modo da diversificare le assenze dalle presenze
# AM: in blu le presenze
# AM: in rosso le assenze
plot(species[species$Occurrence==1,],col='blue',pch=16)
points(species[species$Occurrence==0,],col='red',pch=16)

# AM: individuare dove sono le variabili ambientali disponibili
# AM: all'interno del pacchetto sdm si torva la certella external
path <- system.file("external",package="sdm")
# AM: importare i file predittori per prevedere quale sarà la distribuzione nello spazio in base anche alle variabili ambientali
lst <- list.files(path=path,pattern='asc$',full.names = T) 

# AM: con la funzione stack si procederà nel fare un unico oggetto della lista lst delle variabili
preds <- stack(lst)
# AM: scegliere una color Ramp Palette
cl<-colorRampPalette(c('light green','dark blue','pink'))(100)
plot(preds,col=cl)

# AM: plot dell'elevation
plot(preds$elevation,col=cl)
# AM: aggiungere i punti in cui la specie è presente
points(species[species$Occurrence==1,], pch=16)
# AM: conclusione-> la specie adora la bassa elevation

# AM: considerare le termperature
plot(preds$temperature, col=cl)
# AM: aggiungere i punti in cui la specie è presente
points(species[species$Occurrence==1,],pch=16,cex=0.8)
# AM: conclusione-> temperature preferite sono quelle medio alte

# AM esaminiamo la precipitation
plot(preds$precipitation, col=cl)
# AM: aggiungere i punti in cui la specie è presente
points(species[species$Occurrence==1,],pch=16,cex=0.8)
# AM: conclusione-> non si hanno valori di massimo e minimo ma è nella media

# AM esaminiamo la vegetation
plot(preds$vegetation, col=cl)
# AM: aggiungere i punti in cui la specie è presente
points(species[species$Occurrence==1,],pch=16,cex=0.8)
# AM: conclusione-> la specie preferisce una zona ombreggiata

# AM: bassa elevation
# AM: alta temperatura
# AM: piogge normali, nè precipitazioni elevate, nè situazioni di siccità
# AM: coperta dalla vegetazione

# AM: Generalized Linear Model (glm)
# AM: modello lineare generalizzato

d<-sdmData(train=species,predictors=preds)
d

# AM: y sono le occurrence
# AM: x z w u sono i predittori
# AM: la tilde corrisponde all' uguale
# AM: data= per specificare il dataset 
# AM: argomento da utilizzare è methods. Il metodo da usare è il glm
m1<-sdm(Occurrence~elevation+precipitation+temperature+vegetation, data=d,methods='glm')

# AM: previsione
# AM: creare mappa predittiva della distribuzione della specie sulla base delle quattro variabili
p1<-predict(m1,newdata=preds)
plot(p1,col=cl)
points(species[species$Occurrence== 1,], pch=16,cex=0.8)



##################################################
##################################################
##################################################

# R_code_Exam

# CODICE ESAME 





