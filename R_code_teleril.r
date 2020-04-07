#Codice R per analisi di immagini satellitari

# packages: raster

install.packages("raster") 

library(raster)

setwd("C:/Users/asus/Desktop/Lezioni/Ecologia del paesaggio/p224r63")
 
p224r63_2011<- brick("p224r63_2011_masked.grd")
plot(p224r63_2011)

p224r63_1988m <- brick("p224r63_1988_masked.grd")

pairs(p224r63_2011m) # time consuming

# plotRGB
# plotRGB(p224r63_2011m,r=3,g=2,b=1)
plotRGB(p224r63_2011m,r=3,g=2,b=1,scale=1000,stretch="Lin")
plotRGB(p224r63_2011m,r=4,g=3,b=2,scale=1000,stretch="Lin")

par(mfrow=c(2,1))
plotRGB(p224r63_2011m,r=3,g=2,b=1,scale=1000,stretch="Lin")
plotRGB(p224r63_2011m,r=4,g=3,b=2,scale=1000,stretch="Lin")

# stretch
par(mfrow=c(2,1))
plotRGB(p224r63_2011m,r=4,g=3,b=2,scale=1000,stretch="Lin")
plotRGB(p224r63_2011m,r=4,g=3,b=2,scale=1000,stretch="hist")

# output
pdf("image.pdf")
plotRGB(p224r63_2011m,r=4,g=3,b=2,scale=1000,stretch="hist")
dev.off()

## resampling
# plot(p224r63_2011m)
p224r63_2011m_res <- aggregate(p224r63_2011m, fact=20)
# p224r63_2011m_res  <- resample(p224r63_2011m, agg)

par(mfrow=c(2,1))
plotRGB(p224r63_2011m,r=4,g=3,b=2,scale=1000,stretch="Lin")
plotRGB(p224r63_2011m_res,r=4,g=3,b=2,scale=1000,stretch="Lin")

multitemp <- p224r63_2011m - p224r63_1988m 

plot(multitemp)

##- spectral indices

dvi2011 <- p224r63_2011m$B4_sre-p224r63_2011m$B3_sre
# ~time: 1min 

dvi1988 <- p224r63_1988m$B4_sre-p224r63_1988m$B3_sre 
# ~time: 1min

#--- potential function: spectralIndices

par(mfrow=c(2,1))
plot(dvi1988,main="DVI 1988")
plot(dvi2011,main="DVI 2011")

dif <- dvi2011-dvi1988

# par
par(mfrow=c(1,2))
hist(dvi1988,ylim=c(0,2000000))
hist(dvi2011,ylim=c(0,2000000))

# close the window

# io:
# Histogram Colored (blue and red)
hist(dvi1988, col=rgb(1,0,0,0.5),main="DVI frequencies")
hist(dvi2011, col=rgb(0,0,1,0.5), add=T)
box()
legend("topleft", c("DVI 1988", "DVI 2011"), fill=c("red", "blue"))
