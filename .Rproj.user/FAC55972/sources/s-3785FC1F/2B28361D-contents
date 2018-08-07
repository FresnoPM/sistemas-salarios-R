#seteo la carpeta en la cual quiero trabajar

setwd("~/Brecha salarial IT")

#rutas de archivos desde los cuales voy a sacar mis datos

fpath20181 = file.path("Material", "sysarmy2018-1.csv")
fpath20172 = file.path("Material", "sysarmy2017-2.csv")
fpath20171 = file.path("Material", "sysarmy2017-1.csv")
fpath20162 = file.path("Material", "sysarmy2016-2.csv")

# decido qué colores voy a usar para cada subset

o.color <- "turquoise"
h.color <- "purple"
m.color <- "orange"

#_________________________________
# Censo 2018 Enero

#leo del archivo correspondiente
S20181<-read.csv(fpath20181)

#genero 3 subsets según los géneros del dataframe
m20181<-subset(S20181, Me.identifico=="Mujer")
h20181<-subset(S20181, Me.identifico=="Hombre")
o20181<-subset(S20181, Me.identifico=="Otros")

# Datos
# mujeres
m20181.cant <- m20181$Max.Cantidad.de.empleados #453
m20181.maximo <- max(summary(factor(m20181.cant)))
m20181.minimo <- min(summary(factor(m20181.cant)))

m20181.total.nro<-nrow(m20181)
m20181.total.leg<-paste("Total de mujeres consultadas: ",m20181.total.nro)
m20181.promedio.nro<-floor(mean(m20181$USD))
m20181.promedio.leg<-paste("Promedio de salario fem. en USD: ",m20181.promedio.nro)

# hombres
h20181.cant <- h20181$Max.Cantidad.de.empleados #3482
h20181.maximo <- max(summary(factor(h20181.cant)))
h20181.minimo <- min(summary(factor(h20181.cant)))

h20181.total.nro<-nrow(h20181)
h20181.total.leg<-paste("Total de hombres consultados: ",h20181.total.nro)
h20181.promedio.nro<-floor(mean(h20181$USD))
h20181.promedio.leg<-paste("Promedio de salario masc. en USD: ",h20181.promedio.nro)

# no binaries
o20181.cant <- o20181$Max.Cantidad.de.empleados #14
o20181.maximo <- max(summary(factor(o20181.cant)))
o20181.minimo <- min(summary(factor(o20181.cant)))

o20181.total.nro<-nrow(o20181)
o20181.total.leg<-paste("Total de no binaries consultades: ",o20181.total.nro)
o20181.promedio.nro<-floor(mean(o20181$USD))
o20181.promedio.leg<-paste("Promedio de salario n/b en USD: ",o20181.promedio.nro)



#genero el plot base, elijo el muestreo más nutrido, en este caso hombres

plot(h20181$Max.Cantidad.de.empleados , h20181$USD
     ,xlim = c(10,10002)
     ,xlab="Tamaño de la empresa en cantidad de empleades"
     ,ylim = c(0,15000)
     ,ylab="Salario en USD"
     ,main= "2018 - Enero - Salario en USD en IT según \n el tamaño de la empresa para la que trabajan"  
     ,col=h.color
     ,pch=2 
)

#superpongo los puntos de las muestras de mujeres y no binaries
points(m20181$Max.Cantidad.de.empleados , m20181$USD, col=m.color, pch=1)
points(o20181$Max.Cantidad.de.empleados , o20181$USD, col=o.color, pch=3)

#trazo los promedios
abline(h=h20181.promedio.nro, lty=2, col=h.color)
abline(h=m20181.promedio.nro, lty=2, col=m.color)
abline(h=o20181.promedio.nro, lty=2, col=o.color)

#escribo los textos explicativos para puntos y líneas
legend("topright"
       , inset=.02
       , legend=c(h20181.total.leg, h20181.promedio.leg, m20181.total.leg, m20181.promedio.leg, o20181.total.leg, o20181.promedio.leg)
       , col=c(h.color, h.color, m.color, m.color, o.color, o.color)
       , bg="transparent"
       , lty=1:2
       #, fill=topo.colors(3)
       #, horiz=TRUE
       , cex=0.8
)

#_______________________________________
# Cantidad de empleades por tamaño de empresa

# plots
par(mfrow=c(3,1))

barplot(
  table(h20181.cant)
  , main="Cantidad de hombres trabajando en empresas  \n según su tamaño hasta enero del 2018"
  , xlab="Tamaño de empresas en cantidad de empleades"
)
abline(  h = h20181.maximo , col=h.color, lty=1 , lwd = 2 )
abline(  h = h20181.minimo , col=h.color, lty=2 , lwd = 2)
legend(
  "topright"
  , inset=.03
  , legend = c( paste("máximo: ",h20181.maximo), paste("mínimo: ",h20181.minimo))
  , col = h.color
  , lty = c(1:2)
  , lwd = 2
)

barplot(
  table(m20181.cant)
  , main="Cantidad de mujeres trabajando en empresas  \n según su tamaño hasta enero del 2018"
  , xlab="Tamaño de empresas en cantidad de empleades"
)
abline(  h = m20181.maximo , col=m.color, lty=1 , lwd = 2 )
abline(  h = m20181.minimo , col=m.color, lty=2 , lwd = 2)
legend(
  "topright"
  , inset=.03
  , legend = c(  paste("máximo: ",m20181.maximo), paste("mínimo: ",m20181.minimo))
  , col = m.color
  , lty = c(1:2)
  , lwd = 2
)

barplot(
  table(o20181.cant)
  , main="Cantidad de personas no binarias trabajando en empresas \n según su tamaño hasta enero del 2018"
  , xlab="Tamaño de empresas en cantidad de empleades"
)

abline(  h = o20181.maximo , col=o.color, lty=1 , lwd = 2)
abline(  h = o20181.minimo , col=o.color, lty=2 , lwd = 2)
legend(
  "topright"
  , inset=.03
  , legend = c(paste("máximo: ",o20181.maximo), paste("mínimo: ",o20181.minimo))
  , col = o.color
  , lty = c(1:2)
  , lwd = 2
)







#______________________________________________
# Censo julio 2017
# Datos


S20172<-read.csv(fpath20172)
m20172<-subset(S20172, Me.identifico=="Mujer")
h20172<-subset(S20172, Me.identifico=="Hombre")
o20172<-subset(S20172, Me.identifico=="Otros")

m20172.cant <- m20172$Max.Cantidad.de.empleados #453

m20172.maximo <- max(summary(factor(m)))
m20172.minimo <- min(summary(factor(m)))

m20172.total.nro<-nrow(m20172)
m20172.total.leg<-paste("Total de mujeres consultadas: ",m20172.total.nro)
m20172.promedio.nro<-floor(mean(m20172$USD))
m20172.promedio.leg<-paste("Promedio de salario fem. en USD: ",m20172.promedio.nro)

h20172.cant <- h20172$Max.Cantidad.de.empleados #3482

h20172.maximo <- max(summary(factor(h)))
h20172.minimo <- min(summary(factor(h)))


h20172.total.nro<-nrow(h20172)
h20172.total.leg<-paste("Total de hombres consultados: ",h20172.total.nro)
h20172.promedio.nro<-floor(mean(h20172$USD))
h20172.promedio.leg<-paste("Promedio de salario masc. en USD: ",h20172.promedio.nro)


o20172.cant <- o20172$Max.Cantidad.de.empleados #14

o20172.maximo <- max(summary(factor(o)))
o20172.minimo <- min(summary(factor(o)))


o20172.total.nro<-nrow(o20172)
o20172.total.leg<-paste("Total de no binaries consultades: ",o20172.total.nro)
o20172.promedio.nro<-floor(mean(o20172$USD))
o20172.promedio.leg<-paste("Promedio de salario n/b en USD: ",o20172.promedio.nro)

plot(h20172$Max.Cantidad.de.empleados , h20172$USD, 
     
     
     main= "2017 - Julio - Salario en USD en IT \n según el tamaño de la empresa para la que trabajan"  ,
     xlim = c(10,1002),
     xlab="Tamaño de la empresa en cantidad de empleades",
     ylim = c(0,15000),
     ylab="Salario en USD",
     col=h.color, 
     pch=2 
)

points(m20172$Max.Cantidad.de.empleados , m20172$USD, col=m.color, pch=1)
points(o20172$Max.Cantidad.de.empleados , o20172$USD, col=o.color, pch=3)
abline(h=h20172.promedio.nro, lty=2, col=h.color)
abline(h=m20172.promedio.nro, lty=2, col=m.color)
abline(h=o20172.promedio.nro, lty=2, col=o.color)
legend("topright"
       , inset=.02
       , legend=c(h20172.total.leg, h20172.promedio.leg, m20172.total.leg, m20172.promedio.leg, o20172.total.leg, o20172.promedio.leg)
       , col=c(h.color, h.color, m.color, m.color, o.color, o.color)
       , bg="transparent"
       , lty=1:2
       #, fill=topo.colors(3)
       #, horiz=TRUE
       , cex=0.8
)


#_______________________________________
# Cantidad de empleades por tamaño de empresa

# plots
par(mfrow=c(3,1))

barplot(
  table(h20172.cant)
  , main="Cantidad de hombres trabajando en empresas  \n según su tamaño hasta julio del 2017"
  , xlab="Tamaño de empresas en cantidad de empleades"
)
abline(  h = h20172.maximo , col=h.color, lty=1 , lwd = 2 )
abline(  h = h20172.minimo , col=h.color, lty=2 , lwd = 2)
legend(
  "topright"
  , inset=.03
  , legend = c(  paste("máximo: ",h20172.maximo), paste("mínimo: ",h20172.minimo))
  , col = h.color
  , lty = c(1:2)
  , lwd = 2
)

barplot(
  table(m20172.cant)
  , main="Cantidad de mujeres trabajando en empresas  \n según su tamaño hasta julio del 2017"
  , xlab="Tamaño de empresas en cantidad de empleades"
)
abline(  h = m20172.maximo , col=m.color, lty=1 , lwd = 2 )
abline(  h = m20172.minimo , col=m.color, lty=2 , lwd = 2)
legend(
  "topright"
  , inset=.03
  , legend = c( paste("máximo: ",m20172.maximo), paste("mínimo: ",m20172.minimo))
  , col = m.color
  , lty = c(1:2)
  , lwd = 2
)


barplot(
  table(o20172.cant)
  , main="Cantidad de personas no binarias trabajando en empresas \n según su tamaño hasta julio del 2017"
  , xlab="Tamaño de empresas en cantidad de empleades"
)

abline(  h = o20172.maximo , col=o.color, lty=1 , lwd = 2)
abline(  h = o20172.minimo , col=o.color, lty=2 , lwd = 2)
legend(
  "topright"
  , inset = .03
  , legend = c(  paste("máximo: ",o20172.maximo), paste("mínimo: ",o20172.minimo))
  , col = o.color
  , lty = c(1:2)
  , lwd = 2
)






#______________________________________________
# Censo enero 2017

S20171<-read.csv(fpath20171)
m20171<-subset(S20171, Me.identifico=="Mujer")
h20171<-subset(S20171, Me.identifico=="Hombre")

# Datos

m20171.cant <- m20171$Max.Cantidad.de.empleados #453
m20171.maximo <- max(summary(factor(m20171.cant )))
m20171.minimo <- min(summary(factor(m20171.cant )))
m20171.total.nro<-nrow(m20171)
m20171.promedio.nro<-floor(mean(m20171$USD))
m20171.total.leg<-paste("Total de mujeres consultadas: ",m20171.total.nro)
m20171.promedio.leg<-paste("Promedio de salario fem. en USD: ",m20171.promedio.nro)

h20171.cant <- h20171$Max.Cantidad.de.empleados #3482
h20171.maximo <- max(summary(factor(h20171.cant )))
h20171.minimo <- min(summary(factor(h20171.cant )))
h20171.total.nro<-nrow(h20171)
h20171.total.leg<-paste("Total de hombres consultados: ",h20171.total.nro)
h20171.promedio.nro<-floor(mean(h20171$USD))
h20171.promedio.leg<-paste("Promedio de salario masc. en USD: ",h20171.promedio.nro)

# plots

plot(h20171$Max.Cantidad.de.empleados , h20171$USD, 
     
     main= "2017 - Enero - Salario en USD en IT \n según el tamaño de la empresa para la que trabajan"  ,
     xlim = c(10,1002),
     xlab="Tamaño de la empresa en cantidad de empleades",
     ylim = c(0,15000),
     ylab="Salario en USD",
     col=h.color, 
     pch=2 
)

points(m20171$Max.Cantidad.de.empleados , m20171$USD, col=m.color, pch=1)

abline(h=h20171.promedio.nro, lty=2, col=h.color)
abline(h=m20171.promedio.nro, lty=2, col=m.color)
legend("topright"
       , inset=.02
       , legend=c(h20171.total.leg, h20171.promedio.leg, m20171.total.leg, m20171.promedio.leg)
       , col=c(h.color, h.color, m.color, m.color)
       , bg="transparent"
       , lty=1:2
       , cex=0.8
)

#_______________________________________
# Cantidad de empleades por tamaño de empresa


# plots
par(mfrow=c(2,1))

barplot(
  table(h20171.cant)
  , main="Cantidad de hombres trabajando en empresas  \n según su tamaño hasta enero del 2017"
  , xlab="Tamaño de empresas en cantidad de empleades"
)
abline(  h = h20171.maximo , col=h.color, lty=1 , lwd = 2 )
abline(  h = h20171.minimo , col=h.color, lty=2 , lwd = 2)
legend(
  "topright"
  , inset=.03
  , legend = c(  paste("máximo: ",h20171.maximo), paste("mínimo: ",h20171.minimo))
  , col = h.color
  , lty = c(1:2)
  , lwd = 2
)

barplot(
  table(m20171.cant)
  , main="Cantidad de mujeres trabajando en empresas  \n según su tamaño hasta enero del 2017"
  , xlab="Tamaño de empresas en cantidad de empleades"
)
abline(  h = m20171.maximo , col=m.color, lty=1 , lwd = 2 )
abline(  h = m20171.minimo , col=m.color, lty=2 , lwd = 2)
legend(
  "topright"
  , inset=.03
  , legend = c( paste("máximo: ",m20171.maximo), paste("mínimo: ",m20171.minimo))
  , col = m.color
  , lty = c(1:2)
  , lwd = 2
)


#________________________________________
# Censo julio 2016
# Datos
S20162<-read.csv(fpath20162)
m20162<-subset(S20162, Me.identifico=="Mujer")
h20162<-subset(S20162, Me.identifico=="Hombre")

m20162.cant <- m20162$Max.Cantidad.de.empleados #453

m20162.maximo <- max(summary(factor(m20162.cant)))
m20162.minimo <- min(summary(factor(m20162.cant)))
m20162.total.nro<-nrow(m20162)
m20162.total.leg<-paste("Total de mujeres consultadas: ",m20162.total.nro)
m20162.promedio.nro<-floor(mean(m20162$USD))
m20162.promedio.leg<-paste("Promedio de salario fem. en USD: ",m20162.promedio.nro)

h20162.cant <- h20162$Max.Cantidad.de.empleados #3482

h20162.maximo <- max(summary(factor(h20162.cant)))
h20162.minimo <- min(summary(factor(h20162.cant)))
h20162.total.nro<-nrow(h20162)
h20162.total.leg<-paste("Total de hombres consultados: ",h20162.total.nro)
h20162.promedio.nro<-floor(mean(h20162$USD))
h20162.promedio.leg<-paste("Promedio de salario masc. en USD: ",h20162.promedio.nro)




#_______________________________________
# Plots
# salario en usd por tamaño de empresa

plot(h20162$Max.Cantidad.de.empleados , h20162$USD, 
     
     
     main= "2016 - Julio - Salario en USD en IT \n según el tamaño de la empresa para la que trabajan"  ,
     xlim = c(10,1002),
     xlab="Tamaño de la empresa en cantidad de empleades",
     ylim = c(0,15000),
     ylab="Salario en USD",
     col=h.color, 
     pch=2 #type="l"
)


points(m20162$Max.Cantidad.de.empleados , m20162$USD, col=m.color, pch=1)

abline(h=h20162.promedio.nro, lty=2, col=h.color)
abline(h=m20162.promedio.nro, lty=2, col=m.color)
legend("topright"
       , inset=.02
       , legend=c(h20162.total.leg, h20162.promedio.leg, m20162.total.leg, m20162.promedio.leg)
       , col=c(h.color, h.color, m.color, m.color)
       , bg="transparent"
       , lty=1:2
       #, fill=topo.colors(3)
       #, horiz=TRUE
       , cex=0.8
)


#_______________________________________
# Cantidad de empleades por tamaño de empresa

par(mfrow=c(2,1))

barplot(
  table(h20162.cant)
  , main="Cantidad de hombres trabajando en empresas  \n según su tamaño hasta julio del 2016"
  , xlab="Tamaño de empresas en cantidad de empleades"
)
abline(  h = h20162.maximo , col=h.color, lty=1 , lwd = 2 )
abline(  h = h20162.minimo , col=h.color, lty=2 , lwd = 2)
legend(
  "topright"
  , inset=.03
  , legend = c(  paste("máximo: ",h20162.maximo), paste("mínimo: ",h20162.minimo))
  , col = h.color
  , lty = c(1:2)
  , lwd = 2
)

barplot(
  table(m)
  , main="Cantidad de mujeres trabajando en empresas  \n según su tamaño hasta julio del 2016"
  , xlab="Tamaño de empresas en cantidad de empleades"
)
abline(  h = m20162.maximo , col=m.color, lty=1 , lwd = 2 )
abline(  h = m20162.minimo , col=m.color, lty=2 , lwd = 2)
legend(
  "topright"
  , inset=.03
  , legend = c( paste("máximo: ",m20162.maximo), paste("mínimo: ",m20162.minimo))
  , col = m.color
  , lty = c(1:2)
  , lwd = 2
)

#________________________________________________

# comparacion histórica de promedios salariales

m.salarioshistoricos<-c(m20162.promedio.nro, m20171.promedio.nro, m20172.promedio.nro, m20181.promedio.nro )
h.salarioshistoricos<-c(h20162.promedio.nro, h20171.promedio.nro, h20172.promedio.nro, h20181.promedio.nro )
o.salarioshistoricos<-c(0,0, o20172.promedio.nro, o20181.promedio.nro )
fechas<-c("Julio 2016", "Enero 2017", "Julio 2017", "Enero 2018")
salarioshistoricos.promedios <-  data.frame( cbind( m.salarioshistoricos, h.salarioshistoricos, o.salarioshistoricos))
rownames(salarioshistoricos.promedios)<-fechas
colnames(salarioshistoricos.promedios)<-c("Mujeres", "Hombres", "NoBinaries")


plot(salarioshistoricos.promedios$Hombres
  , ylab = "Salario promedio en USD"
  , xlab = "Fechas"
  , ylim = c(150,2500)
  , type = "l"
  , col= h.color
  , lwd = 2
  , xaxt ="n"
  , main = "Promedio Salarial en USD según género autopercibido \n en los últimos 4 semestres"
)

axis(1, tick = FALSE, at=c(1:4), labels=fechas)
lines(salarioshistoricos.promedios$Mujeres, col = m.color, lwd = 2)
lines(salarioshistoricos.promedios$NoBinaries, col = o.color, lwd = 2)




text(
  c(1:4)
  , salarioshistoricos.promedios$Hombres[c(1:4)]+100
  , labels = salarioshistoricos.promedios$Hombres[c(1:4)]
  #, col = h.color
  , cex = .8
)
text(
  c(1:4)
  , salarioshistoricos.promedios$Mujeres[c(1:4)]+10
  , labels = salarioshistoricos.promedios$Mujeres[c(1:4)]
  , cex = .8
)

text(
  c(3,4)
  , salarioshistoricos.promedios$NoBinaries[c(3:4)]-30
  , labels = salarioshistoricos.promedios$NoBinaries[c(3:4)]
  , cex = .8
)

