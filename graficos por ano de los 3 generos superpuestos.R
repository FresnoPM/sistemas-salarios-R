setwd("~/Brecha salarial IT")


fpath20181 = file.path("Material", "sysarmy2018-1.csv")
fpath20172 = file.path("Material", "sysarmy2017-2.csv")
fpath20171 = file.path("Material", "sysarmy2017-1.csv")
fpath20162 = file.path("Material", "sysarmy2016-2.csv")
#--------------

S20181<-read.csv(fpath20181)
m20181<-subset(S20181, Me.identifico=="Mujer")
h20181<-subset(S20181, Me.identifico=="Hombre")
o20181<-subset(S20181, Me.identifico=="Otros")


"Censo enero 2018: "


plot(h20181$Max.Cantidad.de.empleados , h20181$USD
     ,xlim = c(10,10002)
     ,xlab="Tamaño de la empresa en cantidad de empleades"
     ,ylim = c(0,15000)
     ,ylab="Salario en USD"
     ,main= "2018 - Enero - Salario en USD en IT según \n el tamaño de la empresa para la que trabajan"  
     ,col="purple" 
     ,pch=2 
)
points(m20181$Max.Cantidad.de.empleados , m20181$USD, col="orange", pch=1)
points(o20181$Max.Cantidad.de.empleados , o20181$USD, col="turquoise", pch=3)
abline(h=mean(h20181$USD), lty=2, col="purple")
abline(h=mean(m20181$USD), lty=2, col="orange")
abline(h=mean(o20181$USD), lty=2, col="turquoise")
totalHombres=paste("Total de hombres consultados: ",nrow(h20181));
promedioHombres=paste("Promedio de salario masc. en USD: ",floor(mean(h20181$USD)))
totalMujeres=paste("Total de mujeres consultadas: ",nrow(m20181));
promedioMujeres=paste("Promedio de salario fem. en USD: ",floor(mean(m20181$USD)))
totalNoBinarie=paste("Total de no binaries consultades: ",nrow(o20181));
promedioNoBinarie=paste("Promedio de salario n/b en USD: ",floor(mean(o20181$USD)))
legend("topright"
       , inset=.02
       , legend=c(totalHombres, promedioHombres, totalMujeres, promedioMujeres, totalNoBinarie, promedioNoBinarie)
       , col=c("purple", "purple", "orange","orange", "turquoise", "turquoise")
       , bg="transparent"
       , lty=1:2
       #, fill=topo.colors(3)
       #, horiz=TRUE
       , cex=0.8
)





S20172<-read.csv(fpath20172)
m20172<-subset(S20172, Me.identifico=="Mujer")
h20172<-subset(S20172, Me.identifico=="Hombre")
o20172<-subset(S20172, Me.identifico=="Otros")








plot(h20172$Max.Cantidad.de.empleados , h20172$USD
     ,xlim = c(10,10002)
     ,xlab="Tamaño de la empresa en cantidad de empleades"
     ,ylim = c(0,15000)
     ,ylab="Salario en USD"
     ,main= "2017 - Julio - Salario en USD en IT según \n el tamaño de la empresa para la que trabajan"  
     ,col="purple" 
     ,pch=2 
)
points(m20172$Max.Cantidad.de.empleados , m20172$USD, col="orange", pch=1)
points(o20172$Max.Cantidad.de.empleados , o20172$USD, col="turquoise", pch=3)
abline(h=mean(h20172$USD), lty=2, col="purple")
abline(h=mean(m20172$USD), lty=2, col="orange")
abline(h=mean(o20172$USD), lty=2, col="turquoise")
totalHombres=paste("Total de hombres consultados: ",nrow(h20172));
promedioHombres=paste("Promedio de salario masc. en USD: ",floor(mean(h20172$USD)))
totalMujeres=paste("Total de mujeres consultadas: ",nrow(m20172));
promedioMujeres=paste("Promedio de salario fem. en USD: ",floor(mean(m20172$USD)))
totalNoBinarie=paste("Total de no binaries consultades: ",nrow(o20172));
promedioNoBinarie=paste("Promedio de salario n/b en USD: ",floor(mean(o20172$USD)))
legend("topright"
       , inset=.02
       , legend=c(totalHombres, promedioHombres, totalMujeres, promedioMujeres, totalNoBinarie, promedioNoBinarie)
       , col=c("purple", "purple", "orange","orange", "turquoise", "turquoise")
       , bg="transparent"
       , lty=1:2
       #, fill=topo.colors(3)
       #, horiz=TRUE
       , cex=0.8
)




















#______________________________________

S20171<-read.csv(fpath20171)
m20171<-subset(S20171, Me.identifico=="Mujer")
h20171<-subset(S20171, Me.identifico=="Hombre")



plot(h20171$Max.Cantidad.de.empleados , h20171$USD, 
     
     
     main= "2017 - Enero - Salario en USD de hombres en IT \n según el tamaño de la empresa para la que trabajan"  ,
     xlim = c(10,1002),
     xlab="Tamaño de la empresa en cantidad de empleades",
     ylim = c(0,15000),
     ylab="Salario en USD",
     col="purple", 
     pch=2 #type="l"
)



plot(m20171$Max.Cantidad.de.empleados , m20171$USD, 
     
     
     main= "2017 - Enero - Salario en USD de mujeres en IT \n según el tamaño de la empresa para la que trabajan"  ,
     xlim = c(10,1002),
     xlab="Tamaño de la empresa en cantidad de empleades",
     ylim = c(0,15000),
     ylab="Salario en USD",
     col="orange", 
     pch=1 #type="l"
)


S20162<-read.csv(fpath20162)
m20162<-subset(S20162, Me.identifico=="Mujer")
h20162<-subset(S20162, Me.identifico=="Hombre")



plot(h20172$Max.Cantidad.de.empleados , h20172$USD, 
     
     
     main= "2016 - Enero - Salario en USD de hombres en IT \n según el tamaño de la empresa para la que trabajan"  ,
     xlim = c(10,1002),
     xlab="Tamaño de la empresa en cantidad de empleades",
     ylim = c(0,15000),
     ylab="Salario en USD",
     col="purple", 
     pch=2 #type="l"
)



plot(m20161$Max.Cantidad.de.empleados , m20161USD, 
     
     
     main= "2016 - Enero - Salario en USD de mujeres en IT \n según el tamaño de la empresa para la que trabajan"  ,
     xlim = c(10,1002),
     xlab="Tamaño de la empresa en cantidad de empleades",
     ylim = c(0,15000),
     ylab="Salario en USD",
     col="orange", 
     pch=1 #type="l"
)

