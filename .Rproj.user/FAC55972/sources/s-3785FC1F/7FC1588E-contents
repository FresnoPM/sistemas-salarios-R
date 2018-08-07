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
"- Total de Hombres en IT :"
nrow(h20181)

"- Promedio salarial de Hombres en IT:"
mean(h20181$USD)


plot(h20181$Max.Cantidad.de.empleados , h20181$USD, 
     xlim = c(10,10002)
     ,xlab="Tamaño de la empresa en cantidad de empleades"
     ,ylim = c(0,15000)
     ,ylab="Salario en USD"
     ,main= "2018 - Enero - Salario en USD de Hombres en IT según \n el tamaño de la empresa para la que trabajan"  
     ,col="purple" 
     ,pch=2 
    # ,type="l"
)

"Censo enero 2018: "
"- Total de mujeres en IT :"
nrow(m20181)

"- Promedio salarial de mujeres en IT:"
mean(m20181$USD)

plot(m20181$Max.Cantidad.de.empleados , m20181$USD, 
   
     main= "2018 - Enero - Salario en USD de Mujeres en IT \n según el tamaño de la empresa para la que trabajan"  ,
     xlim = c(10,10002),
     xlab="Tamaño de la empresa en cantidad de empleades",
     ylim = c(0,15000),
     ylab="Salario en USD",
     col="orange", 
     pch=1 #type="l"
)


"Censo enero 2018: "
"- Total de personas no binarias en IT :"
nrow(o20181)

"- Promedio salarial de personas no binarias en IT:"
mean(o20181$USD)


plot(o20181$Max.Cantidad.de.empleados , o20181$USD, 
     
     
     main= "2018 - Enero - Salario en USD de personas no binarias en IT \n según el tamaño de la empresa para la que trabajan"  ,
     xlim = c(10,10002),
     xlab="Tamaño de la empresa en cantidad de empleades",
     ylim = c(0,15000),
     ylab="Salario en USD",
     col="darkgreen", 
     pch=12 #type="l"
)
boxplot( o20181$USD)

abline(h = mean(o20181$USD),  col = "blue")
text( 9000,mean(o20181$USD), floor(mean(o20181$USD)) , col = "blue", adj = c(0, -.1))

abline(h = max(o20181$USD),  col = "red")
text( 9000,max(o20181$USD), floor(max(o20181$USD)) , col = "red", adj = c(0, -.1))




S20172<-read.csv(fpath20172)
m20172<-subset(S20172, Me.identifico=="Mujer")
h20172<-subset(S20172, Me.identifico=="Hombre")
o20172<-subset(S20172, Me.identifico=="Otros")

plot(o20172$Max.Cantidad.de.empleados , o20172$USD, 
     
     
     main= "2017 - Julio - Salario en USD de personas no binarias en IT \n según el tamaño de la empresa para la que trabajan"  ,
     xlim = c(10,1002),
     xlab="Tamaño de la empresa en cantidad de empleades",
     ylim = c(0,15000),
     ylab="Salario en USD",
     col="darkgreen", 
     pch=12 #type="l"
)




plot(h20172$Max.Cantidad.de.empleados , h20172$USD, 
     
     
     main= "2017 - Julio - Salario en USD de hombres en IT \n según el tamaño de la empresa para la que trabajan"  ,
     xlim = c(10,10002),
     xlab="Tamaño de la empresa en cantidad de empleades",
     ylim = c(0,15000),
     ylab="Salario en USD",
     col="purple", 
     pch=2 #type="l"
)



plot(m20172$Max.Cantidad.de.empleados , m20172$USD, 
     
     
     main= "2017 - Julio - Salario en USD de mujeres en IT \n según el tamaño de la empresa para la que trabajan"  ,
     xlim = c(10,1002),
     xlab="Tamaño de la empresa en cantidad de empleades",
     ylim = c(0,15000),
     ylab="Salario en USD",
     col="orange", 
     pch=1 #type="l"
)

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

