#### Iniciando el analicis de los datos ####
rm(list = ls())
setwd(dir = "C:/Users/DANIPZA/Desktop/pylinR/Rclass/Tarea1ProgramacionR")
getwd()
Empresa <- read.table("201806_TABLA04_SICLI.txt",header = TRUE,sep = "\t",
         col.names = c("CodEmpresa","Suministro","PuntoSuministro","Fecha","EnergíaActiva","EnergíaReactiva","Periodo"),
        colClasses = c("factor","factor","factor", "character","numeric","numeric","character"))
#### Familiarizandome con la Data ####
class(Empresa)
str(Empresa)
dim(Empresa)
####Emitir los na #####
Empresa <- na.omit(Empresa)
#### Eliminando las columnas que no varian en el Mes de Junio####
#CodEmpresa = 	CEEP
#Suministro =  CL0036
#PuntoSuministro = B0047
#Periodo -> 201806 
# para que no ocupe mucho campo 
Empresa <- Empresa[, -c(1:3,7)]


help("lubridate")
library(lubridate)
help("as_date")


#### Conversion de la fecha ####
Empresa$Fecha # sin el formato adecuado 

str(Empresa$NuevaFecha)

Empresa$Fecha <- ymd_hm(Empresa$Fecha)
# guardamos los cambia en la misma linea ya que al final no nos servira con ese formato 
getwd()

library(dplyr) # manipulacion de datos 

#### Poniendp en el formato adecuado ####
# format recupera el dato que varian 

library(tidyverse)
library(lubridate)
library(datos)
help(tidyverse)
Empresa$dia <- as.day(format(Empresa$Fecha,"%d"))
hour.Empresa$hora <- format(Empresa$Fecha,"%H")
minute.Empresa$minutos <- format(Empresa$Fecha,"%M")

# Otra formama  
Empresa$dia1 <- day(Empresa$Fecha)
Empresa$hora2 <- hour(Empresa$Fecha)
Empresa$minutos3 <- minute(Empresa$Fecha)

#### Poniendo en un  formato mas simple (Fecha) ####
Empresa$Fecha <- as.Date(c(Empresa$Fecha))
###############################################
#### Tabla de la suma de la Energia Activa ####
###############################################
# Nueva tabla segun el dia
Resumen <- data.frame(  Dia = integer(), VentaTotal= double())
NuevaTabla <- data.frame()

# sumar ventas por año
for(y in unique(Empresa$dia)){
  NuevaTabla <- data.frame(Dia = y,VentaTotal=sum(Empresa[Empresa$dia==y,]$EnergíaActiva))
  Resumen <- rbind( Resumen , NuevaTabla )
}

####Grafico de las suma por dia de la Energia activa ####
plot(x = Resumen$Dia, y = Resumen$Venta,main="Suma por dia de la Energia Activa",
     xlab="Numero de Dia",ylab="Cantidad de Energia",col="blue",type = 'l')

###############################################
#### Tabla de la Media de la Energia Activa####
###############################################
# sumar ventas por año
for(y in unique(Empresa$dia)){
  NuevaTabla <- data.frame(Dia = y,MediaActiva=mean(Empresa[Empresa$dia==y,]$EnergíaActiva))
  Resumen <- rbind( Resumen , NuevaTabla )
}

# plot(x = Resumen$Dia, y = Resumen$Venta) + geom_point()
plot(x = Resumen$Dia, y = Resumen$MediaActiva,main="MEDIA POR DIAS (MES DE JUNIO)",xlab="Numero de Dia",ylab="Cantidad de Energia Media",col="red",type = "h")

###############################
#### Mediana por dias 2018 ####
###############################

# Nueva tabla segun el dia
Resumen <- data.frame(  Dia = integer(), MedianaActiva= double())
NuevaTabla <- data.frame()

# sumar ventas por año
for(y in unique(Empresa$dia)){
  NuevaTabla <- data.frame(Dia = y,MedianaActiva=median(Empresa[Empresa$dia==y,]$EnergíaActiva))
  Resumen <- rbind( Resumen , NuevaTabla )
}

par(col='blue', col.axis='darkgreen', col.lab='darkblue',
    col.main='darkgreen', col.sub='purple', bty='n')
plot(x = Resumen$Dia,y = Resumen$MedianaActiva, pch=20, cex=1, las=1.5,
     main='MEDIANA POR DIAS EN EL MES DE JUNIO', xlab='Numero de Dia',
     ylab='Mediana', sub='Suministro =  CL0036, CodEmpresa = 	CEEP, PuntoSuministro = B0047')

##########################################
####MEDIANA Por HoraS del mes de Junio####
##########################################
# Nueva tabla segun la hora
Resumen <- data.frame(  Hora = integer(), MedianaActiva= double())
NuevaTabla1 <- data.frame()

# media por hora
for(y in unique(Empresa$hora)){
  NuevaTabla1 <- data.frame(Hora = y,MedianaActiva=median(Empresa[Empresa$hora==y,]$EnergíaActiva))
  Resumen <- rbind( Resumen , NuevaTabla1 )
}
#Graficando
library(ggplot2)
ggplot(Resumen, aes(x = Hora, y = MedianaActiva, colour = Hora )) + 
  geom_point() + geom_smooth() + ggtitle("MEDIANA POR HORAS MES DE JUNIO") +labs(x = "HORA", 
                                                                                 y = "MEDIANA")

###########################################
#### Media Por Horas en el mes de Junio####
###########################################
# Nueva tabla segun el dia
Resumen <- data.frame(  Hora = integer(), MediaActiva= double())
NuevaTabla1 <- data.frame()

# sumar ventas por año
for(y in unique(Empresa$hora)){
  NuevaTabla1 <- data.frame(Hora = y,MediaActiva=mean(Empresa[Empresa$hora==y,]$EnergíaActiva))
  Resumen <- rbind( Resumen , NuevaTabla1 )
}
library(ggplot2)
ggplot(Resumen, aes(x = Hora, y = MediaActiva, colour = Hora )) + 
  geom_point() + geom_smooth() + ggtitle("       MEDIA POR HORAS MES DE JUNIO") +labs(x = "HORA", 
                                                                                      y = "MEDIA POR CADA HORA")

#------------------------------------------------------------------------------------------------------------------------------------------------------------

#### Iniciando el analicis de los datos 2019####
rm(list = ls())
setwd(dir = "C:/Users/DANIPZA/Desktop/pylinR/Tarea1ProgramacionR")
getwd()
Empresa <- read.table("201906_TABLA4.txt",header = TRUE,sep = "\t",
                      col.names = c("CodEmpresa","Suministro","PuntoSuministro","Fecha","EnergíaActiva","EnergíaReactiva","Periodo")  )
str(Empresa)  
#### Eliminando las columnas que no varian en el Mes de Junio####
#CodEmpresa = 	ATRE
#Suministro =  CL0434
#PuntoSuministro = B0310
#Periodo -> 201906 
# para que no ocupe mucho campo 
Empresa <- Empresa[, -c(1:3,7)]
str(Empresa)

Empresa$EnergíaActiva <- as.numeric(Empresa$EnergíaActiva)
Empresa$EnergíaReactiva <- as.numeric(Empresa$EnergíaReactiva)

library(lubridate)
help("as_date")

#### Convertir en formato fecha ####
#Empresa$Fecha # sin el formato adecuado 

Empresa$FechaPrimera <- as.Date(substr(Empresa$Fecha, start = 29,stop = 29),format = "%d/%m/%Y")

library(tidyverse)
library(datos)  
if( substr(Empresa$Fecha, start = 29,stop = 30) == "$PM" ){
  Empresa$Fecha1 <- as.POSIXct(Empresa$Fecha,format = "%d/%m/%Y %H:%M:%S")
  Empresa$hora <- hour(Empresa$Fecha)+12
  Empresa$minuto <- minute(Empresa$Fecha)
} else {
  Empresa$Fecha1 <- as.POSIXct(Empresa$Fecha,format = "%d/%m/%Y %H:%M:%S") 
}
Empresa$dia <- day(Empresa$Fecha1)
Empresa$año <- year(Empresa$Fecha1) +2000
as.double(Empresa$EnergíaActiva, sep=".")

substr(Empresa$EnergíaActiva, start = 0,stop = ".")  


###############################################
#### Tabla de la suma de la Energia Activa 2019 ####
###############################################
# Nueva tabla segun el dia
Resumen <- data.frame(  Dia = integer(), VentaTotal= double())
NuevaTabla <- data.frame()

# sumar ventas por año
for(y in unique(Empresa$dia)){
  NuevaTabla <- data.frame(Dia = y,VentaTotal=sum(Empresa[Empresa$dia==y,]$EnergíaActiva))
  Resumen <- rbind( Resumen , NuevaTabla )
}

####Grafico de las suma por dia de la Energia activa ####
plot(x = Resumen$Dia, y = Resumen$Venta,main="Suma por dia de la Energia Activa",
     xlab="Numero de Dia",ylab="Cantidad de Energia",col="blue",type = 'l')

###############################################
#### Tabla de la Media de la Energia Activa 2019 ####
###############################################
# sumar ventas por año
for(y in unique(Empresa$dia)){
  NuevaTabla <- data.frame(Dia = y,MediaActiva=mean(Empresa[Empresa$dia==y,]$EnergíaActiva))
  Resumen <- rbind( Resumen , NuevaTabla )
}

# plot(x = Resumen$Dia, y = Resumen$Venta) + geom_point()
plot(x = Resumen$Dia, y = Resumen$MediaActiva,main="MEDIA POR DIAS (MES DE JUNIO)",xlab="Numero de Dia",ylab="Cantidad de Energia Media",col="red",type = "h")

###############################
#### Mediana por dias 2019 ####
###############################

# Nueva tabla segun el dia
Resumen <- data.frame(  Dia = integer(), MedianaActiva= double())
NuevaTabla <- data.frame()

# sumar ventas por año
for(y in unique(Empresa$dia)){
  NuevaTabla <- data.frame(Dia = y,MedianaActiva=median(Empresa[Empresa$dia==y,]$EnergíaActiva))
  Resumen <- rbind( Resumen , NuevaTabla )
}

par(col='blue', col.axis='darkgreen', col.lab='darkblue',
    col.main='darkgreen', col.sub='purple', bty='n')
plot(x = Resumen$Dia,y = Resumen$MedianaActiva, pch=20, cex=1, las=1.5,
     main='MEDIANA POR DIAS EN EL MES DE JUNIO', xlab='Numero de Dia',
     ylab='Mediana', sub='Suministro =  CL0434, CodEmpresa = 	ATRE, PuntoSuministro = B0310')
#CodEmpresa = 	ATRE
#Suministro =  CL0434
#PuntoSuministro = B0310
#Periodo -> 201906 
###############################################
####MEDIANA Por HoraS del mes de Junio 2019####
###############################################
# Nueva tabla segun la hora
Resumen <- data.frame(  Hora = integer(), MedianaActiva= double())
NuevaTabla1 <- data.frame()

# media por hora
for(y in unique(Empresa$hora)){
  NuevaTabla1 <- data.frame(Hora = y,MedianaActiva=median(Empresa[Empresa$hora==y,]$EnergíaActiva))
  Resumen <- rbind( Resumen , NuevaTabla1 )
}
#Graficando
library(ggplot2)
ggplot(Resumen, aes(x = Hora, y = MedianaActiva, colour = Hora )) + 
  geom_point() + geom_smooth() + ggtitle("MEDIANA POR HORAS MES DE JUNIO") +labs(x = "HORA", 
                                                                                 y = "MEDIANA")




