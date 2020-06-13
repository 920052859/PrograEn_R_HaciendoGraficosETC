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

#### Tabla de la suma de la Energia Activa ####

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




