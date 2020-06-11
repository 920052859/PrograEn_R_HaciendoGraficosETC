#### Iniciando el analicis de los datos ####
rm(list = ls())
setwd(dir = "C:/Users/DANIPZA/Desktop/pylinR/Rclass/Tarea1ProgramacionR")
getwd()
Empresa <- read.table("201806_TABLA04_SICLI.txt",header = TRUE,sep = "\t",
                      col.names = c("CodEmpresa","Suministro","PuntoSuministro","Fecha","EnergíaActiva","EnergíaReactiva","Periodo"),
                      colClasses = c("factor","factor","factor", "character","numeric","numeric","character"))
class(Empresa)
str(Empresa)
dim(Empresa)
#### Conversion de la fecha ####
Empresa$Fecha # sin el formato adecuado 

help("lubridate")
library(lubridate)
help("as_date")
str(Empresa$NuevaFecha)
Empresa$Fecha <- ymd_hm(Empresa$Fecha) # guardamos los cambia en la misma linea ya que al final no nos servira con ese formato 
# Empresa <- Empresa[ ,-c(4)] # estoy pensando tambien borrar otros campos en la cual su valor es repeptitivo 
getwd()
library(dplyr) # manipulacion de datos 

# format recupera el dato 
Empresa$dia <- format(Empresa$Fecha,"%d")
Empresa$hora <- format(Empresa$Fecha,"%H")
Empresa$minutos <- format(Empresa$Fecha,"%M")