#Script para analizar por Whitaker-Henderson
#A. Carga de paquetes
#B. importar datos
#c. simulacion de la mortalidad bruta
#D. exportar a un fichero excel
#ESTOS DOS ULTIMOS PASOS SOLO LOS HE EJECUTADO UNA VEZ, YA QUE SI VUELVO A SIMULAR LA MORTALIDAD BRUTA,
#TENDRÍA QUE VOLVER HA PASAR LAS NUEVAS SIMULACIONES POR LA MACRO DE LA CONSULTORA,
#Y ES UN PROCESO QUE NO ESTA AUTOMATIZADO DEL TODO
##
# Package names
packages <- c("pracma", "ptw","KernSmooth","MortalityTables")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
library("KernSmooth")
library("pracma")
library("ptw")
library("MortalityTables")


# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
##
library(xlsx)
#B. Lectura de datos

library(readxl)
datos <- read_excel("datos.xlsx")
View(datos)
edad<-datos$edad
mortalidad.bruta<-datos$`mortalidad bruta hombres`
PASEM2020<-datos$`PASEM 2020 hombres`
wh_consultora<-datos$`consultora hombre`
pesos<-datos$`pesos hombres`

#C simulacion de la mortalidad bruta
#definimos el numero de veces que queremos simular la mortalidad bruta (1000) y el numero de edades para las que simulamos (nro tiempos)
nro.trayectorias<-1000
nro.tiempos<-length(mortalidad.bruta)


#ahora simulamos la mortalidad bruta con una poisson 1000 veces, para ello lo hacemos con los fallecidos
#es decir lo multiplicamos por los pesos, despues hacemos la simulacion y despues volvemos a dividir
#por los pesos para tener la mortalidad bruta
#Fmortalidad.bruta<-mortalidad.bruta*pesos

#definimos la matriz en la que se volcaran las simulaciones de la mortalidad bruta
#MORTALIDADBRUTA<-matrix(data=0,nrow=nro.trayectorias,ncol=nro.tiempos)

#definimos la funcion
#x_t1<-function(t_arg)
#{
  #y_tmp1<-rpois(1, lambda = Fmortalidad.bruta[i])             #runif(1, min=0.9, max=1.1)                #######rnorm(1,mean=1,sd=0.1)
 #z_tmp1<-pesos[i]
  #x_out1<- (y_tmp1 / z_tmp1)
 #return(x_out1)
#}
# Calculo de las trayectorias
#for(j in 1:nro.trayectorias)
#{
 #for(i in 1:nro.tiempos)
#{
  #  MORTALIDADBRUTA[j,i]<-x_t1(i)
 #}
#}

#volcamos los resultados en un excel
#install.packages("readxl")
#library(readxl)
#library(xlsx)
#MORTALIDADBRUTA<-as.data.frame(MORTALIDADBRUTA)
#write.xlsx (MORTALIDADBRUTA,"mortalidad bruta.xlsx")

