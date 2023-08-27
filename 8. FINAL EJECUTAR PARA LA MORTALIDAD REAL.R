#A. Carga de paquetes
#B. IMPORTAR DATOS
#C. Suavizar la mortalidad bruta con el metodo de W-H de R
#D. Quitar sesgo
#E. Enlace con las PASEM recargadas un X%
#F, Exportar datos


#A. Carga de paquetes

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


#B. IMPORTAR DATOS


library(readxl)
datos <- read_excel("datos.xlsx")
View(datos)
edad<-datos$edad
mortalidad.bruta<-datos$`mortalidad bruta hombres`
PASEM2020<-datos$`PASEM 2020 hombres`
wh_consultora<-datos$`consultora hombre`
pesos<-datos$`pesos hombres`



#c Suavizar cada simulación de la mortalidad bruta con el método de W-H de R
nro.tiempos<-57
#Definimos la matriz en la que se volcarán los datos
mortalidad.suavizada.2<-matrix(data=0,nrow=1,ncol=nro.tiempos)

  obsTable = mortalityTable.period(
  name = "trivial observed table",
    ages = 16:72,
    deathProbs = as.numeric( mortalidad.bruta),
    exposures = pesos
 )

  obsTable.smooth = whittaker.mortalityTable(obsTable,
                                             lambda = 1/2, d = 2, name.postfix = " smoothed (d=2, lambda=0.5)")
  mortalidad.suavizada.2<-obsTable.smooth@deathProbs

  
#D. Quitar sesgo
  
FACOTADOS<-mortalidad.bruta[c(11:49)]*pesos[c(11:49)]
Fsuavizada<-mortalidad.suavizada.2[c(11:49)]*pesos[c(11:49)]
sum(FACOTADOS)
sum(Fsuavizada)
coeficiente<-sum(FACOTADOS)/sum(Fsuavizada)

mortalidad.final<-mortalidad.suavizada.2*coeficiente
Ffinal<-Fsuavizada*coeficiente
Ffinal2<-mortalidad.final*pesos



#E. Enlace con las PASEM recargadas un X%

todas.las.edades<- 57

# Crear una matriz vacía con el mismo tamaño que las matrices de entrada
enlace<-function(mortalidad.final,PASEM2020){
  mortalidad.final.2 = matrix(data=0, 1, todas.las.edades)
  
    for (j in 1:todas.las.edades) {
      
      if (j <= 13) {
        mortalidad.final.2[j] <- X*PASEM2020[j]
      } else if (j == 14) {
        mortalidad.final.2[j] <- 0.75 *X* PASEM2020[j] + 0.25 * mortalidad.final[j]
      } else if (j == 15) {
        mortalidad.final.2[j] <- 0.5 *X* PASEM2020[j] + 0.5 * mortalidad.final[j]
      } else if (j == 16) {
        mortalidad.final.2[j] <- 0.25 *X*PASEM2020[j] + 0.75 * mortalidad.final[j]
      } else if (j >= 17 && j <= 42) {
        mortalidad.final.2[j] <- mortalidad.final[j]
      } else if (j == 43) {
        mortalidad.final.2[j] <- 0.75 * mortalidad.final[j] + 0.25 * X*PASEM2020[j]
      } else if (j == 44) {
        mortalidad.final.2[j]<- 0.5 *mortalidad.final[j] + 0.5 *X* PASEM2020[j]
      } else if (j == 45) {
        mortalidad.final.2[j] <- 0.25 * mortalidad.final[j] + 0.75 *X* PASEM2020[j]
      } else {
        mortalidad.final.2[j] <- X*PASEM2020[j]
      }
    }
  
  
  return(mortalidad.final.2)
  
}

tabla.segundo.orden<-enlace(mortalidad.final,PASEM2020)

tabla.segundo.orden<-as.data.frame(tabla.segundo.orden)


write.xlsx(tabla.segundo.orden,"tabla.segundo.orden.xlsx")
mortalidad.final<-as.data.frame(mortalidad.final)
write.xlsx(mortalidad.final,"mortalidad.final.xlsx")

