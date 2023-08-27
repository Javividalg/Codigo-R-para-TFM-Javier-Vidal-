#A Importar mortalidad
#B Enlace de las tablas con las PASEM



#A Importar mortalidad


#Importamos la mortalidad bruta del excel
mortalidad.bruta<-as.data.frame( read_excel("mortalidad bruta.xlsx"))

#Importar la mortalidad suavizada consultora
wh_consultorax<-as.data.frame( read_excel("importar mortalidad consultora.xlsx"))


#Importar mortalidad suavizada en R

mortalidad.suavizada.2x<-as.data.frame( read_excel("mortalidad suavizada con R.xlsx"))






#B Enlace de las tablas con las PASEM UN PORCENTAJE X DE LAS PASEM 2020

# Crear una matriz vacía con el mismo tamaño que las matrices de entrada
enlace<-function(wh_consultorax,PASEM2020){
  wh_consultora = matrix(data=0, nro.trayectorias, nro.tiempos)
  
  for (i in 1:nro.trayectorias) {
    for (j in 1:nro.tiempos) {
      
      if (j <= 13) {
        wh_consultora[i, j] <- X*PASEM2020[j]
      } else if (j == 14) {
        wh_consultora[i, j] <- 0.75 *X* PASEM2020[j] + 0.25 * wh_consultorax[i, j]
      } else if (j == 15) {
        wh_consultora[i, j] <- 0.5 *X* PASEM2020[j] + 0.5 * wh_consultorax[i, j]
      } else if (j == 16) {
        wh_consultora[i, j] <- 0.25 *X*PASEM2020[j] + 0.75 * wh_consultorax[i, j]
      } else if (j >= 17 && j <= 42) {
        wh_consultora[i, j] <- wh_consultorax[i, j]
      } else if (j == 43) {
        wh_consultora[i, j] <- 0.75 * wh_consultorax[i, j] + 0.25 * X*PASEM2020[j]
      } else if (j == 44) {
        wh_consultora[i, j] <- 0.5 * wh_consultorax[i, j] + 0.5 *X* PASEM2020[j]
      } else if (j == 45) {
        wh_consultora[i, j] <- 0.25 * wh_consultorax[i, j] + 0.75 *X* PASEM2020[j]
      } else {
        wh_consultora[i, j] <- X*PASEM2020[j]
      }
    }
  }
  
  return(wh_consultora)
  
}

wh_consultoraxx<-enlace(wh_consultorax,PASEM2020)

mortalidad.suavizada.2xx<-enlace(mortalidad.suavizada.2x,PASEM2020)


wh_consultora<-wh_consultoraxx
mortalidad.suavizada.2<-mortalidad.suavizada.2xx





