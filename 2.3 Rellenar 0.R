#A Calcular los fallecidos con la mortalidad bruta
#B Sustituir los 0 de los fallecidos con la mortalidad bruta por la media entre los fallecidos con la suavización de r y la consultora para cada i,j



nro.trayectorias<-1000

#A Calcular los fallecidos con la mortalidad bruta


#Definimos la matriz que nos calcula los fallecidos a todas las edades para cada simulación
fmortalidad.bruta1<-matrix(data=0,nrow=nro.trayectorias,ncol=nro.tiempos)

#Función que devuelve los fallecidos con la mortalidad bruta para cada simulación
x_t<-function(t_arg)
{ 
  y_tmp<-mortalidad.bruta[j,i]*pesos[i]      
  x_out<- (y_tmp)
  return(x_out)
}
# Cálculo de las trayectorias
for(j in 1:nro.trayectorias)
{
  for(i in 1:nro.tiempos)
  {
    fmortalidad.bruta1[j,i]<-x_t(i)
  }
}

Fmortalidad.bruta1<-matrix(data=0,nrow=nro.trayectorias,ncol=nro.tiempos)
Fmortalidad.bruta1<-fmortalidad.bruta1



#B Sustituir los 0 de los fallecidos con la mortalidad bruta por la media entre los fallecidos con la suavización de r y la consultora para cada i,j


#Definimos la función
reemplazar_ceros_media <- function(Fmortalidad.bruta1) {
  for (i in 1:nrow(Fmortalidad.bruta1)) {
    for (j in 1:ncol(Fmortalidad.bruta1)) {
      if (Fmortalidad.bruta1[i, j] == 0) {
        if (j > 1 && j < ncol(Fmortalidad.bruta1)) {
          Fmortalidad.bruta1[i, j] <- (mortalidad.suavizada.2[i, j]*pesos[j] + wh_consultora[i, j]*pesos[j]) / 2
        } else if (j == 1) {
          Fmortalidad.bruta1[i, j] <- (mortalidad.suavizada.2[i, j]*pesos[j] + wh_consultora[i, j]*pesos[j]) / 2
        } else if (j == ncol(Fmortalidad.bruta1)) {
          Fmortalidad.bruta1[i, j] <- (mortalidad.suavizada.2[i, j]*pesos[j] + wh_consultora[i, j]*pesos[j]) / 2
        }
      }
    }
  }
  return(Fmortalidad.bruta1)
}

#Primera realización
matriz_resultante <- reemplazar_ceros_media(Fmortalidad.bruta1)
any(matriz_resultante==0)


Fmortalidad.bruta2 <- matriz_resultante

any(is.na(Fmortalidad.bruta2))

Fmortalidad.bruta2<-as.data.frame(Fmortalidad.bruta2)

