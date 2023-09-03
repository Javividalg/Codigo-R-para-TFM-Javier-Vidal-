#A. Calcular el nº de fallecidos calculados con las suavización en R y los expuestos
#B. Crear el coeficiente
#C. Cálculo de los fallecidos sin sesgo




#A. calcular el nº de fallecidos calculados con la suavización en R y los expuestos
#Definimos la matriz en la que se volcarán los fallecidos

Fsuavizada.2x<-matrix(data=0,nrow=nro.trayectorias,ncol=nro.tiempos)



#Función que devuelve los fallecidos
x_t<-function(t_arg)
{ 
  z_tmp<- mortalidad.suavizada.2[j,i]*pesos[i] 
  x_out<- ( z_tmp)
  return(x_out)
}
# Cálculo de las trayectorias
for(j in 1:nro.trayectorias)
{
  for(i in 1:nro.tiempos)
  {
    Fsuavizada.2x[j,i]<-x_t(i)
  }
}


#B. Crear el coeficiente
# Acotamos las matrices que hemos generado
################################IMPORTANTE PONER EL RANGO DE EDADES QUE QUEREMOS################
edad.min<-11
edad.max<-49
nro.tiemposvalidacion<-edad.max - edad.min +1 


Fmortalidad.bruta3<-Fmortalidad.bruta2[,c(edad.min:edad.max)]
Fsuavizada.2<-Fsuavizada.2x[,c(edad.min:edad.max)]

Fmortalidad.bruta3<-as.data.frame(Fmortalidad.bruta3)


#############Primero hacemos la suma para cada trayectoria
FTmortalidad.bruta3<-apply(Fmortalidad.bruta3, 1, sum)
FTsuavizada.2<-apply(Fsuavizada.2, 1, sum)
#Creamos el coeficiente por el que tenemos que multiplicar nuesta mortalidad suavizada en r
#siendo la suma los fallecidos con la mortalidad bruta simulada para cada trayectoria 
#entre la suma de los fallecidos con la suavizacion de R
coeficiente<-FTmortalidad.bruta3/FTsuavizada.2
coeficiente <- matrix(rep(coeficiente, times = nro.tiemposvalidacion), nrow = 1000)


#C. Cálculo de los fallecidos sin sesgo

#Definimos la matriz de los fallecidos sin sesgo
Fsuavizada.2.sinsesgo<-matrix(data=0,nrow=nro.trayectorias,ncol=nro.tiemposvalidacion)

#Función que multiplica los fallecidos con la suavización de R por el coeficiente
x_t<-function(t_arg)
{ 
  z_tmp<-Fsuavizada.2[j,i]*coeficiente[j] 
  x_out<- (z_tmp)
  return(x_out)
}
# Cálculo de las trayectorias
for(j in 1:nro.trayectorias)
{
  for(i in 1:nro.tiemposvalidacion)
  {
    Fsuavizada.2.sinsesgo[j,i]<-x_t(i)
  }
}



FTsuavizada.2.sinsesgo<-apply(Fsuavizada.2.sinsesgo, 1, sum)



#resultados
Fmortalidad.bruta3
Fsuavizada.2.sinsesgo

