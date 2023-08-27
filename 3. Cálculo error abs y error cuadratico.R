#A. Calcular las diferencias entre los fallecidos calculados con la mortalidad bruta y los fallecidos con la consultora
#B. Calcular las diferencias entre los fallecidos calculados con la mortalidad bruta y los fallecidos con la suavización de R
#C. Acotamos las matrices que hemos generado
#D. Contador 1: cuenta el nº de trayectorias que el error absoluto de nuestra suavización es menor que el error absoluto de la consultora (nos ajustamos mejor)
#E. Contador 2: cuenta el nº de edades que el error absoluto de nuesta suavización es menor que el error absoluto de la consultora (nos ajustamos mejor)
#F. Contador 3: cuenta el nº de trayectorias que el error cuadratico de nuestra suavización es menor que el error cuadrático de la consultora (nos ajustamos mejor)
#G. Contador 4: cuenta el nº de edades que el error cuadratico de nuesta suavización es menor que el error cuadrático de la consultora (nos ajustamos mejor)




#A. Calcular las diferencias entre los fallecidos calculados con la mortalidad bruta y los fallecidos con la consultora
#1º Calcular los fallecidos con la consultora
#Definimos la matriz en la que se volcarán lo fallecidos calculados suavización de la consultora
Fconsultorax<-matrix(data=0,nrow=nro.trayectorias,ncol=nro.tiempos)

#Función 
x_t<-function(t_arg)
{ 
  z_tmp<-wh_consultora[j,i]*pesos[i] 
  x_out<- ( z_tmp)
  return(x_out)
}
# Cálculo de las trayectorias
for(j in 1:nro.trayectorias)
{
  for(i in 1:nro.tiempos)
  {
    Fconsultorax[j,i]<-x_t(i)
  }
}

Fconsultora<-Fconsultorax[,c(edad.min:edad.max)]


#Definimos la matriz en la que se volcarán las diferencias de fallecidos entre las simulaciones de la mortalidad bruta y su suavización de la consultora
dif.mb.consultora.mat.trayectorias<-matrix(data=0,nrow=nro.trayectorias,ncol=nro.tiemposvalidacion)

#Función que devuelve diferencias de fallecidos entre las simulaciones de la mortalidad bruta y su suavización de la consultora para cada simulación j y cada edad i
x_t<-function(t_arg)
{ 
  y_tmp<-Fmortalidad.bruta3[j,i]       
  z_tmp<-Fconsultora[j,i]
  x_out<- (y_tmp - z_tmp)
  return(x_out)
}
# Cálculo de las trayectorias
for(j in 1:nro.trayectorias)
{
  for(i in 1:nro.tiemposvalidacion)
  {
    dif.mb.consultora.mat.trayectorias[j,i]<-x_t(i)
  }
}



#B. Calcular las diferencias entre los fallecidos calculados con la mortalidad bruta y los fallecidos con la suavización de R
#Definimos la matriz en la que se volcarán las diferencias de fallecidos entre las simulaciones de la mortalidad bruta y su suavización de R

dif.mb.suavizada2.mat.trayectorias<-matrix(data=0,nrow=nro.trayectorias,ncol=nro.tiemposvalidacion)



#Función que devuelve la diferencia entre la simulación de los fallecidos y los de R para cada simulación para cada  edad i
x_t<-function(t_arg)
{ 
  y_tmp<-Fmortalidad.bruta3[j,i]       
  z_tmp<- Fsuavizada.2.sinsesgo [j,i]
  x_out<- (y_tmp - z_tmp)
  return(x_out)
}
# Cálculo de las trayectorias
for(j in 1:nro.trayectorias)
{
  for(i in 1:nro.tiemposvalidacion)
  {
    dif.mb.suavizada2.mat.trayectorias[j,i]<-x_t(i)
  }
}





dif.mb.consultora.mat.trayectorias1<-dif.mb.consultora.mat.trayectorias
dif.mb.suavizada2.mat.trayectorias1<-dif.mb.suavizada2.mat.trayectorias



#D. Contador 1: cuenta el nº de trayectorias que el error absoluto de nuestra suavización es menor que el error absoluto de la consultora (nos ajustamos mejor)
#############Primero hacemos la suma de los valores absolutos para cada trayectoria
dif.mb.consultora.trayectorias<-apply(abs(dif.mb.consultora.mat.trayectorias1), 1, sum)
dif.mb.suavizada2.trayectorias<-apply(abs(dif.mb.suavizada2.mat.trayectorias1), 1, sum)

####Ahora hacemos un contador cuenta el nº de veces que nuestra curva tiene menos diferencias con la simulación que la de la consultora con la simulación
contador1=0
for (i in 1:nro.trayectorias) {
  
  if (dif.mb.consultora.trayectorias[i] > dif.mb.suavizada2.trayectorias[i])
  {
    contador1=contador1+1
  } 
}



#E. Contador 2: cuenta el nº de edades que el error absoluto de nuesta suavización es menor que el error absoluto de la consultora (nos ajustamos mejor)

nro.tiemposvalidacion<-edad.max - edad.min +1

#Suma en valor absoluto de las diferencias a cada edad
#############Primero hacemos la suma de los valores absolutos para cada edad
dif.mb.consultora.edad<-apply(abs(dif.mb.consultora.mat.trayectorias1), 2, sum)
dif.mb.suavizada2.edad<-apply(abs(dif.mb.suavizada2.mat.trayectorias1), 2, sum)

####Ahora hacemos un contador cuenta el nº de años que nuestra curva tiene menos diferencias con la simulación que la de la consultora con la simulación

contador2=0
for (i in 1:nro.tiemposvalidacion) {
  
  if (dif.mb.consultora.edad[i] > dif.mb.suavizada2.edad[i])
  {
    contador2=contador2+1
  } 
}
contador2





#F. Contador 3: cuenta el nº de trayectorias que el error cuadrático de nuestra suavización es menor que el error cuadrático de la consultora (nos ajustamos mejor)
#############Primero hacemos la suma de los valores al cuadrado para cada trayectoria

dif.mb.consultora.trayectorias2<-apply(dif.mb.consultora.mat.trayectorias1^2, 1, sum)
dif.mb.suavizada2.trayectorias2<-apply(dif.mb.suavizada2.mat.trayectorias1^2, 1, sum)


####Ahora hacemos un contador cuenta el nº de veces que nuestra curva tiene menos diferencias con la simulaciÓn que la de la consultora con la simulaciÓn
contador3=0
for (i in 1:nro.trayectorias) {
  
  if (dif.mb.consultora.trayectorias2[i] > dif.mb.suavizada2.trayectorias2[i])
  {
    contador3=contador3+1
  } 
}
contador3


#G. contador 4: cuenta el nº de edades que el error cuadrático de nuesta suavización es menor que el error cuadrático de la consultora (nos ajustamos mejor)
#############Primero hacemos la suma de los valores cuadráticos para cada edad


dif.mb.consultora.edad2<-apply(dif.mb.consultora.mat.trayectorias1^2, 2, sum)
dif.mb.suavizada2.edad2<-apply(dif.mb.suavizada2.mat.trayectorias1^2, 2, sum)


####Ahora hacemos un contador cuenta el nº de años que nuestra curva tiene menos diferencias con la simulaciÓn que la de la consultora con la simulaciÓn

contador4=0
for (i in 1:nro.tiemposvalidacion) {
  
  if (dif.mb.consultora.edad2[i] > dif.mb.suavizada2.edad2[i])
  {
    contador4=contador4+1
  } 
}
contador4

contador1
contador2
contador3
contador4


