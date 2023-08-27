#A. Cálculo de las diferencias cuadráticas relativas entre la mortalidad bruta y la suavización de la consultora
#B. Cálculo de las diferencias cuadráticas relativas entre la mortalidad bruta y la suavización en R
#C. Contador 7: cuenta el nº de simulaciones que nuestra curva tiene menos diferencias con la mortalidad bruta que la de la consultora con la mortalidad bruta
#D. Contador 8: cuenta el nº de edades que nuestra curva tiene menos diferencias con la mortalidad bruta que la de la consultora con la mortalidad bruta



#A. Cálculo de las diferencias cuadráticas relativas entre la mortalidad bruta y la suavización de la consultora

#Definimos la matriz en la que se nos van a volcar as diferencias relativas de la consultora con la mortaliad bruta
dif.rel.mb.consultora.C<-matrix(data=0,nrow=nro.trayectorias1,ncol=nro.tiemposvalidacion)

#Definimos la función que es la diferencia entre los fallecidos con con la mortalidad bruta simulada y la nuestra 
# al cuadrado, entre los fallecidos de la mortalidad bruta, para cada edad para cada trayectoria

x_dif<-function(t_arg)
{
  
  x_1<- dif.mb.consultora.mat.trayectorias1[j,i]^2/Fmortalidad.bruta3[j,i]
  return(x_1)
  
  
}
# Cálculo de las diferencias relativas

for(j in 1:nro.trayectorias1)
{
  for(i in 1:nro.tiemposvalidacion)
  {
    dif.rel.mb.consultora.C[j,i]<-x_dif(i)
  }
}



#B. Cálculo de las diferencias cuadráticas relativas entre la mortalidad bruta y la suavización en R

#Definimos la matriz
dif.rel.mb.suavizada2.C<-matrix(data=0,nrow=nro.trayectorias1,ncol=nro.tiemposvalidacion)


#definimos la funcion que es la diferencia entre los fallecidos con con la mortalidad bruta simulada y la nuestra 
# al cuadrado, entre los fallecidos de la mortalidad bruta, para cada edad para cada trayectoria
x_dif1<-function(t_arg)
{
  
  x_2<- dif.mb.suavizada2.mat.trayectorias1[j,i]^2/Fmortalidad.bruta3[j,i]
  return(x_2)
}
# Cálculo de las diferencias relativas
for(j in 1:nro.trayectorias1)
{
  for(i in 1:nro.tiemposvalidacion)
  {
    dif.rel.mb.suavizada2.C[j,i]<-x_dif1(i)
  }
}





#C.Contador 7: cuenta el nº de simulaciones que nuestra curva tiene menos diferencias con la mortalidad bruta que la de la consultora con la mortalidad bruta
#Suma por trayectorias de las diferencias

dif.rel.mb.consultora.trayectoria2.C<-apply(dif.rel.mb.consultora.C , 1, sum)
dif.rel.mb.suavizada2.trayectoria2.C<-apply(dif.rel.mb.suavizada2.C, 1, sum)


####Ahora hacemos un contador cuenta el nº de veces que nuestra curva tiene mas diferencias con la mortalidad bruta simulada que la de la consultora con la mortalidad bruta simulada
contador7=0
for (i in 1:nro.trayectorias) {
  
  if (dif.rel.mb.consultora.trayectoria2.C[i] > dif.rel.mb.suavizada2.trayectoria2.C[i])
  {
    contador7=contador7+1
  } 
}



#D.Contador 8: cuenta el nº de edades que nuestra curva tiene menos diferencias con la mortalidad bruta que la de la consultora con la mortalidad bruta




###Suma por edades  de las diferencias

dif.rel.mb.consultora.edad2.C<-apply(dif.rel.mb.consultora.C, 2, sum)
dif.rel.mb.suavizada2.edad2.C<-apply(dif.rel.mb.suavizada2.C, 2, sum)

####Ahora hacemos un contador cuenta el nº de años que nuestra curva tiene mas diferencias con la mortalidad bruta que la de la consultora con la mortalidad bruta

contador8=0
for (i in 1:nro.tiemposvalidacion) {
  
  if (dif.rel.mb.consultora.edad2.C[i] > dif.rel.mb.suavizada2.edad2.C[i])
  {
    contador8=contador8+1
  } 
}
contador8



contador7
contador8

contador1
contador2
contador3
contador4
contador5
contador6
contador7
contador8

