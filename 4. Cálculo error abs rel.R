
#A. Cálculo de las diferencias absolutas relativas entre la mortalidad bruta y la suavización de laconsultora
#B. Cálculo de las diferencias absolutas relativas entre la mortalidad bruta y la suavización en R
#C. Contador 5: cuenta el nº de simulaciones que nuestra curva tiene menos diferencias con la mortalidad bruta que la de la consultora con la mortalidad bruta
#D. Contador 6: cuenta el nº de edades que nuestra curva tiene menos diferencias con la mortalidad bruta que la de la consultora con la mortalidad bruta




#A. Cálculo de las diferencias absolutas relativas entre la mortalidad bruta y la suavización de la consultora

nro.trayectorias1<-1000
nro.tiemposvalidacion<-edad.max - edad.min + 1
Fmortalidad.bruta2<-Fmortalidad.bruta2[,c(edad.min:edad.max)]
#Definimos la matriz en la que se nos van a volcar as diferencias relativas de la consultora con la mortaliad bruta
dif.rel.mb.consultora<-matrix(data=0,nrow=nro.trayectorias1,ncol=nro.tiemposvalidacion)
#Definimos la funcion que el la diferencia entre los fallecidos con la mortalidad bruta simulada y con la consultora entre los fallecidos por la mortalidad bruta para cada edad para cada trayectoria
x_dif<-function(t_arg)
{
  
  x_1<- abs(dif.mb.consultora.mat.trayectorias1[j,i])/Fmortalidad.bruta3[j,i]
  return(x_1)
  
  
}

# Cálculo de las diferencias relativas

for(j in 1:nro.trayectorias1)
{
  for(i in 1:nro.tiemposvalidacion)
  {
    dif.rel.mb.consultora[j,i]<-x_dif(i)
  }
}




#B. Cálculo de las diferencias absolutas relativas entre la mortalidad bruta y la suavización en R


##Calculamos una matriz con las diferencias relativas entre los fallecidos con la mortalidad bruta simulada y la nuestra
#Definimos la matriz
dif.rel.mb.suavizada2<-matrix(data=0,nrow=nro.trayectorias1,ncol=nro.tiemposvalidacion)


#Definimos la función 
x_dif1<-function(t_arg)
{
  
  x_2<- abs(dif.mb.suavizada2.mat.trayectorias1[j,i])/Fmortalidad.bruta3[j,i]
  return(x_2)
}
# Cálculo de las diferencias relativas
for(j in 1:nro.trayectorias1)
{
  for(i in 1:nro.tiemposvalidacion)
  {
    dif.rel.mb.suavizada2[j,i]<-x_dif1(i)
  }
}





#C.Contador 5: cuenta el nº de simulaciones que nuestra curva tiene menos diferencias con la mortalidad bruta que la de la consultora con la mortalidad bruta


###Suma por trayectoria en valor absoluto de las diferencias
dif.rel.mb.consultora.trayectoria<-apply(abs(dif.rel.mb.consultora) , 1, sum)
dif.rel.mb.suavizada2.trayectoria<-apply(abs(dif.rel.mb.suavizada2), 1, sum)

contador5=0
for (i in 1:nro.trayectorias) {
  
  if (dif.rel.mb.consultora.trayectoria[i] > dif.rel.mb.suavizada2.trayectoria[i])
  {
    contador5=contador5+1
  } 
}



#D.Contador 6: cuenta el nº de edades que nuestra curva tiene menos diferencias con la mortalidad bruta que la de la consultora con la mortalidad bruta

###Suma por edades en valor absoluto de las diferencias

dif.rel.mb.consultora.edad<-apply(abs(dif.rel.mb.consultora) , 2, sum)
dif.rel.mb.suavizada2.edad<-apply(abs(dif.rel.mb.suavizada2), 2, sum)


contador6=0
for (i in 1:nro.tiemposvalidacion) {
  
  if (dif.rel.mb.consultora.edad[i] > dif.rel.mb.suavizada2.edad[i])
  {
    contador6=contador6+1
  } 
}

contador5
contador6

