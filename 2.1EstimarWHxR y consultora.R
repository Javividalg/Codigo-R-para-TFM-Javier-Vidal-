#A. Importar la mortalidad bruta calculada anteriormente
#B. Importar la suavización de la consultora (hecha en excel)
#c. Suavizar cada simulación de la mortalidad bruta con el método de W-H de R



#A. Importar la mortalidad bruta calculada anteriormente
mortalidad.bruta<-as.data.frame( read_excel("mortalidad bruta.xlsx"))


#B Importar la suavización de la consultora (hecha en excel)
#Aqui usamos la mortalidad bruta que hemos exportado al excel para pasar cada simulación por el excel de la consultora
#de este modo tenemos la suavización de cada trayectoria y los importamos
wh_consultora<-as.data.frame( read_excel("importar mortalidad consultora.xlsx"))



#c Suavizar cada simulación de la mortalidad bruta con el método de W-H de R

#Definimos la matriz en la que se volcarán los datos
mortalidad.suavizada.2<-matrix(data=0,nrow=nro.trayectorias,ncol=nro.tiempos)

#Bucle que suaviza la con r la mortalidad bruta y me lo pone en la matriz
for (i in 1:nro.trayectorias) {
  obsTable = mortalityTable.period(
    name = "trivial observed table",
    ages = 16:72,
    deathProbs = as.numeric( mortalidad.bruta[i,]),
    exposures = pesos
 )
  
  obsTable.smooth = whittaker.mortalityTable(obsTable,
                                             lambda = 1/2, d = 2, name.postfix = " smoothed (d=2, lambda=0.5)")
  mortalidad.suavizada.2[i,]<-obsTable.smooth@deathProbs
  
}


library(readxl)
library(xlsx)
mortalidad.suavizada.2<-as.data.frame(mortalidad.suavizada.2)
write.xlsx (mortalidad.suavizada.2,"mortalidad suavizada con R.xlsx")
mortalidad.suavizada.2<-read_excel("mortalidad suavizada con R.xlsx")
mortalidad.suavizada.2<-as.data.frame(mortalidad.suavizada.2)
length(mortalidad.suavizada.2)
mortalidad.suavizada.2<-mortalidad.suavizada.2[,c(2:58)]
