#A. importar la mortalidad bruta calculada anteriormente
#B importar la suavizacion de la consultora (hecha en excel)
#c Suavizar cada simulación de la mortalidad bruta con el metodo de W-H de R



#A. importar la mortalidad bruta calculada anteriormente
mortalidad.bruta<-as.data.frame( read_excel("mortalidad bruta.xlsx"))


#B importar la suavizacion de la consultora (hecha en excel)
#Aqui usamos la mortalidad bruta que hemos exportado al excel para pasar cada simulacion por el excel de la consultora
#de este modo tenemos la suavizacion de cada trayectoria y los importamos
wh_consultora<-as.data.frame( read_excel("importar mortalidad consultora.xlsx"))



#c Suavizar cada simulación de la mortalidad bruta con el metodo de W-H de R

#definimos la matriz en la que se volcaran los datos
#mortalidad.suavizada.2<-matrix(data=0,nrow=nro.trayectorias,ncol=nro.tiempos)

#bucle que suaviza la con r la mortalidad bruta y me lo pone en la matriz
# lo corto porque cada vez que lo ejecuto me sale una suavizacion muy parecida pero diferente
#for (i in 1:nro.trayectorias) {
#  obsTable = mortalityTable.period(
    #name = "trivial observed table",
   # ages = 16:72,
  #  deathProbs = as.numeric( mortalidad.bruta[i,]),
 #   exposures = pesos
 #)
  
  #obsTable.smooth = whittaker.mortalityTable(obsTable,
  #                                           lambda = 1/2, d = 2, name.postfix = " smoothed (d=2, lambda=0.5)")
 # mortalidad.suavizada.2[i,]<-obsTable.smooth@deathProbs
  
#}


#library(readxl)
#library(xlsx)
#mortalidad.suavizada.2<-as.data.frame(mortalidad.suavizada.2)
#write.xlsx (mortalidad.suavizada.2,"mortalidad suavizada con R.xlsx")
mortalidad.suavizada.2<-read_excel("mortalidad suavizada con R.xlsx")# sheetName = "Sheet 1")
mortalidad.suavizada.2<-as.data.frame(mortalidad.suavizada.2)
length(mortalidad.suavizada.2)
mortalidad.suavizada.2<-mortalidad.suavizada.2[,c(2:58)]
