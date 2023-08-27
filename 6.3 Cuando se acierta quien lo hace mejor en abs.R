#A. Crear una matriz de dos columnas la primera con el error de r y la segunda con el error absoluto de la consultora
#B. Generar medidas descritivas de estos errores
#C. Histograma



#A. Crear una matriz de dos columnas la primera con el error de r y la segunda con el error absoluto de la consultora
#Generamos una matriz 1000x2 la primera columna muesta el error abs cuando el modelo propuesto acierta y 0 cuando falla
#la segunda el error abs del modelo de la consultora cuando esta acierta y 0 cuando falla

error1<-matrix(data = 0,ncol = 2,nrow = nro.trayectorias)
for (i in 1:nro.trayectorias) {
  
  if (dif.mb.consultora.trayectorias[i] > dif.mb.suavizada2.trayectorias[i])
  {
    
    error1[i,1]<-dif.mb.suavizada2.trayectorias[i]
    error1[i,2]<-0
    
  } else{
    error1[i,1]<-0
    error1[i,2]<-dif.mb.consultora.trayectorias[i]
  }
}
error.r1<-error1[,1]
error.co1<-error1[,2]


#B. Generar medidas descritivas de estos errores

media.cuandosResmejor<- mean(error.r1[error.r1 != 0])
media.cuandoConesmejor <- mean(error.co1[error.co1 != 0])
max.cuandoResmejor<-max(error.r1)
max.cuandoConesmejor<-max(error.co1)
min.cuandoResmejor<-min(subset(error.r1, error.r1 > 0))
min.cuandoConesmejor<-min(subset(error.co1, error.co1 > 0))
mediana.cuandoResmejor <- median(error.r1[error.r1 != 0])
mediana.cuandoConesmejor <- median(error.co1[error.co1 != 0])


#C. Histograma

xmin1 <- min(min.cuandoResmejor, min.cuandoConesmejor)
xmax1<- max(max(error.r1), max(error.co1))
num_intervalos1 <- 20
ancho_intervalo1 <- (xmax1 - xmin1) / num_intervalos1


hist(error.co1[error.co1>0],breaks = seq(xmin1, xmax1, ancho_intervalo1),freq=FALSE,col = alpha("red", 0.5),xlab = "valores", ylab = "Densidad", main = "Error absoluto",cex.main = 2, cex.lab=1.5)
hist(error.r1[error.r1>0],breaks = seq(xmin1, xmax1, ancho_intervalo1),freq=FALSE, add=TRUE, col = alpha("blue", 0.5))
a1<-quantile(error.r1[error.r1>0],c(0.5))
b1<-quantile(error.co1[error.co1>0],c(0.5))
abline(v=a1,col="blue")
abline(v=b1,col="red")
legend("topright", legend = c("Método propuesto", "Método actual"), fill = c("blue", "red"))




