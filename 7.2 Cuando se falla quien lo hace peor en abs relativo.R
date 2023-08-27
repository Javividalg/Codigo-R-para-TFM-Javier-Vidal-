#A. Crear una matriz de dos columnas la primera con el error de r y la segunda con el error absoluto relativo de la consultora
#B. Generar medidas descritivas de estos errores
#C. Histograma



#A. Crear una matriz de dos columnas la primera con el error de r y la segunda con el error absoluto de la consultora
#generamos una matriz 1000x2 la primera columna muesta el error abs relativo cuando el modelo propuesto falla y 0 cuando acierta
#la segunda el error abs relativo del modelo de la consultora cuando esta falla y 0 cuando acierta


error<-matrix(data = 0,ncol = 2,nrow = nro.trayectorias)
for (i in 1:nro.trayectorias) {
  
  if (dif.rel.mb.consultora.trayectoria[i] < dif.rel.mb.suavizada2.trayectoria[i])
  {
    
    error[i,1]<-dif.rel.mb.suavizada2.trayectoria[i]
    error[i,2]<-0
    
  } else{
    error[i,1]<-0
    error[i,2]<-dif.rel.mb.consultora.trayectoria[i]
  }
}
error.r<-error[,1]
error.co<-error[,2]


#B. Generar medidas descritivas de estos errores

media.cuandosRespeor<- mean(error.r[error.r != 0])
media.cuandoConespeor <- mean(error.co[error.co != 0])
max.cuandoRespeor<-max(error.r)
max.cuandoConespeor<-max(error.co)
min.cuandoRespeor<-min(subset(error.r, error.r > 0))
min.cuandoConespeor<-min(subset(error.co, error.co > 0))
mediana.cuandoRespeor <- median(error.r[error.r != 0])
mediana.cuandoConespeor <- median(error.co[error.co != 0])



#C. Histograma

xmin <- min(min.cuandoRespeor,min.cuandoConespeor)
xmax <- max(max(error.r), max(error.co))
num_intervalos <- 20
ancho_intervalo <- (xmax - xmin) / num_intervalos

hist(error.r[error.r>0],breaks = seq(xmin, xmax, ancho_intervalo),freq=FALSE,col = alpha("blue", 0.5),xlab = "valores", ylab = "Densidad", main = "Error absoluto relativo",cex.main = 2, cex.lab=1.5)
hist(error.co[error.co>0],breaks = seq(xmin, xmax, ancho_intervalo),freq=FALSE, add=TRUE, col = alpha("red", 0.5))
a<-quantile(error.r[error.r>0],c(0.85))
b<-quantile(error.co[error.co>0],c(0.5))
abline(v=a,col="blue")
abline(v=b,col="red")
legend("topright", legend = c("Método propuesto", "Método actual"), fill = c("blue", "red"))

