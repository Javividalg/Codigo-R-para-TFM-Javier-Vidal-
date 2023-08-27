#A. Medidas descritivas del error absoluto relativo
#B. Histograma de los errores absolutos relativos sin tener en cuenta quien lo hace mejor ni peor



#A. Medidas descritivas del error absoluto relativo

mean(dif.rel.mb.suavizada2.trayectoria)
mean(dif.rel.mb.consultora.trayectoria)
max(dif.rel.mb.suavizada2.trayectoria)
max(dif.rel.mb.consultora.trayectoria)
min(dif.rel.mb.suavizada2.trayectoria)
min(dif.rel.mb.consultora.trayectoria)
median(dif.rel.mb.suavizada2.trayectoria)
median(dif.rel.mb.consultora.trayectoria)



#B. Histograma de los errores absolutos relativos sin tener en cuenta quien lo hace mejor ni peor

xmin2 <- min(min(dif.rel.mb.suavizada2.trayectoria), min(dif.rel.mb.consultora.trayectoria))
xmax2<- max(max(dif.rel.mb.suavizada2.trayectoria), max(dif.rel.mb.consultora.trayectoria))
num_intervalos2 <- 20
ancho_intervalo2 <- (xmax2 - xmin2) / num_intervalos2


hist(dif.rel.mb.suavizada2.trayectoria,breaks = seq(xmin2, xmax2, ancho_intervalo2),freq=FALSE, col = alpha("blue", 0.5),xlab = "valores", ylab = "Densidad", main = "Error absoluto relativo",cex.main = 2, cex.lab=1.5)
hist(dif.rel.mb.consultora.trayectoria,breaks = seq(xmin2, xmax2, ancho_intervalo2),freq=FALSE, add=TRUE, col = alpha("red", 0.5))
a2<-quantile(dif.rel.mb.suavizada2.trayectoria,c(0.5))
b2<-quantile(dif.rel.mb.consultora.trayectoria,c(0.5))
abline(v=a2,col="blue")
abline(v=b2,col="red")
legend("topright", legend = c("Método propuesto", "Método actual"), fill = c("blue", "red"))


