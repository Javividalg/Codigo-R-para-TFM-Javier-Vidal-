#A. Medidas descritivas del error absoluto
#B. Histograma de los errores absolutos sin tener en cuenta quien lo hace mejor ni peor

#A. Medidas descritivas del error absoluto

mean(dif.mb.suavizada2.trayectorias)
mean(dif.mb.consultora.trayectorias)
max(dif.mb.suavizada2.trayectorias)
max(dif.mb.consultora.trayectorias)
min(dif.mb.suavizada2.trayectorias)
min(dif.mb.consultora.trayectorias)
median(dif.mb.suavizada2.trayectorias)
median(dif.mb.consultora.trayectorias)




#B. Histograma de los errores absolutos sin tener en cuenta quien lo hace mejor


xmin2 <- min(min(dif.mb.suavizada2.trayectorias), min(dif.mb.consultora.trayectorias))
xmax2<- max(max(dif.mb.suavizada2.trayectorias), max(dif.mb.consultora.trayectorias))
num_intervalos2 <- 20
ancho_intervalo2 <- (xmax2 - xmin2) / num_intervalos2


hist(dif.mb.suavizada2.trayectorias,breaks = seq(xmin2, xmax2, ancho_intervalo2),freq=FALSE, col = alpha("blue", 0.5),xlab = "valores", ylab = "Densidad", main = "Error absoluto",cex.main = 2,cex.lab=1.5)
hist(dif.mb.consultora.trayectorias,breaks = seq(xmin2, xmax2, ancho_intervalo2),freq=FALSE, add=TRUE, col = alpha("red", 0.5))
a2<-quantile(dif.mb.suavizada2.trayectorias,c(0.5))
b2<-quantile(dif.mb.consultora.trayectorias,c(0.5))
abline(v=a2,col="blue")
abline(v=b2,col="red")
legend("topright", legend = c("Método propuesto", "Método actual"), fill = c("blue", "red"))


