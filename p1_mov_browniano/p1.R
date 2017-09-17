

repetir <- 400 #numero de veces que se repite el experimento (caminata)
pasos <- 200 #numero de pasos en la caminata

library(parallel)
 
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "pasos")
datos <-  data.frame()
 
for (dimension in 1:7) {
    clusterExport(cluster, "dimension")
    resultado <- parSapply(cluster, 1:repetir,function(r) {
                           pos <- rep(0, dimension)
                           cont <- 0
                           for (t in 1:pasos) {
                               cambiar <- sample(1:dimension, 1)
                               cambio <- 1
                               if (runif(1) < 0.5) {
                                   cambio <- -1
                               }
                               pos[cambiar] <- pos[cambiar] + cambio
				#esta es la parte que se modifica, se agrega un contador que se incrementa cada vez que la particula regresa al origen
      				if(all(pos==0)) {
        				cont <- cont + 1
                           	}
			}
				return(cont)})

    datos <- rbind(datos, resultado)
}
stopCluster(cluster)

png("p400p.png")
boxplot(data.matrix(datos), use.cols=FALSE, xlab="Dimensi\u{F3}n", ylab="No. veces retorno al origen")
graphics.off()
