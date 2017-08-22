#practica 2: Automata celular
#tarea 2

library(parallel)
library(Rlab)

dim <- 10
num <-  dim^2

datos <- data.frame()

paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

prob = seq(0.1,0.9,0.1)
for(p in prob) { # aqui variamos la probabilidad
  for (repetir in 1:50) {
    i <- 0
    actual <- matrix(rbern(100,p), nrow=dim, ncol=dim) #matriz celulas vivas con cierta probabilidad
    for (iteracion in 1:15) {
      i <- i + 1 # contador para las generaciones
      clusterExport(cluster, "actual")
      siguiente <- parSapply(cluster, 1:num, paso)
      if (sum(siguiente) == 0) { # todos murieron
        i <- iteracion
        break;  
      }        
      actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    }
    datos <- rbind(datos, i)
  }
}

data <- matrix(t(datos), nrow=50, ncol=9)

stopCluster(cluster)

png("prueba.png")
colnames(data) = prob
boxplot(data, xlab="Probabilidad", ylab="Iteraciones",)
graphics.off()

