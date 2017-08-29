#practica3: teoria de colas

primo <- function(n) { #revisa si un numero es primo o no
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if (n!=i && (n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#iintervalo de los numeros a revisar 
desde <- 1000
hasta <-  3000

original <- desde:hasta #numeros a revisar orden original
invertido <- hasta:desde #orden inverso
aleatorio <- sample(original) #orden aleatorio
replicas <- 10

suppressMessages(library(doParallel))
nucleos <- detectCores()
resultadosOT <- data.frame()
resultadosIT <- data.frame()
resultadosAT <- data.frame()

for (nu in 1:nucleos) {
  cluster <- makeCluster(nu)
  registerDoParallel(cluster)
  ot <-  numeric()
  it <-  numeric()
  at <-  numeric()	
  for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    at <- c(at, system.time(foreach(n = aleatorio, .combine=c) %dopar% primo(n))[3]) # orden aleatorio
  }
  
  resultadosOT <- rbind(resultadosOT,ot)
  dataOT <- matrix(t(resultadosOT), nrow=replicas, ncol=nucleos)
  
  resultadosIT <- rbind(resultadosIT,it)
  dataIT <- matrix(t(resultadosIT), nrow=replicas, ncol=nucleos)
  
  resultadosAT <- rbind(resultadosAT,at)
  dataAT <- matrix(t(resultadosAT), nrow=replicas, ncol=nucleos)
  
  stopImplicitCluster()
  stopCluster(cluster)
}

png("p3.png")
par(mfrow=c(1,3))
colnames(nucleos)
boxplot(dataOT, xlab="Nucleos", ylab="Tiempo", ylim = c(0, max(dataIT)))
boxplot(dataIT, xlab="Nucleos", ylab="Tiempo", ylim = c(0, max(dataIT)))
boxplot(dataAT, xlab="Nucleos", ylab="Tiempo", ylim = c(0, max(dataIT)))
graphics.off()
