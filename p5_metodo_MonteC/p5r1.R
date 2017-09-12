#Practica 5: Medoto de Monte-Carlo
#Encontrar el valor de pi 

r <- 1 
tamMuestra <- c(1000, 5000, 10000, 50000, 100000, 500000, 1000000)

cuantos <- 500
inCircle <- function() {
    x <- runif(pedazo, -r, r)
	y <- runif(pedazo, -r, r)
    return(sum(x**2 + y**2 <= 1))
}

tiempos <- numeric()
datos <- numeric()
real <- 3.141592

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for(p in tamMuestra){
	pedazo <- p
	time <- system.time(montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% inCircle())[3]
	stopImplicitCluster()
	aprox <- sum(montecarlo)/(cuantos * p)
	print(4*aprox)

	datos <- rbind(datos, real-aprox)
	tiempos <- rbind(tiempos,time)
}

png("tiemp5r1.png")
plot(tiempos, xlab="Tamaño de la muestra", ylab="Tiempo", col="darkblue", xaxt='n')
axis(1, at=1:7, labels=tamMuestra)
graphics.off()

png("diffp5r1.png")
plot(datos, xlab="Tamaño de la muestra", ylab="Error relativo", col="darkred", xaxt='n')
axis(1, at=1:7, labels=tamMuestra)
graphics.off()


