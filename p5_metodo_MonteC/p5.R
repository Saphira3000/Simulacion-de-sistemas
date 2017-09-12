#Practica 5: Medoto de Monte-Carlo

inicio <- -6
final <- -inicio
paso <- 0.25

x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }

suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador muestra que cumple con cierta distribucion de probabilidad 
muestra <- generador(50000) # sacamos una muestra

desde <- 3
hasta <- 7
tamMuestra <- c(1000, 5000, 10000, 50000, 100000, 500000, 1000000)
cuantos <- 500
parte <- function() {
    valores <- generador(pedazo)
    return(sum(valores >= desde & valores <= hasta))
}

tiempos <- numeric()
datos <- numeric()
real <- 0.04883411

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for(p in tamMuestra){
	pedazo <- p
	tiempos <- c(tiempos, system.time(montecarlo <- foreach(i=1:cuantos, .combine=c) %dopar% parte())[3])
	stopImplicitCluster()
	aprox <- (pi / 2) * sum(montecarlo) / (cuantos * p)
	error <- (abs(real-aprox)/real)*100
	datos <- c(datos, error)
	print(aprox)
}

png("tiemposp5.png")
plot(tiempos, xlab="Tamaño de la muestra", ylab="Tiempo", col="darkblue", xaxt='n')
axis(1, at=1:7, labels=tamMuestra)
graphics.off()

png("diffp5.png")
plot(datos, xlab="Tamaño de la muestra", ylab="Error relativo", col="darkred", xaxt='n')
axis(1, at=1:7, labels=tamMuestra)
graphics.off()

