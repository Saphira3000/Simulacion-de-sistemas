# Practica7: busqueda local
# minimiza -g(x,y)

g <- function(x, y) { 
    return((((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100) )
}

low <- -4
high <- 3.5
step <- 0.25
tmax <- 40
replicas <- 3
real <- 0.0666822

replica <- function(t) {
	x <- runif(1, low, high)
	y <- runif(1, low, high)
	bestpos <- c(x, y)
	bestval <- -g(x, y)
	trayectoria <- c(bestval) 
	for (tiempo in 1:t) {
		d <- runif(1, 0, step)
		op = rbind(max(x - d, low), y, min(x + d, high), y, x, max(y - d, low), x, min(y + d, high))
		posibles = numeric() 
    	for (i in 1:4) {
    		posibles <- c(posibles, -g(op[2*i - 1], op[2*i]))
    	}
    	mejor <- which.min(posibles)
    	nuevo = posibles[mejor] 
    	if (nuevo < bestval) { # minimizamos
    		bestpos <- c(op[ (2*mejor - 1)], op[2*mejor])
    		bestval <- nuevo
    	}	
    	x <- bestpos[1]
    	y <- bestpos[2]
    	trayectoria <- c(trayectoria, -bestval)
	}
	return(trayectoria)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
resultados <- foreach(i = 1:replicas, .combine=rbind) %dopar% replica(tmax)	

png("p7tarea.png")
plot(0:tmax, resultados[1, ], col="darkblue", pch=15, xlab="Pasos en el tiempo", ylab="Trayectoria")
points(0:tmax, resultados[2, ], col="darkmagenta", pch=16)
points(0:tmax, resultados[3, ], col="darkgreen", pch=17)
abline(h=real, col="red")
graphics.off()
