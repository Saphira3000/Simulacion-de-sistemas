# Practica 7: reto 2
# reccido simulado
g <- function(x, y) { 
    return((((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100) )
}

low <- -4
high <- 3.5
tmax <- 100
step <- 0.25
temp <- c(10,100,1000)
eps <- 0.99
resultados <- data.frame()

for (t in temp) {
    print(t)
    x <- runif(1, low, high)
    y <- runif(1, low, high)
    
    bestpos <- c(x, y)
    bestval <- -g(x, y)
    trayectoria <- c(-bestval)
    for (tiempo in 1:tmax) {
        d <- runif(1, -step, step)
        if (x + d > high) {
            x <- high
            vecino = c(x, y)
        }
        else if (x + d < low) {
            x <- low
            vecino = c(x , y)
        }
        else
            vecino = c(x + d, y)
        delta <- -g(vecino[1], vecino[2]) - bestval 
        if (delta > 0) { 
            bestpos <- vecino
            bestval <- -g(vecino[1], vecino[2])
        }
        else if(delta < 0) {
            if(runif(1) < exp(delta/t)) {
                t <- t*eps
                bestpos <- vecino
                bestval <- -g(vecino[1], vecino[2])
            }
        }
        x <- bestpos[1]
        y <- bestpos[2]
        trayectoria <- c(trayectoria, -bestval)
    }
    resultados <- rbind(resultados, trayectoria)
}

maxi <- max(resultados)
mini <- min(resultados)
real <- 0.0666822
png("p7_reto2.png")
plot(0:tmax, resultados[1, ], col="darkblue", type = "l", xlab="Pasos en el tiempo", ylab="Trayectoria", ylim=c(mini, max(real, maxi)))
lines(0:tmax, resultados[2, ], col="darkmagenta")
lines(0:tmax, resultados[3, ], col="darkgreen")
abline(h=real, col="red")
graphics.off()
