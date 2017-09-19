# Practica 6: Sistema mutiagente
# version paralelizada

suppressMessages(library(doParallel))
cluster <- makeCluster(detectCores()-1)

l <- 1.5 # donde se mueven
n <- 50 # agentes
pi <- 0.05 # probabilidad inicial de infeccion
pr <- 0.02 # probabilidad de recuperacion
v <- l / 30 # margen de "velocidad"
pvacuna <- seq(0.05,0.95,0.1) # probabilidad de vacuna

# estado inicial de todos los agentes
inicio <- function() {
    e <- "S"
    if (runif(1) < pv) {
        e <- "R"
    }
    edo_inicial <- data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                              dx = runif(1, -v, v), dy = runif(1, -v, v),
                              estado = e)
    return(edo_inicial)
}

mueveActualiza <- function(i) {
    a <- agentes[i, ]
    print(a)
    if (contagios[i]) {
        a$estado <- "I"
    } else if (a$estado == "I") { # ya estaba infectado
        if (runif(1) < pr) {
            a$estado <- "R" # recupera
        }
    }
    a$x <- a$x + a$dx
    a$y <- a$y + a$dy
    if (a$x > l) {
        a$x <- a$x - l
    }
    if (a$y > l) {
        a$y <- a$y - l
    }
    if (a$x < 0) {
        a$x <- a$x + l
    }
    if (a$y < 0) {
    }
    return(a)
}

agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())
resultados <- data.frame()

registerDoParallel(cluster)

    for(pv in pvacuna) {
        agentes <- foreach(i = 1:n, .combine=rbind) %dopar% inicio()
        levels(agentes$estado) <- c("S", "I", "R")
        
        epidemia <- integer()
        r <- 0.1 # radio de infeccion
        tmax <- 80 # tiempo de simulacion
        digitos <- floor(log(tmax, 10)) + 1
        
        # movimiento en el tiempo
        
        for (tiempo in 1:tmax) { 
            infectados <- dim(agentes[agentes$estado == "I",])[1]
            epidemia <- c(epidemia, infectados)
            if (infectados == 0) { # no hay infectados
                break
            }
            
            contagios <- rep(FALSE, n)
            for (i in 1:n) {
                a1 <- agentes[i, ]
                if (a1$estado == "I") { # desde los infectados
                    for (j in 1:n) {
                        if (!contagios[j]) { # aun sin contagio
                            a2 <- agentes[j, ]
                            if (a2$estado == "S") { # hacia los susceptibles
                                dx <- a1$x - a2$x
                                dy <- a1$y - a2$y
                                d <- sqrt(dx^2 + dy^2)
                                if (d < r) { # umbral
                                    p <- (r - d) / r
                                    if (runif(1) < p) {
                                        contagios[j] <- TRUE
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            registerDoParallel(cluster)
            agentes <- foreach(i = 1:n, .combine=rbind) %dopar% mueveActualiza(i)
            stopImplicitCluster()
            
            aS <- agentes[agentes$estado == "S",]
            aI <- agentes[agentes$estado == "I",]
            aR <- agentes[agentes$estado == "R",]
        }
        m <- mean(epidemia)
        resultados <- rbind(resultados, c(pv, 100 * m / n))
    }
stopImplicitCluster()

resultados <- resultados[,-1]

png("p6r1.png")
plot(resultados, xlab="Probabilidad vacuna", ylab="Porcentaje promedio de infectados", xaxt='n')
axis(1, at=1:10, labels=pvacuna)
graphics.off()
