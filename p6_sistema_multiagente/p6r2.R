# Practica 6: Sistema mutiagente
# version paralelizada

suppressMessages(library(doParallel))
cluster <- makeCluster(detectCores() - 1)

l <- 1.5 # donde se mueven
n <- 50 # agentes
pr <- 0.02 # probabilidad de recuperacion
v <- l / 30 # margen de "velocidad"

# estado inicial de todos los agentes
inicio <- function(p) {
    e <- "S"
    if (runif(1) < pi) {
        e <- "I"
    }
    edo_inicial <- data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                              dx = runif(1, -v, v), dy = runif(1, -v, v),
                              estado = e)
    return(edo_inicial)
}

mueveActualiza <- function(i) {
    a <- agentes[i, ]
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

prob <- seq(0.05, 0.8, 0.07) 
for (replica in 1:5) {
    for(pi in prob) {
        agentes <- foreach(i = 1:n, .combine=rbind) %dopar% inicio(pi)
        levels(agentes$estado) <- c("S", "I", "R")
        
        epidemia <- integer()
        r <- 0.1 # radio de infeccion
        tmax <- 80 # tiempo de simulacion
        digitos <- floor(log(tmax, 10)) + 1
        
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
            
            agentes <- foreach(i = 1:n, .combine=rbind) %dopar% mueveActualiza(i)
            
            aS <- agentes[agentes$estado == "S",]
            aI <- agentes[agentes$estado == "I",]
            aR <- agentes[agentes$estado == "R",]
        }
    }
	m <- max(epidemia)
    resultados <- rbind(resultados, c(pi, 100 * m / n))
}
stopImplicitCluster()

result <- resultados[,-1]
data <- t(result)

png("p6r2.png")
colnames(data) <- prob 
boxplot(data, xlab="Probabilidad de infecci\u{F3}n", ylab="Porcentaje m\u{E1}ximo de infectados")
graphics.off()
