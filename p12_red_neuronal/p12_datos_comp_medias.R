# practica 12: redes neuronales
# con version paralelizada
# datos para comparar medias

binario <- function(d, l) {
    b <-  rep(FALSE, l)
    while (l > 0 | d > 0) {
        b[l] <- (d %% 2 == 1)
        l <- l - 1
        d <- bitwShiftR(d, 1)
    }
    return(b)
}

decimal <- function(bits, l) {
    valor <- 0
    for (pos in 1:l) {
        valor <- valor + 2^(l - pos) * bits[pos]
    }
    return(valor)
}

modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- 0.995
modelos[modelos=='g'] <- 0.92
modelos[modelos=='b'] <- 0.002

por_par <- numeric()
por_sec <- numeric()

tt <- 100
replicas <- 50

for (r in 1:replicas) {
    
    r <- 5
    c <- 3
    dim <- r * c
    
    tasa <- 0.15
    tranqui <- 0.99
    
    tope <- 9
    digitos <- 0:tope
    k <- length(digitos)
    
    n <- floor(log(k-1, 2)) + 1
    neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones
    
    for (t in 1:5000) { # entrenamiento
        d <- sample(0:tope, 1) # aleatorio entre 0-tope
        pixeles <- runif(dim) < modelos[d + 1,] # vector TRUE/FALSE
        correcto <- binario(d, n) # etiqueta al numero
        for (i in 1:n) {
            w <- neuronas[i,] 
            deseada <- correcto[i]
            resultado <- sum(w * pixeles) >= 0
            if (deseada != resultado) {
                ajuste <- tasa * (deseada - resultado)
                tasa <- tranqui * tasa
                neuronas[i,] <- w + ajuste * pixeles
            }
        }
    }
    
    # -------------------------- paralelo -------------------------
    suppressMessages(library(parallel))
    cluster <- makeCluster(detectCores()-1)
    clusterExport(cluster, c("modelos", "binario", "neuronas", "decimal"))
    clusterExport(cluster, c("tope", "n", "k", "dim"))
    
    result <- parSapply(cluster, 1:tt, function(algo) {
        d <- sample(0:tope, 1)
        pixeles <- runif(dim) < modelos[d + 1,] 
        salida <- rep(FALSE, n)
        for (i in 1:n) { 
            w <- neuronas[i,]
            resultado <- sum(w * pixeles) >= 0
            salida[i] <- resultado
        }
        r <- min(decimal(salida, n), k)
        return(d == r)
    })
    stopCluster(cluster)
    
    # -------------------------- secuencial -----------------------
    por <- logical()
    for (t in 1:tt) { # prueba
        d <- sample(0:tope, 1)
        pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
        salida <- rep(FALSE, n)
        for (i in 1:n) {
            w <- neuronas[i,]
            resultado <- sum(w * pixeles) >= 0
            salida[i] <- resultado
        }
        r <- min(decimal(salida, n), k) # todos los no-existentes van al final
        por <- c(por, d == r)
    }
    
    por_par <- c(por_par, (sum(result)*100)/tt)
    por_sec <- c(por_sec, (sum(por)*100)/tt)   
}

write.csv(por_par, "aciertos_paralelo.csv")
write.csv(por_sec, "aciertos_secuencial.csv")


