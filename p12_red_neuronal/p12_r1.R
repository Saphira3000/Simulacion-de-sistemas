# practica 12: redes neuronales
# con version paralelizada

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

por_bien <- numeric() 
replicas <- 10
prob <- seq(0.002, 0.15, 0.03)
for (p in prob) {
    for (r in 1:replicas) {
        
        modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
        modelos[modelos=='n'] <- 0.99
        modelos[modelos=='g'] <- 0.9
        modelos[modelos=='b'] <- p
        
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
        
        for (t in 1:4000) { # entrenamiento
            d <- sample(0:tope, 1)
            pixeles <- runif(dim) < modelos[d + 1,] 
            correcto <- binario(d, n) 
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
        tt <- 250
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
        por_bien <- c(por_bien, (sum(result)*100)/tt)
    }
    
}

aciertos <- matrix(por_bien, nrow=replicas, ncol=length(prob), byrow=F)
write.csv(aciertos, "aciertos_reto1_b.csv")

colnames(aciertos) <- prob
png("p12_reto1_var_b.png")
boxplot(t(aciertos), use.cols=FALSE, xlab="probabilidad", ylab="porcentaje de aciertos", main="Blanco")
graphics.off()


