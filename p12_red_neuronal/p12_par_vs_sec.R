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

modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- 0.995
modelos[modelos=='g'] <- 0.92
modelos[modelos=='b'] <- 0.002

t_par <- integer()
t_sec <- integer()

por_par <- numeric()
por_sec <- numeric()

tvar <- seq(100, 400, 100)
replicas <- 15

for (tt in tvar) {  
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
        cluster <- makeCluster(detectCores())
        clusterExport(cluster, c("modelos", "binario", "neuronas", "decimal"))
        clusterExport(cluster, c("tope", "n", "k", "dim"))
        
        time.par = proc.time()[3]
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
        time.par = proc.time()[3] - time.par
        stopCluster(cluster)
        
        # -------------------------- secuencial -----------------------
        por <- logical()
        time.sec = proc.time()[3]
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
        time.sec = proc.time()[3] - time.sec  
        
        t_par <- c(t_par, time.par)
        t_sec <- c(t_sec, time.sec)
        
        por_par <- c(por_par, (sum(result)*100)/tt)
        por_sec <- c(por_sec, (sum(por)*100)/tt)   
    }
}

tiempos_par <- matrix(t_par, nrow=replicas, ncol=length(tvar), byrow=F)
tiempos_sec <- matrix(t_sec, nrow=replicas, ncol=length(tvar), byrow=F)
#write.csv(tiempos_par, "tiempos_paralelo.csv")
#write.csv(tiempos_sec, "tiempos_secuencial.csv")

colnames(tiempos_par) <- tvar
png("p12_tiempos_par.png")
boxplot(t(tiempos_par), use.cols=FALSE, xlab="pruebas", ylab="tiempos", ylim = c(0, max(t_par, t_sec)), main="Paralelo")
graphics.off()

colnames(tiempos_sec) <- tvar
png("p12_tiempos_sec.png")
boxplot(t(tiempos_sec), use.cols=FALSE, xlab="pruebas", ylab="tiempos", ylim = c(0, max(t_par, t_sec)), main="Secuencial")
graphics.off()

aciertos_par <- matrix(por_par, nrow=replicas, ncol=length(tvar), byrow=F)
aciertos_sec <- matrix(por_sec, nrow=replicas, ncol=length(tvar), byrow=F)
#write.csv(aciertos_par, "aciertos_paralelo.csv")
#write.csv(aciertos_sec, "aciertos_secuencial.csv")

colnames(aciertos_par) <- tvar
png("p12_aciertos_par.png")
boxplot(t(aciertos_par), use.cols=FALSE, xlab="pruebas", ylab="porcentaje de aciertos", ylim = c(0, max(por_par, por_sec)+10), main="Paralelo")
graphics.off()


colnames(aciertos_sec) <- tvar
png("p12_aciertos_sec.png")
boxplot(t(aciertos_sec), use.cols=FALSE, xlab="pruebas", ylab="porcentaje de aciertos", ylim = c(0, max(por_par, por_sec)+10), main="Secuencial")
graphics.off()
