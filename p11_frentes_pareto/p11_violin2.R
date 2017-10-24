# practica 11: frentes de pareto

pick.one <- function(x) {
    if (length(x) == 1) {
        return(x)
    } else {
        return(sample(x, 1))
    }
}

poli <- function(maxdeg, varcount, termcount) {
    f <- data.frame(variable=integer(), coef=integer(), degree=integer())
    for (t in 1:termcount) {
        var <- pick.one(1:varcount)
        deg <- pick.one(1:maxdeg)
        f <-  rbind(f, c(var, runif(1), deg))
    }
    names(f) <- c("variable", "coef", "degree")
    return(f)
}

eval <- function(pol, vars, terms) {
    value <- 0.0
    for (t in 1:terms) {
        term <- pol[t,]
        value <-  value + term$coef * vars[term$variable]^term$degree
    }
    return(value)
}

domin.by <- function(target, challenger, total) {
    if (sum(challenger < target) > 0) {
        return(FALSE) # hay empeora
    } # si no hay empeora, vemos si hay mejora
    return(sum(challenger > target) > 0)
}

dom <- function(i) {
    d <- logical()
    for (j in 1:n) {
        d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
    }
    cuantos <- sum(d)
    return(cuantos == 0)
}

# ---------------------------------------------------------------
# ---------------------------------------------------------------

vc <- 4
md <- 3
tc <- 5
n <- 100 # cuantas soluciones aleatorias
replicas <- 30
kf <- seq(2,5,1) # funciones objetivo

suppressMessages(library(doParallel))
cluster <- makeCluster(detectCores()-1)
registerDoParallel(cluster)

resultados <- data.frame()

for (k in kf) {
for (r in 1:replicas) {
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)

val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
obj <- list()
for (j in 1:k) {
    obj <- poli(vc, md, tc)
      val[,j] <- foreach(i = 1:n, .combine=rbind) %dopar% eval(obj, runif(vc), tc)
   }

no.dom <- logical()
no.dom <- foreach (i = 1:n, .combine=c) %dopar% dom(i)
frente <- subset(val, no.dom)
no.dom <- sum(no.dom)
no.dom <- (no.dom*100)/n

resultados <- rbind(resultados, c(k=k, por=no.dom))
}
}
stopImplicitCluster()

names(resultados) <- c("k", "por")

cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
png("p11_frente.png")
plot(val[,1], val[,2], xlab=xl,
     ylab=yl,
     main="Ejemplo bidimensional")
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
graphics.off()


library(ggplot2)
#data <- data.frame(pos=rep(0, n), dom=dominadores)
png("p11_violin.png")
gr <- ggplot(resultados, aes(x=k, y=por, group=k)) + geom_violin(fill="orange", color="red")
gr + geom_boxplot(width=0.2, fill="blue", color="white", lwd=2) +
    xlab("Funciones objetivo") +
    ylab("Porcentaje de soluciones") +
    ggtitle("Soluciones de Pareto")
graphics.off()
