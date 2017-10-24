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

ndom <- function(i) {
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
k <- 2 # cuantas funciones objetivo
nvar <- seq(100,600,100)
t.sec <- numeric()
t.par <- numeric()
replicas <- 30

suppressMessages(library(doParallel))
cluster <- makeCluster(detectCores())
registerDoParallel(cluster)
    
for (n in nvar) {
  for (r in 1:replicas) {
    # --------------------------- paralelo --------------------------
    
    t <- proc.time()[3]
    minim <- (runif(k) > 0.5)
    sign <- (1 + -2 * minim)
    val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
    obj <- list()
    for (j in 1:k) {
      obj <- poli(vc, md, tc)
      val[,j] <- foreach(i = 1:n, .combine=rbind) %dopar% eval(obj, runif(vc), tc)
    }
    
    no.dom <- logical()
	no.dom <- foreach (i = 1:n, .combine=c) %dopar% ndom(i)
	
    frente <- subset(val, no.dom) # solamente las no dominadas
    t <- proc.time()[3] - t
    t.par <- c(t.par, t)
    
    # ----------------------- secuencial -------------------------------
    t <- proc.time()[3]
    minim <- (runif(k) > 0.5)
    sign <- (1 + -2 * minim)
    obj <- list()
    for (i in 1:k) {
      obj[[i]] <- poli(vc, md, tc)
    }
    val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
    for (i in 1:n) { # evaluamos las soluciones
      for (j in 1:k) { # para todos los objetivos
        val[i, j] <- eval(obj[[j]], runif(vc), tc)
      }
    }
    
    no.dom <- logical()
    for (i in 1:n) {
      d <- logical()
      for (j in 1:n) {
        d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
      }
      cuantos <- sum(d)
      no.dom <- c(no.dom, cuantos == 0) # nadie le domina
    }
    frente <- subset(val, no.dom) # solamente las no dominadas
    t <- proc.time()[3] - t
    t.sec <- c(t.sec, t) 
  }
}
stopImplicitCluster()

tiempos <- matrix(t.par, nrow=replicas, ncol=length(nvar), byrow=F)
colnames(tiempos) <- nvar
png("p11_tiempos_par.png")
boxplot(t(tiempos), use.cols=FALSE, xlab="soluciones", ylab="tiempos", main="Paralelo")
graphics.off()

tiempos <- matrix(t.sec, nrow=replicas, ncol=length(nvar), byrow=F)
colnames(tiempos) <- nvar
png("p11_tiempos_sec.png")
boxplot(t(tiempos), use.cols=FALSE, xlab="soluciones", ylab="tiempos", main="Secuencial")
graphics.off()
