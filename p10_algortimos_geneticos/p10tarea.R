# practica 10: algoritmos geneticos

library(testit)

# mochila  
knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, costos) {
  return(sum(seleccion * costos))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.costos <- function(pesos, min, max) {
  n <- length(pesos)
  costos <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    costos <- c(costos, rnorm(1, media, desv))
  }
  costos <- normalizar(costos) * (max - min) + min
  return(costos)
}

# aqui inicia algortimos geneticos (subrutinas) 
poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

replicas <- 5
tiempos <- numeric()

pi <- c(50,150,250)
for(init in pi) {
  for (r in 1:replicas) {
    n <- 30
    pesos <- generador.pesos(n, 15, 80)
    costos <- generador.costos(pesos, 10, 500)
    capacidad <- round(sum(pesos) * 0.65)
    optimo <- knapsack(capacidad, pesos, costos)
    
    p <- poblacion.inicial(n, init) # ceros y unos
    tam <- dim(p)[1]
    assert(tam == init)
    pm <- 0.05 # probabilidad de mutacion
    rep <- 20 # reproducciones
    
    tmax <- 30
    mejores <- double()
    
    suppressMessages(library(parallel))
    cluster <- makeCluster(detectCores()-1)
    clusterExport(cluster, "mutacion")
    clusterExport(cluster, "reproduccion")
    clusterExport(cluster, "objetivo")
    clusterExport(cluster, "factible")
    
    t_i <- proc.time()[3]
    for (iter in 1:tmax) {
      p$obj <- NULL 
      p$fact <- NULL
      
      clusterExport(cluster, "n")
      clusterExport(cluster, "p")
      clusterExport(cluster, "pm")
      clusterExport(cluster, "tam")
      result <- parSapply(cluster, 1:tam, function(i) { if (runif(1) < pm) {
        return(unlist(mutacion(p[i,], n)))}})
      result <- matrix(!is.null(result), ncol=n, byrow = F)
      p <- rbind(p, result)
      
      clusterExport(cluster, "tam")
      clusterExport(cluster, "p")
      a <- parSapply(cluster, 1:rep, function(i) { padres <- sample(1:tam, 2, replace=FALSE)
      return(reproduccion(p[padres[1],], p[padres[2],], n))})
      a <- matrix(unlist(a), ncol=n, byrow = F)
      p <- rbind(p, a)
      
      tam <- dim(p)[1]
      obj <- double()
      fact <- integer()
      for (i in 1:tam) {
        obj <- c(obj, objetivo(p[i,], costos))
        fact <- c(fact, factible(p[i,], pesos, capacidad))
      }
      
      p <- cbind(p, obj)
      p <- cbind(p, fact)
      mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
      p <- p[mantener,]
      tam <- dim(p)[1]
      assert(tam == init)
      factibles <- p[p$fact == TRUE,]
      mejor <- max(factibles$obj)
      mejores <- c(mejores, mejor)
    }
    stopCluster(cluster)
    t_f = proc.time()[3]
    t = t_f - t_i
    tiempos <- rbind(tiempos, cbind(t,init))
  }
}
tiempos <- matrix(tiempos, nrow=replicas, byrow = T)

#colnames(tiempos) <- c("t", "init")
png("p10_tiempos.png")
boxplot(tiempos, xlab="Tiempos", ylab="Poblaci\u{F3}n inicial")
graphics.off()