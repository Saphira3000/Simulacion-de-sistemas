# Practica 8: modelo de urnas
duracion <- 5
paralelo <- TRUE
tiempo <- integer()
tiempoS <- integer()
potencias <- 6
replicas <- 5

for (pot in 5:potencias) {
  tiemp <- proc.time()[3]
  k <- 10^pot
  n <- 30*k
# creo cumulos que sigan distribucion normal
for (replica in 1:replicas) {  
originales <- rnorm(k)
cumulos <- originales - min(originales) + 1
cumulos <- round(n * cumulos / sum(cumulos))
diferencia <- n - sum(cumulos)
assert(min(cumulos) > 0)

# para que no falten o sobren particulas
if (diferencia > 0) { 
  for (i in 1:diferencia) {
    p <- sample(1:k, 1)
    cumulos[p] <- cumulos[p] + 1
  }
} else if (diferencia < 0) {
  for (i in 1:-diferencia) {
    p <- sample(1:k, 1)
    if (cumulos[p] > 1) {
      cumulos[p] <- cumulos[p] - 1
    }
  }
}
assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
assert(sum(cumulos) == n)

c <- median(cumulos) # tamanio critico de cumulos
d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva

romperse <- function(i) {
  urna <- freq[i,]
  if (urna$tam > 1) { 
    romper <- round((1 / (1 + exp((c - urna$tam) / d))) * urna$num) 
    resultado <- rep(urna$tam, urna$num - romper) 
    if (romper > 0) {
      for (cumulo in 1:romper) { # agregar las rotas
        t <- 1
        if (urna$tam > 2) { 
          t <- sample(1:(urna$tam-1), 1)
        }
        resultado <- c(resultado, t, urna$tam - t)
      }
    }
    assert(sum(resultado) == urna$tam * urna$num)
    return(resultado)
  } else {
    return(rep(1, urna$num))
  }
}

unirse <- function(i) {
  urna <- freq[i,]
  unir <- round((-urna$tam/c) * urna$num) 
  if (unir > 0) {
    division <- c(rep(-urna$tam, unir), rep(urna$tam, urna$num - unir))
    assert(sum(abs(division)) == urna$tam * urna$num)
    return(division)
  } else {
    return(rep(urna$tam, urna$num))
  }
}

if (paralelo) {
  suppressMessages(library(parallel))
  cluster <- makeCluster(detectCores())
  clusterExport(cluster, "c")
  clusterExport(cluster, "d")
  clusterExport(cluster, "assert")
}

for (paso in 1:duracion) {
  assert(sum(cumulos) == n)
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num") 
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  cumulos <- integer()
  listas <- numeric()
  
  if (paralelo) {
    clusterExport(cluster, "freq")
    listas <- parSapply(cluster, 1:dim(freq)[1], romperse)
  } else { 
    listas <- sapply(1:dim(freq)[1], romperse)
    }
  cumulos = Reduce(c, listas)
  
  assert(sum(cumulos) == n)
  assert(length(cumulos[cumulos == 0]) == 0)
  freq <- as.data.frame(table(cumulos)) # actualizar urnas
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  cumulos <- integer()
  listas <- numeric()
  
  if (paralelo) {
    clusterExport(cluster, "freq")
    listas <- parSapply(cluster, 1:dim(freq)[1], unirse)
  } else {
    m = dim(freq)[1]
    listas <- sapply(1:dim(freq)[1], unirse)
  }
  cumulos = Reduce(c, listas)
  
  assert(sum(cumulos) == n)
  assert(length(cumulos[cumulos == 0]) == 0)
  juntarse <- -cumulos[cumulos < 0]
  cumulos <- cumulos[cumulos > 0]
  
  nt <- length(juntarse)
  if (nt > 0) {
    if (nt > 1) {
      juntarse <- sample(juntarse)
      for (i in 1:floor(nt / 2) ) {
        cumulos <- c(cumulos, juntarse[(2*i)-1] + juntarse[2*i])
      }
    }
    if (nt %% 2 == 1) {
      cumulos <- c(cumulos, juntarse[nt])
    }
  }
}
if (paralelo) {
	stopCluster(cluster)
}
tiempo <- c(tiempo, proc.time()[3] - tiemp)
}
}  

paralelo <- FALSE
for (pot in 1:potencias) {
  tiempS <- proc.time()[3]
  k <- 10^pot
  n <- 30*k
  # creo cumulos que sigan distribucion normal
  for (replica in 1:replicas) {  
    originales <- rnorm(k)
    cumulos <- originales - min(originales) + 1
    cumulos <- round(n * cumulos / sum(cumulos))
    diferencia <- n - sum(cumulos)
    assert(min(cumulos) > 0)
    
    # para que no falten o sobren particulas
    if (diferencia > 0) { 
      for (i in 1:diferencia) {
        p <- sample(1:k, 1)
        cumulos[p] <- cumulos[p] + 1
      }
    } else if (diferencia < 0) {
      for (i in 1:-diferencia) {
        p <- sample(1:k, 1)
        if (cumulos[p] > 1) {
          cumulos[p] <- cumulos[p] - 1
        }
      }
    }
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    assert(sum(cumulos) == n)
    
    c <- median(cumulos) # tamanio critico de cumulos
    d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
    
    romperse <- function(i) {
      urna <- freq[i,]
      if (urna$tam > 1) { 
        romper <- round((1 / (1 + exp((c - urna$tam) / d))) * urna$num) 
        resultado <- rep(urna$tam, urna$num - romper) 
        if (romper > 0) {
          for (cumulo in 1:romper) { # agregar las rotas
            t <- 1
            if (urna$tam > 2) { 
              t <- sample(1:(urna$tam-1), 1)
            }
            resultado <- c(resultado, t, urna$tam - t)
          }
        }
        assert(sum(resultado) == urna$tam * urna$num)
        return(resultado)
      } else {
        return(rep(1, urna$num))
      }
    }
    
    unirse <- function(i) {
      urna <- freq[i,]
      unir <- round((-urna$tam/c) * urna$num) 
      if (unir > 0) {
        division <- c(rep(-urna$tam, unir), rep(urna$tam, urna$num - unir))
        assert(sum(abs(division)) == urna$tam * urna$num)
        return(division)
      } else {
        return(rep(urna$tam, urna$num))
      }
    }
    
    if (paralelo) {
      suppressMessages(library(parallel))
      cluster <- makeCluster(detectCores())
      clusterExport(cluster, "c")
      clusterExport(cluster, "d")
      clusterExport(cluster, "assert")
    }
    
    for (paso in 1:duracion) {
      assert(sum(cumulos) == n)
      freq <- as.data.frame(table(cumulos))
      names(freq) <- c("tam", "num") 
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      cumulos <- integer()
      listas <- numeric()
      
      if (paralelo) {
        clusterExport(cluster, "freq")
        listas <- parSapply(cluster, 1:dim(freq)[1], romperse)
      } else { 
        listas <- sapply(1:dim(freq)[1], romperse)
      }
      cumulos = Reduce(c, listas)
      
      assert(sum(cumulos) == n)
      assert(length(cumulos[cumulos == 0]) == 0)
      freq <- as.data.frame(table(cumulos)) # actualizar urnas
      names(freq) <- c("tam", "num")
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      cumulos <- integer()
      listas <- numeric()
      
      if (paralelo) {
        clusterExport(cluster, "freq")
        listas <- parSapply(cluster, 1:dim(freq)[1], unirse)
      } else {
        m = dim(freq)[1]
        listas <- sapply(1:dim(freq)[1], unirse)
      }
      cumulos = Reduce(c, listas)
      
      assert(sum(cumulos) == n)
      assert(length(cumulos[cumulos == 0]) == 0)
      juntarse <- -cumulos[cumulos < 0]
      cumulos <- cumulos[cumulos > 0]
      
      nt <- length(juntarse)
      if (nt > 0) {
        if (nt > 1) {
          juntarse <- sample(juntarse)
          for (i in 1:floor(nt / 2) ) {
            cumulos <- c(cumulos, juntarse[(2*i)-1] + juntarse[2*i])
          }
        }
        if (nt %% 2 == 1) {
          cumulos <- c(cumulos, juntarse[nt])
        }
      }
    }
    if (paralelo) {
      stopCluster(cluster)
    }
    tiempoS <- c(tiempoS, proc.time()[3] - tiempS)
  }
} 

datos1 <- matrix(tiempo, nrow=replicas, ncol=potencias)
datos2 <- matrix(tiempoS, nrow=replicas, ncol=potencias)

png("tiempos.png")
par(mfrow = c(2, 1))
boxplot(datos1, xlab="i", ylab="Tiempo de ejecuci\u{F3}n", ylim = c(0, max(tiempoS, tiempo)), main="Paralelo")
boxplot(datos2, xlab="i", ylab="Tiempo de ejecuci\u{F3}n", ylim = c(0, max(tiempoS, tiempo)), main="Secuencial")
graphics.off()