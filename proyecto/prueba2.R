# lobos-ovejas version 2.0 

l <- 1 # region
n <- 50 # individuos (ambas especies)

epsilon <- 0 # prob reproduccion lobos
alpha <- 0.08 # prob reproduccion ovejas

gama <- 0.3 # probabilidad muerte lobos
bet <- 0.2 # muerte de ovejas

v <- l/30 # tamano de paso
poblacion <- data.frame(x = double(), y = double(), dx = double(), dy = double(), especie  = character())

plobos <- numeric() # poblacion lobos a lo largo del tiempo
povejas <- numeric() # poblacion ovejas a lo largo del tiempo

plobosi <- numeric() 
povejasi <- numeric()

# INICIO
for (i in 1:n) {
  e <- "O"
  if ( i == n ) {
    e <- "L"
  } 
  poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), dx = runif(1, -v, v), dy = runif(1, -v, v), especie = e))
}

iL <- poblacion[poblacion$especie == "L",]
iO <- poblacion[poblacion$especie == "O",]

tmax <- 30
digitos <- floor(log(tmax, 10)) + 1

system("rm -f pr2_lotka_t*.png")
system("rm -f pr2_poblacion_t*.png")

tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
png(paste("pr2_lotka_t", tl, ".png", sep=""))
plot(l, type="n", xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y", main="Poblaci\u{F3}n inicial")
rect(0, 0, l, l, col = "green")
if (dim(iL)[1] > 0) {
  points(iL$x, iL$y, pch=15, col="black")
}
if (dim(iO)[1] > 0) {
  points(iO$x, iO$y, pch=16, col="white")
}
graphics.off()

for (tiempo in 1:tmax) {
  plobosi = dim(poblacion[poblacion$especie=="L", ])[1]
  povejasi = dim(poblacion[poblacion$especie=="O", ])[1]
  
  mo = round(bet*plobosi)
  ro = round(alpha*povejasi)
  
  #ml = round(gama*povejasi)
  rl = round(((povejasi - plobosi)*(1/4) / plobosi) + epsilon)
  
  povejasf = povejasi + ro - mo
  plobosf = plobosi + rl
  
  for (i in 1:mo) {
    ovejas = which(poblacion$especie == "O", arr.ind = T)
    quita = sample(ovejas, 1, replace = F)
    poblacion[quita, "x"] = NA
  }
  poblacion <- poblacion[!is.na(poblacion$x), ]
  
  for (i in 1:ro) {
    poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), dx = runif(1, -v, v), dy = runif(1, -v, v), especie = "O")) 
  }
  
  if (rl < 0) {
    for (i in 1:(-rl)) {
      lobos = which(poblacion$especie == "L", arr.ind = T)
      quita = sample(lobos, 1, replace = F)
      poblacion[quita, "x"] = NA
    }
  } else {
    for (i in 1:rl) {
      poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), 
                                               dx = runif(1, -v, v), dy = runif(1, -v, v), especie = "L")) 
    }
  }
  poblacion <- poblacion[!is.na(poblacion$x), ]
  
  # desplazamiento
  if (dim(poblacion)[1] != 0) {
    for (i in 1:dim(poblacion)[1]) {
      ind <- poblacion[i, ]
      ind$x <- ind$x + ind$dx
      ind$y <- ind$y + ind$dy
      if (ind$x > l) {
        ind$x <- ind$x - (l/10)
      }
      if (ind$y > l) {
        ind$y <- ind$y - (l/10)
      }
      if (ind$x < 0) {
        ind$x <- ind$x + (l/10)
      }
      if (ind$y < 0) {
        ind$y <- ind$y + (l/10)
      }
      poblacion[i, ] <- ind
    }
    
    iL <- poblacion[poblacion$especie == "L",]
    iO <- poblacion[poblacion$especie == "O",]
    
    # graficamos...
    tl <- paste(tiempo, "", sep="")
    while (nchar(tl) < digitos) {
      tl <- paste("0", tl, sep="")
    }
    salida <- paste("pr2_lotka_t", tl, ".png", sep="")
    paso <- paste("Paso", tiempo)
    png(salida)
    plot(l, type="n", xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y", main=paso)
    rect(0, 0, l, l, col = "green")
    if (dim(iL)[1] > 0) {
      points(iL$x, iL$y, pch=15, col="black")
    }
    if (dim(iO)[1] > 0) {
      points(iO$x, iO$y, pch=16, col="white")
    }
    graphics.off()
    
    lobos = dim(iL)[1]
    ovejas = dim(iO)[1]

    plobos <- c(plobos, lobos)
    povejas <- c(povejas, ovejas)
    png(paste("pr2_poblacion_t", tl, ".png", sep=""), width=600, height=300)
    plot(1:tiempo, plobos, xlim=c(0, tmax), ylim=c(0, 1.5*n), xlab="Tiempo", ylab="Poblaci\u{F3}n", col = "red")
    points(1:tiempo, povejas, xlim=c(0, tmax), ylim=c(0, 1.5*n), pch=16, col = "blue")
    graphics.off()
  }
}

system("convert -delay 50 -size 300x300 pr2_lotka_t*.png -loop 0 pr2_lotka.gif")
system("convert -delay 50 -size 300x300 pr2_poblacion_t*.png -loop 0 pr2_poblacion.gif")

# rect(-0.9, 1.9, -0.5, 1.2, col = "magenta")
#grid(nx = 1000, ny = 1000, col = "chartreuse", lty = "dotted",
#    lwd = par("lwd"), equilogs = TRUE)
