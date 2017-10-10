# parctica 9: interacciones entre particulas

library(ggplot2)
library(lattice)

n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=sample(seq(0.1,2,0.2), n, replace=T))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1

p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
carga <- p$g
masa <- p$m

eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi <- p[i,]$m
  fx <- 0
  fy <- 0
  for (j in 1:n) {
    cj <- p[j,]$c
    mj <- p[j,]$m
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- ((-1)^(1 + 1 * (ci * cj < 0))) *  abs(ci - cj) / (mi* (sqrt(dx^2 + dy^2) + eps))
    fx <- fx - dx * factor
    fy <- fy - dy * factor
  }
  return(c(fx, fy))
}

tmax <- 100
system("rm -f p9_t*.png")
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
png(paste("p9_t", tl, ".png", sep=""))
qplot(p$x, p$y, colour=carga, size = masa, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
      main="Estado inicial", xlab="X", ylab="Y")
dev.off()

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
for (iter in 1:tmax) {
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f))
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
  
  tl <- paste(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("p9_t", tl, ".png", sep=""))
  print({
    qplot(p$x, p$y, colour=carga, size = masa, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1), main=paste("Paso", iter), xlab="X", ylab="Y")
  })
  dev.off()
}
stopImplicitCluster()
system("convert -delay 50 -size 300x300 p9_t*.png -loop 0 p9.gif")
