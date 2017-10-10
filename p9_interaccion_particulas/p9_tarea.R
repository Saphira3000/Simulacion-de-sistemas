# parctica 9: interacciones entre particulas

n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=sample(seq(0.1,1,0.1), n, replace=T))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1

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

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
tmax <- 100
result <- data.frame()

for (iter in 1:tmax) {
  iniciox <- p$x
  inicioy <- p$y
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f))
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
  vx <- p$x - iniciox
  vy <- p$y - inicioy
  vel <- sqrt(vx*vx + vy*vy)
  result <- rbind(result, cbind(vel, p$m))
}
stopImplicitCluster()

colnames(result) <- c("v", "m")
png("p9_vel_masa.png")
boxplot(result$v ~ result$m, xlab = "Masa", ylab = "Velocidad")
graphics.off()
