
data <- read.csv("zika.csv", header=TRUE, sep=",")
#graficamos los datos
png("p5r2.png", width=600, height=300, units="px")
plot(data$sem, data$casos, xlab="Semana", ylab="Casos nuevos", xlim=c(1,34))
lines(data$sem, data$casos, type="l")

#histograma de los datos
png("histp5.png")
hist(data$casos, xlab="Semana", ylab="Casos nuevos", main="Histograma casos Zika")
graphics.off()

media <- mean(data$casos)
dist <- rexp(34, rate = 1/media) #distribucion exponencial con media de los datos

#comparamos datos y datos distribucion
png("distp5.png", width=600, height=300, units="px")
plot(data$sem, data$casos, xlab="Semana", ylab="Casos nuevos")
lines(data$sem, data$casos, type="l")
par(new=TRUE)
plot(dist, axes = FALSE, xlab="", ylab="", xlim=c(1,34), col="darkblue")
lines(dist, type="l", col="darkblue")

#aplicamos Monte Carlo
#pedazo <- 35
#cuantos <- 500
#predice <- function() {
 #   valores <- rexp(pedazo, rate = 1/media)
  #  return(sum(valores[pedazo]))
#}

#suppressMessages(library(doParallel))
#registerDoParallel(makeCluster(detectCores() - 1))
#montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% predice()
#stopImplicitCluster()
#prediccion <- sum(montecarlo) / (cuantos)
#print(prediccion)
