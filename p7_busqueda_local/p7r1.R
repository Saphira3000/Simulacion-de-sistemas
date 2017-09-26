# Practica7: busqueda local
# minimiza -g(x,y)
# reto 1

g <- function(x, y) { 
    return((((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100) )
}

low <- -6
high <- 5
step <- 0.25
tmax <- 100
replicas <- 5

x <- seq(low, high, length.out=500)
y <- x
z <- outer(x, y, g)
dimnames(z) <- list(x, y)
library(reshape2)
d <- melt(z)
names(d) <- c("x", "y", "z")
library(lattice)
png("p7_reto1.png")
levelplot(z ~ x * y, data = d)
color <- c("blue", "magenta4", "green4", "red", "black")

for (rep in 1:replicas) {
    x <- runif(1, low, high)
    y <- runif(1, low, high)
    bestpos <- c(x, y)
    bestval <- -g(x, y)
    a <- 0.5
    for (tiempo in 1:tmax) {
        d <- runif(1, 0, step)
        op = rbind(max(x - d, low), y, min(x + d, high), y, x, max(y - d, low), x, min(y + d, high))
        posibles = numeric() 
        for (i in 1:4) 
            posibles <- c(posibles, -g(op[2*i - 1], op[2*i]))
        mejor <- which.min(posibles)
        nuevo = posibles[mejor] 
        if (nuevo < bestval) { # minimizamos
            bestpos <- c(op[ (2*mejor - 1)], op[2*mejor])
            bestval <- nuevo
        }
        x <- bestpos[1]
        y <- bestpos[2]
        a <- a + 0.02
        trellis.focus("panel", 1, 1, highlight=FALSE)
    	lpoints(bestpos[1], bestpos[2], col=color[rep], cex=a)
    }
}
trellis.unfocus()	
graphics.off()

