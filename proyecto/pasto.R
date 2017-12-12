# Pasto Lotka-Volterra

library(parallel)
suppressMessages(library("sna"))

dim <- 10
num <-  dim^2
pasto <- matrix(1*(runif(num) <= 0.30), nrow=dim, ncol=dim)

rotate <- function(x) t(apply(x, 2, rev))
pasto <- rotate(pasto)

system("rm -f pasto-*.png")

paso <- function(pos) {
    fila <- floor((pos - 1) / dim) + 1
    columna <- ((pos - 1) %% dim) + 1
	vecindad <-  pasto[max(fila - 1, 1) : min(fila + 1, dim),
                        max(columna - 1, 1): min(columna + 1, dim)]
	if ((fila == 1 && columna == 1) || (fila == 1 && columna == 10) || (fila == 10 && columna == 1) || (fila == 10 && columna == 10)) {
		return(1 * ((sum(vecindad) - pasto[fila, columna]) > 2)) 
	} else if ( fila == 1 || columna == 1 || fila == 10 || columna == 10) {
		return(1 * ((sum(vecindad) - pasto[fila, columna]) > 3))
	} else {
    	return(1 * ((sum(vecindad) - pasto[fila, columna]) >= 4))
	}
}
 
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

tmax <- 50
digitos <- floor(log(tmax, 10)) + 1

tl <- "0"
while (nchar(tl) < digitos) {
   tl <- paste("0", tl, sep="")
}
png(paste("pasto-", tl, ".png", sep=""))
image(pasto, col=c("green","tan3"), xaxt = 'n', yaxt = 'n')
grid(nx = 10, ny = 10, col = "black", lty = "dotted")
graphics.off()

for (iteracion in 1:tmax) {
    clusterExport(cluster, "pasto")
    siguiente <- parSapply(cluster, 1:num, paso)
    pasto <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)

	tl <- paste(iteracion, "", sep="")
	while (nchar(tl) < digitos) {
   		tl <- paste("0", tl, sep="")
	}

	png(paste("pasto-", tl, ".png", sep=""))
    image(pasto, col=c("green","tan3"), xaxt = 'n', yaxt = 'n')
	grid(nx = 10, ny = 10, col = "black", lty = "dotted")
    graphics.off()
}
stopCluster(cluster)

system("convert -delay 40 -size 300x300 pasto-*.png -loop 0 pasto.gif")

