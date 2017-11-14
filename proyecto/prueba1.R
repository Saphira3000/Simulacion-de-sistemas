
l <- 1 # region
n <- 50 # individuos (ambas especies)
pl <- 0.4 # es lobo

prl <- 0.02 # prob reproduccion lobos
pro <- 0.03 # prob reproduccion ovejas

ec <- 5 # pasos en el tiempo de espera para volver a comer
em <- 3 # lobos que no se alimenten despues de este tiempo moriran

v <- l/30 # tamano de paso
r <- 0.1 # radio en el que deben estar las ovejas para que el lobo las coma
poblacion <- data.frame(x = double(), y = double(), dx = double(), dy = double(), especie  = character())

# INICIO
for (i in 1:n) {
	e <- "O"
    if (runif(1) < pl) {
        e <- "L"
    } 
    poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), dx = runif(1, -v, v), dy = runif(1, -v, v), especie = e))
}

iL <- poblacion[poblacion$especie == "L",]
iO <- poblacion[poblacion$especie == "O",]

png("inicio.png")
plot(l, type="n", xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
    if (dim(iL)[1] > 0) {
        points(iL$x, iL$y, pch=15, col="black")
    }
    if (dim(iO)[1] > 0) {
        points(iO$x, iO$y, pch=16, col="blue")
    }
graphics.off()

# hambriento??
poblacion$comer <- NULL
ph <- 0.5 # probabilidad minima hambrientos
for (i in 1:dim(poblacion[poblacion$especie,])[1]) { 
	if (poblacion[i, "especie"] == "L") {
		if (runif(1) > ph) {
			poblacion[i, "comer"] = 1;
		} else {
			poblacion[i, "comer"] = 0;
		}
	}
	else if (poblacion[i, "especie"] == "O")
		poblacion[i, "comer"] = 0; 
}

# comen lobos
comen <- poblacion[poblacion$comer == 1,]
ovejas <- which(poblacion$especie == "O",arr.ind = T) #poblacion[poblacion$especie == "O",]
for (i in 1:dim(comen)[1]) {
	lobo <- comen[i,]
	for(j in 1:length(ovejas)) {
		oveja <- poblacion[ovejas[j], ]
		dx <- lobo$x - oveja$x
        dy <- lobo$y - oveja$y
        d <- sqrt(dx^2 + dy^2)
        if (d < r) { 
			poblacion[ovejas[j],"especie"] <- "NULL" 
		}
	}
}

# reproduccion ovejas


iL <- poblacion[poblacion$especie == "L",]
iO <- poblacion[poblacion$especie == "O",]

png("comida.png")
plot(l, type="n", xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
    if (dim(iL)[1] > 0) {
        points(iL$x, iL$y, pch=15, col="black")
    }
    if (dim(iO)[1] > 0) {
        points(iO$x, iO$y, pch=16, col="blue")
    }
graphics.off()


