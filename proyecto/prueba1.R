# lobos-ovejas version 1.0 

l <- 1 # region
n <- 50 # individuos (ambas especies)
pl <- 0.4 # es lobo

prl <- 0.20 # prob reproduccion lobos
pro <- 0.10 # prob reproduccion ovejas

ec <- 5 # pasos en el tiempo de espera para volver a comer
em <- 3 # lobos que no se alimenten despues de este tiempo moriran

v <- l/30 # tamano de paso
r <- 0.2 # radio en el que deben estar las ovejas para que el lobo las coma
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
ph <- 0.50 # probabilidad minima hambrientos
for (i in 1:dim(poblacion[poblacion$especie,])[1]) { 
	if (poblacion[i, "especie"] == "L") {
		if (runif(1) < ph) {
			poblacion[i, "comer"] = 1;
		} else {
			poblacion[i, "comer"] = 0;
		}
	}
	else if (poblacion[i, "especie"] == "O")
		poblacion[i, "comer"] = 0; 
}


tmax <- 30
digitos <- floor(log(tmax, 10)) + 1
for (tiempo in 1:tmax) {

# comen lobos - muerte ovejas
comen <- which(poblacion$comer != 0, arr.ind = T)
ovejas <- which(poblacion$especie == "O", arr.ind = T) # poblacion[poblacion$especie == "O",]
if (length(comen) != 0) {
for (i in 1:length(comen)) {
	print("lobos que van a comer")
	print(length(comen))
	print("------------")
	lobo <- poblacion[comen[i], ]
	for(j in 1:length(ovejas)) {
		oveja <- poblacion[ovejas[j], ]
		dx <- lobo$x - oveja$x
        dy <- lobo$y - oveja$y
        d <- sqrt(dx^2 + dy^2)
        print(c(d, j))
        if (d < r && !is.na(d)) { #& !is.na(poblacion[ovejas[j], "comer"])) pueden compartir comida
			poblacion[ovejas[j], "comer"] = NA
			#poblacion[comen[i], "comer"] = poblacion[comen[i], "comer"] + 1
		} #else {
			#poblacion[comen[i], "comer"] = poblacion[comen[i], "comer"] - 1
		#}
	}
}
}

poblacion <- poblacion[!is.na(poblacion$comer), ] # actualizo poblacion
# reproduccion ovejas
ovejas <- which(poblacion$especie == "O", arr.ind = T)
for (i in 1:length(ovejas)) {
	if (runif(1) < pro) {
		poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), 
							dx = runif(1, -v, v), dy = runif(1, -v, v), especie = "O", comer = 0))
	}
}

# muerte y reproduccion lobos
lobos <- which(poblacion$especie == "L", arr.ind = T)
if (length(lobos) != 0) {
for (i in 1:length(lobos)) {
    if (runif(1) < 0.3) { #if (poblacion[lobos[i], "comer"] == 0) {
		poblacion[lobos[i], "comer"] = NA
	} else if (runif(1) < pro) {
		poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), 
							dx = runif(1, -v, v), dy = runif(1, -v, v), especie = "L", comer = 0))
	}
}
}
poblacion <- poblacion[!is.na(poblacion$comer), ]

# desplazamiento
for (i in 1:dim(poblacion)[1]) {
	ind <- poblacion[i, ]
	ind$x <- ind$x + ind$dx
    ind$y <- ind$y + ind$dy
    if (ind$x > l) {
        ind$x <- ind$x - l
    }
    if (ind$y > l) {
        ind$y <- ind$y - l
    }
    if (ind$x < 0) {
        ind$x <- ind$x + l
    }
    if (ind$y < 0) {
        ind$y <- ind$y + l
    }
    poblacion[i, ] <- ind
}

iL <- poblacion[poblacion$especie == "L",]
iO <- poblacion[poblacion$especie == "O",]

tl <- paste(tiempo, "", sep="")
while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
}
salida <- paste("p6_t", tl, ".png", sep="")
tiempo <- paste("Paso", tiempo)
png(salida)
plot(l, type="n", xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y", bg = "green")
    if (dim(iL)[1] > 0) {
        points(iL$x, iL$y, pch=15, col="black")
    }
    if (dim(iO)[1] > 0) {
        points(iO$x, iO$y, pch=16, col="blue")
    }
#grid(nx = 1000, ny = 1000, col = "chartreuse", lty = "dotted",
 #    lwd = par("lwd"), equilogs = TRUE)
graphics.off()

}


# rect(-0.9, 1.9, -0.5, 1.2, col = "magenta")
