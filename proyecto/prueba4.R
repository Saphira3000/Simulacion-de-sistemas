# lobos-ovejas version 1.0 mejorada

l <- 1 # region
n <- sample(c(50,60,70,80), 1) # individuos (ambas especies)
pl <- 0.5 # es lobo

prl <- 0.10 # prob reproduccion lobos
pro <- 0.10 # prob reproduccion ovejas

ph <- 0.65 # probabilidad minima hambrientos
pml <- 0.5 # probabilidad muerte lobos

v <- l/30 # tamano de paso
r <- 0.15 # radio en el que deben estar las ovejas para que el lobo las coma
poblacion <- data.frame(x = double(), y = double(), dx = double(), dy = double(), especie  = character())

plobos <- numeric() # poblacion lobos a lo largo del tiempo
povejas <- numeric() # poblacion ovejas a lo largo del tiempo

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

tmax <- 30
digitos <- floor(log(tmax, 10)) + 1

# eliminamos png anteriores
system("rm -f lotka_t*.png")
system("rm -f poblacion_t*.png")

# graficamos poblacion inicial
tl <- "0"
while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
}
png(paste("lotka_t", tl, ".png", sep=""))
plot(l, type="n", xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y", main="Poblaci\u{F3}n inicial")
rect(0, 0, l, l, col = "green")
if (dim(iL)[1] > 0) {
    points(iL$x, iL$y, pch=15, col="black")
}
if (dim(iO)[1] > 0) {
    points(iO$x, iO$y, pch=16, col="white")
}
graphics.off()

# cambio en el tiempo
for (tiempo in 1:tmax) {
    # hambriento??
    poblacion$comer <- NULL
    for (i in 1:dim(poblacion)[1]) {
        poblacion[i, "comer"] = 0;
        if (length(which(poblacion$especie == "L")) != 0) { 
        	if (poblacion[i, "especie"] == "L" ) {
            	if (runif(1) < ph) {
                	poblacion[i, "comer"] = 1;
            	} 
            }
        }
    }
    
    # comen lobos - muerte ovejas
    comen <- which(poblacion$comer != 0, arr.ind = T)
    ovejas <- which(poblacion$especie == "O", arr.ind = T) 
    if (length(comen) != 0 && length(ovejas) != 0) {
        for (i in 1:length(comen)) {
            lobo <- poblacion[comen[i], ]
            for(j in 1:length(ovejas)) {
                oveja <- poblacion[ovejas[j], ]
                dx <- lobo$x - oveja$x
                dy <- lobo$y - oveja$y
                d <- sqrt(dx^2 + dy^2)
                if (d < r && !is.na(d)) {
                    poblacion[ovejas[j], "comer"] = NA
                }
            }
        }
    }
    
    poblacion <- poblacion[!is.na(poblacion$comer), ]
    # reproduccion ovejas
    ovejas <- which(poblacion$especie == "O", arr.ind = T)
    if (length(ovejas) != 0) {
        for (i in 1:length(ovejas)) {
            if (runif(1) < pro) {
                poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), 
                                                         dx = runif(1, -v, v), dy = runif(1, -v, v), especie = "O", comer = 0))
            }
        }
    }
    
    # muerte y reproduccion lobos
    lobos <- which(poblacion$especie == "L", arr.ind = T)
    if (length(lobos) != 0) {
        for (i in 1:length(lobos)) {
            if (runif(1) < pml || length(ovejas) == 0) {
                poblacion[lobos[i], "comer"] = NA
            } else if (runif(1) < prl) {
            	cuantos <- sample(1:3, 1)
            	for (crias in 1:cuantos) {
                	poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), 
                                                         dx = runif(1, -v, v), dy = runif(1, -v, v), especie = "L", comer = round(runif(1))))
                }
            }
        }
    }
    poblacion <- poblacion[!is.na(poblacion$comer), ]
    # desplazamiento
    print(dim(poblacion)[1])
    
    if (dim(poblacion)[1] != 0) {
        for (i in 1:dim(poblacion)[1]) {
        	print(c("i", i))
            ind <- poblacion[i, ]
            ind$x <- ind$x + ind$dx
            ind$y <- ind$y + ind$dy
            if (ind$x > l) {
                ind$x <- ind$x - 2*v
            }
            if (ind$y > l) {
                ind$y <- ind$y - 2*v
            }
            if (ind$x < 0) {
                ind$x <- ind$x + 2*v
            }
            if (ind$y < 0) {
                ind$y <- ind$y + 2*v
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
        salida <- paste("lotka_t", tl, ".png", sep="")
        paso <- paste("Paso", tiempo)
        png(salida)
        plot(l, type="n", xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y", main=paso)
        #box("outer", col="green")
        rect(0, 0, l, l, col = "green")
        if (dim(iL)[1] > 0) {
            points(iL$x, iL$y, pch=15, col="black")
        }
        if (dim(iO)[1] > 0) {
            points(iO$x, iO$y, pch=16, col="white")
        }
        graphics.off()
        
        # poblaciones
        lobos = dim(iL)[1]
        ovejas = dim(iO)[1]
        plobos <- c(plobos, lobos)
        povejas <- c(povejas, ovejas)
        png(paste("poblacion_t", tl, ".png", sep=""), width=600, height=300)
		plot(1:tiempo, plobos, xlim=c(0, tmax), ylim = c(0, 1.5*n), xlab="Tiempo", ylab="Poblaci\u{F3}n", col = "red")
	 	points(1:tiempo, povejas, xlim=c(0, tmax), ylim = c(0, 1.5*n), pch=16, col = "blue")
  		graphics.off()
    }
}

#pmax <- max(plobos, povejas)

#for (tiempo in 1:tmax) {
#    png(paste("poblacion_t", tl, ".png", sep=""), width=600, height=300)
#	plot(1:tiempo, plobos, xlim=c(0, tmax), ylim = c(0, pmax), xlab="Tiempo", ylab="Poblaci\u{F3}n", col = "red")
 #   points(1:tiempo, povejas, xlim=c(0, tmax), ylim = c(0, pmax), pch=16, col = "blue")
  #  graphics.off()
#}

system("convert -delay 50 -size 300x300 lotka_t*.png -loop 0 lotka.gif")
system("convert -delay 50 -size 300x300 poblacion_t*.png -loop 0 poblacion.gif")

# rect(-0.9, 1.9, -0.5, 1.2, col = "magenta")
#grid(nx = 1000, ny = 1000, col = "chartreuse", lty = "dotted",
#    lwd = par("lwd"), equilogs = TRUE)
