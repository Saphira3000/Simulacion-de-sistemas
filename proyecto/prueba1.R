# lobos-ovejas version 1.0 

l <- 1 # region
n <- 50 # individuos (ambas especies)
pl <- 0.4 # es lobo

prl <- 0.20 # prob reproduccion lobos
pro <- 0.10 # prob reproduccion ovejas

ph <- 0.50 # probabilidad minima hambrientos
pml <- 0.3 # probabilidad muerte lobos

#ec <- 5 # pasos en el tiempo de espera para volver a comer
#em <- 3 # lobos que no se alimenten despues de este tiempo moriran

v <- l/30 # tamano de paso
r <- 0.2 # radio en el que deben estar las ovejas para que el lobo las coma
poblacion <- data.frame(x = double(), y = double(), dx = double(), dy = double(), especie  = character())

plobos <- numeric() # porcentaje poblacion lobos a lo largo del tiempo
povejas <- numeric() # porcentaje poblacion ovejas a lo largo del tiempo

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

system("rm -f lotka_t*.png")
system("rm -f por_poblacion_t*.png")

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

for (tiempo in 1:tmax) {
    # hambriento??
    poblacion$comer <- NULL
    for (i in 1:dim(poblacion)[1]) { #dim(poblacion[poblacion$especie,])[1]) { 
        poblacion[i, "comer"] = 0;
        if (poblacion[i, "especie"] == "L") {
            if (runif(1) < ph) {
                poblacion[i, "comer"] = 1;
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
                poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), 
                                                         dx = runif(1, -v, v), dy = runif(1, -v, v), especie = "L", comer = round(runif(1))))
            }
        }
    }
    poblacion <- poblacion[!is.na(poblacion$comer), ]
    # desplazamiento
    if (dim(poblacion)[1] != 0) {
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
        
        # porcentaje poblaciones
        total = dim(poblacion[poblacion$especie,])[1]
        lobos = dim(iL)[1]
        ovejas = dim(iO)[1]
        plobos <- c(plobos, 100 * lobos / total)
        povejas <- c(povejas, 100 * ovejas / total)
        png(paste("por_poblacion_t", tl, ".png", sep=""), width=600, height=300)
        plot(1:tiempo, plobos, xlim=c(0, tmax), ylim=c(0, 100), xlab="Tiempo", ylab="Porcentaje poblaci\u{F3}n", col = "red")
        points(1:tiempo, povejas, xlim=c(0, tmax), ylim=c(0, 100), pch=16, col = "blue")
        graphics.off()
    }
}

system("convert -delay 50 -size 300x300 lotka_t*.png -loop 0 lotka.gif")
system("convert -delay 50 -size 300x300 por_poblacion_t*.png -loop 0 poblacion.gif")

# rect(-0.9, 1.9, -0.5, 1.2, col = "magenta")
#grid(nx = 1000, ny = 1000, col = "chartreuse", lty = "dotted",
#    lwd = par("lwd"), equilogs = TRUE)
