# lobos-ovejas version 1.0 
# agregamos "energia" a los lobos
# version paralelizada

suppressMessages(library(doParallel))
cluster <- makeCluster(detectCores()-1)

l <- 1 # region
n <- 70 # poblacion inicial total (ambas especies)
nlobos <- sample(1:n, 1) # cuantos lobos en poblacion inicial

prl <- 0.10 # prob reproduccion lobos
pro <- 0.05 # prob reproduccion ovejas
pml <- 0.02 # prob muerte lobos
pmo <- 0.02 # prob muerte ovejas

v <- l/30 # tamano de paso
r <- 0.05 # umbral para comer
poblacion <- data.frame(x = double(), y = double(), dx = double(), dy = double(), especie  = character(), energia = integer())

plobos <- numeric() # poblacion lobos a lo largo del tiempo
povejas <- numeric() # poblacion ovejas a lo largo del tiempo

p_inicial <- function() {
	if(i <= nlobos) {
    	e <- "L"
		ener <- sample(1:5, 1)
    } else {
        e <- "O"
		ener <- 0
    } 
    inicio <- data.frame(x = runif(1, 0, l), y = runif(1, 0, l), dx = runif(1, -v, v), 
						dy = runif(1, -v, v), especie = e, energia = ener)
}

movimiento <- function(i) {
    ind <- poblacion[i, ]
    ind$x <- ind$x + ind$dx
    ind$y <- ind$y + ind$dy
    if (ind$x > l) {
        ind$x <- ind$x - v
        ind$dx <- runif(1, -v, v)
    }
    if (ind$y > l) {
    	ind$y <- ind$y - v
    	ind$dy <- runif(1, -v, v)
    }
    if (ind$x < 0) {
        ind$x <- ind$x + v
        ind$dx <- runif(1, -v, v)
    }
   	if (ind$y < 0) {
        ind$y <- ind$y + v
        ind$dy <- runif(1, -v, v)
    }
    return(ind)
}
registerDoParallel(cluster)
poblacion <- foreach(i = 1:n, .combine=rbind) %dopar% p_inicial()

iL <- poblacion[poblacion$especie == "L",]
iO <- poblacion[poblacion$especie == "O",]

system("rm -f pr1_lotka-*.png")
system("rm -f pr1_poblacion-*.png")

tmax <- 40
digitos <- floor(log(tmax, 10)) + 1

tl <- "0"
while (nchar(tl) < digitos) {
   tl <- paste("0", tl, sep="")
}
png(paste("pr1_lotka-", tl, ".png", sep=""))
plot(l, type="n", xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y", main="Poblaci\u{F3}n inicial", axes = FALSE)
rect(0, 0, l, l, col = "green")
if (dim(iL)[1] > 0) {
    points(iL$x, iL$y, pch=15, col="black")
}
if (dim(iO)[1] > 0) {
    points(iO$x, iO$y, pch=16, col="white")
}
graphics.off()

for (tiempo in 1:tmax) {
    # comen lobos - muerte ovejas
    lobos <- which(poblacion$especie == "L", arr.ind = T)
    ovejas <- which(poblacion$especie == "O", arr.ind = T) 
    if (length(lobos) != 0) { # si hay lobos 
        for (i in lobos) {
        	comida = 0
            lobo <- poblacion[i, ]
            if (length(ovejas) != 0) { # si hay ovejas
            	for(j in ovejas) {
                	oveja <- poblacion[j, ]
                	dx <- lobo$x - oveja$x
                	dy <- lobo$y - oveja$y
                	d <- sqrt(dx^2 + dy^2)
                	if (d < r) {
                		comida = comida + 1
                	    poblacion[j, "energia"] = NA # muere oveja
                	}
            	}
            }
            if (comida == 0) {
            	poblacion[i, "energia"] = poblacion[i, "energia"] - 1 # disminuye energia 
            } else {
            	poblacion[i, "energia"] = poblacion[i, "energia"] + 1 # aumenta energia 
            }
        }
    }
    poblacion <- poblacion[!is.na(poblacion$energia), ] # actualizo poblacion
    
    # reproduccion ovejas
    ovejas <- which(poblacion$especie == "O", arr.ind = T)
    if (length(ovejas) != 0) { # si hay ovejas puedo reproducir
        for (i in ovejas) {
            if (runif(1) < pro) { # reproduccion
                poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), 
                					dx = runif(1, -v, v), dy = runif(1, -v, v), especie = "O", energia = 0))
            } else if (runif(1) < pmo) { # muerte "natural"
            	poblacion[i, "energia"] = NA
            }
        }
    }
    poblacion <- poblacion[!is.na(poblacion$energia), ] # acutualiza poblacion
    
    # muerte y reproduccion lobos
    lobos <- which(poblacion$especie == "L", arr.ind = T)
    ovejas <- which(poblacion$especie == "O", arr.ind = T)
    if (length(lobos) != 0) { # hay lobos
        for (i in lobos) {
        	energy = poblacion[i, "energia"] 
            if ( energy < 0 ) { # muerte
                poblacion[i, "energia"] = NA
            } else if (runif(1) < prl) { # reproduccion
               	poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), 
                					dx = runif(1, -v, v), dy = runif(1, -v, v), especie = "L", energia = sample(1:5,1)))
            }
        }
    }
    poblacion <- poblacion[!is.na(poblacion$energia), ] # acutualiza poblacion
    
    # desplazamiento
    if (dim(poblacion)[1] == 0) {
    	tparada = tiempo
    	break;
    } else {
    	tparada = tiempo
        poblacion <- foreach(i = 1:n, .combine=rbind) %dopar% movimiento(i)
        stopImplicitCluster()
    }
        
    iL <- poblacion[poblacion$especie == "L",]
    iO <- poblacion[poblacion$especie == "O",]
        
        # graficamos...
        tl <- paste(tiempo, "", sep="")
        while (nchar(tl) < digitos) {
   			tl <- paste("0", tl, sep="")
		}
        salida <- paste("pr1_lotka-", tl, ".png", sep="")
        paso <- paste("Paso", tiempo)
        png(salida)
        plot(l, type="n", xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y", main=paso, axes = FALSE)
        rect(0, 0, l, l, col = "green")
        if (dim(iL)[1] > 0) {
            points(iL$x, iL$y, pch=15, col="black")
        }
        if (dim(iO)[1] > 0) {
            points(iO$x, iO$y, pch=16, col="white")
        }
        grid(nx = 20, ny = 20, col = "black", lty = "dotted")
        graphics.off()
        
        plobos <- c(plobos, dim(iL)[1])
        povejas <- c(povejas, dim(iO)[1])
}

maxi <- max(plobos, povejas)
for (tiempo in 1:tparada) {
	tl <- paste(tiempo, "", sep="")
    while (nchar(tl) < digitos) {
   		tl <- paste("0", tl, sep="")
	}
	paso <- paste("Paso", tiempo)
	lobos <- plobos[1:tiempo]
	ovejas <- povejas[1:tiempo]
    png(paste("pr1_poblacion-", tl, ".png", sep=""), width=600, height=300)
    plot(l, type="n", xlim=c(0, tparada), ylim = c(0, maxi), xlab="Tiempo", ylab="Poblaci\u{F3}n", main=paso)
	points(1:tiempo, lobos, pch=15, col = "black")
	points(1:tiempo, ovejas, pch=16, col = "blue")
  	graphics.off()	
}

system("convert -delay 40 -size 300x300 pr1_lotka-*.png -loop 0 pr1_lotka.gif")
system("convert -delay 40 -size 300x300 pr1_poblacion-*.png -loop 0 pr1_poblacion.gif")
