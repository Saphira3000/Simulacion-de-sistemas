# version 1.0 (consideramos las mismas velocidades lobo-oveja)
# proyecto: modelo Lotka-Volterra 
# se simula el modelo depredador-presa utilizando el enfoque multi-agente
# noviembre 2017
# Gabriela Sanchez Y.

# la region donde viviran las especies sera (por ahora) el cuadrado unitario
l <- 1 # lado del cuadrado

n <- 30 # poblacion total inicial, considera ambas especies
v <- l/50 # tamaño de paso

# necesito las coordenadas de los individuos en el espacio, direccion en la que se mueven, velocidad de movimiento 
# ovejas se mueven mas lento que lobos

# poblacion inicial (por ahora, al azar lobos y ovejas)
poblacion <- data.frame(x = double(), y = double(), dx = double(), dy = double(), especie  = character())

p <- 0.4 # prob q sea lobo
r <- 0.2 # radio comer

# definimos la especie de los individuos que habra en la poblacion inicial
for (i in 1:n) {
    if (runif(1) < 0.4) {
        e <- "L"
    } else 
    	e <- "O"
    poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), dx = runif(1, -v, v), dy = runif(1, -v, v), especie = e))
}

# lobos comen a las ovejas (interaccion especies)
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
for (tiempo in 1:tmax) {
	lobos <- dim(poblacion[poblacion$especie == "L",])[1]
	ovejas <- dim(poblacion[poblacion$especie == "O",])[1]
	for (i in 1:lobos) {
		lobo <- poblacion[i,]
		for (j in 1:ovejas) {
			oveja <- poblacion[j,]
            dx <- lobo$x - oveja$x
            dy <- lobo$y - oveja$y
            d <- sqrt(dx^2 + dy^2)
            if (d < r) # comemos ovejas que se encuentran cerca
            	
        }
    }
}


