# proyecto: modelo depredador-presa (modelo Lotka-Volterra)
# se simula el modelo depredador-presa utilizando el enfque muti-agente
# noviembre 2017
# Gabriela Sanchez Y.

# la region donde viviran las especies sera (por ahora) el cuadrado unitario
l <- 1 # lado del cuadrado

# cuantos individuos habra en un inicio
n <- 30 # poblacion total, considera ambas especies
v <- l/50 # mallado??

# necesito las coordenadas de los individuos en el espacio, direccion en la que se mueven, velocidad de movimiento (uso que lobos se mueven mas rapido que las ovejas??)

# poblacion inicial (por ahora, al azar lobos y ovejas)
poblacion <- data.frame(x = double(), y = double(), dx = double(), dy = double(), especie  = character())

p <- 0.4 # prob q sea lobo

# definimos la especie de los individuos que habra en la poblacion inicial
for (i in 1:n) {
    if (runif(1) < 0.4) {
        e <- "L"
    } else 
    	e <- "O"
    poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), dx = runif(1, -v, v), dy = runif(1, -v, v), especie = e))
}


