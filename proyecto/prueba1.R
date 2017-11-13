
l <- 1 # region
n <- 50 # individuos (ambas especies)
pl <- 0.4 # es lobo

prl <- 0.02 # prob reproduccion lobos
pro <- 0.03 # prob reproduccion ovejas

ec <- 5 # pasos en el tiempo de espera para volver a comer
em <- 3 # lobos que no se alimenten despues de este tiempo moriran

r <- 0.1 # radio en el que deben estar las ovejas para que el lobo las coma

# I N I C I O
for (i in 1:n) {
	e <- "O"
    if (runif(1) < pl) {
        e <- "L"
    } 
    poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), dx = runif(1, -v, v), dy = runif(1, -v, v), especie = e))
}



# INTERACCION

