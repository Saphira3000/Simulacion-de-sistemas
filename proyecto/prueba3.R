# lobos-ovejas version 3.0
# interaccion con pasto 

l <- 1 # region
n <- 50 # individuos (ambas especies)

epsilon <- 0 # prob reproduccion lobos
alpha <- 0.08 # prob reproduccion ovejas

gama <- 0.3 # probabilidad muerte lobos
bet <- 0.2 # muerte de ovejas

v <- l/30 # tamano de paso

poblacion <- data.frame(x = double(), y = double(), dx = double(), dy = double(), especie  = character())

plobosi <- numeric() # porcentaje poblacion lobos a lo largo del tiempo
povejasi <- numeric() # porcentaje poblacion ovejas a lo largo del tiempo

# INICIO
for (i in 1:n) {
  e <- "O"
  if ( i == n ) {
    e <- "L"
  } 
  poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), dx = runif(1, -v, v), dy = runif(1, -v, v), especie = e))
}

iL <- poblacion[poblacion$especie == "L",]
iO <- poblacion[poblacion$especie == "O",]


