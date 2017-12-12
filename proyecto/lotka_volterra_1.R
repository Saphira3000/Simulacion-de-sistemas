# lobos-ovejas version 1.1 
# agregamos "energia" a los lobos

l <- 1 # region
n <- 60 # poblacion inicial total (ambas especies)
nlobos <- seq(10, 50, 10) # cuantos lobos en poblacion inicial

prl <- 0.10 # prob reproduccion lobos
pro <- 0.10 # prob reproduccion ovejas
pml <- 0.02 # prob muerte lobos
pmo <- 0.02 # prob muerte ovejas

v <- l/30 # tamano de paso
r <- 0.05 # umbral para comer
poblacion <- data.frame(x = double(), y = double(), dx = double(), dy = double(), especie  = character(), energia = numeric())

plobos <- numeric() # poblacion lobos a lo largo del tiempo
povejas <- numeric() # poblacion ovejas a lo largo del tiempo

p_lobos <- numeric() # poblacion lobos a lo largo del tiempo
p_ovejas <- numeric() # poblacion ovejas a lo largo del tiempo

replicas <- 10
for (lob in nlobos) {
	for (rep in 1:replicas) {
		# ----- INICIO -----
		for (i in 1:n) {
			if(i <= lob) {
	    		e <- "L"
				ener <- sample(1:5, 1)
	    	} else {
	        	e <- "O"
				ener <- 0
	    	} 
	    	poblacion <- rbind(poblacion, data.frame(x = runif(1, 0, l), y = runif(1, 0, l), dx = runif(1, -v, v), 
							dy = runif(1, -v, v), especie = e, energia = ener))
		}
		iL <- poblacion[poblacion$especie == "L",]
		iO <- poblacion[poblacion$especie == "O",]
		
		tmax <- 50

		# ----- MOVIMIENTO EN EL TIEMPO -----
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
    	        		poblacion[i, "energia"] = poblacion[i, "energia"] - 0.5 # disminuye energia 
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
            		if ( energy < 0 || runif(1) < pml) { # muerte
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
        		for (i in 1:dim(poblacion)[1]) {
            		ind <- poblacion[i, ]
            		ind$x <- ind$x + ind$dx
            		ind$y <- ind$y + ind$dy
            		if (ind$x > l) {
                		ind$x <- ind$x - v
                		ind$dx <- runif(1, -v, 0)
            		}
            		if (ind$y > l) {
                		ind$y <- ind$y - v
                		ind$dy <- runif(1, -v, 0)
           			}
            		if (ind$x < 0) {
                		ind$x <- ind$x + v
                		ind$dx <- runif(1, 0, v)
            		}
            		if (ind$y < 0) {
                		ind$y <- ind$y + v
                		ind$dy <- runif(1, 0, v)
                	}
            		poblacion[i, ] <- ind
        		}
        	}
       		iL <- poblacion[poblacion$especie == "L",]
        	iO <- poblacion[poblacion$especie == "O",]
        	
        	p_lobos <- c(p_lobos, dim(iL)[1]) 
			p_ovejas <- c(p_ovejas, dim(iO)[1])
    	}
    plobos <- c(plobos, mean(p_lobos))
    povejas <- c(povejas, mean(p_ovejas))
	}
}

write.csv(povejas, "poblacionOvejas.csv")
write.csv(plobos, "poblacionLobos.csv")

ovejas <- matrix(povejas, ncol=replicas, byrow=T)
t_ovejas <- t(ovejas)
colnames(t_ovejas) <- nlobos
png("ovejas.png")
boxplot(t_ovejas, use.cols=FALSE, xlab="poblaci\u{F3}n inicial lobos", ylab="poblacion ovejas")
graphics.off()
