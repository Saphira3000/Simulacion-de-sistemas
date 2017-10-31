aciertos.par <- read.csv("aciertos_paralelo.csv")
aciertos.sec <- read.csv("aciertos_secuencial.csv")

#aciertos.par <- t(aciertos.par)
#aciertos.sec <- t(aciertos.sec)

paralelo <- aciertos.par[,2]
secuencial <- aciertos.sec[,2]

#print(paralelo)

png("p12_hist_par.png")
hist(paralelo)
graphics.off()

png("p12_hist_sec.png")
hist(secuencial)
graphics.off()

shapiro.test(paralelo)
shapiro.test(secuencial)

#wilcox.test(paralelo, secuencial, alternative = "g")
t.test(paralelo, secuencial) 
