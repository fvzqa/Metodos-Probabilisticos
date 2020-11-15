library(gutenbergr)
library(dplyr)
library(tidytext)
library(fitdistrplus)

#Exponencial
la = 5
me = 3
ce = numeric()
repl = 10000
br = 5

for (replica in 1:repl) {
  de = numeric()
  
  while (sum(de) < me) {
    de = c(de, rexp(1, la))
  }
  ce = c(ce, length(de)-1)
}

png(file="prueba1.png",
    width=3000, height=2000, res = 400)
par(mfrow=c(1,2))
hist(ce,col='darkslategray4', main='',ylab="Frecuencias", xlab= " ")
hist(rpois(replica,me*la),col='lavenderblush3', main='',ylab="Frecuencias", xlab= " ")
dev.off()


require(vcd)
gf<-goodfit(ce, type = "poisson", method = "MinChisq")
gf$par

#uniforme
la = 10
mu = exp(-la)
cu = numeric()
repl = 10000


for (replica in 1:repl) {
  du = c(1)
  while (prod(du) > mu) {
    du = c(du, runif(1))
  }
  cu = c(cu, length(du)-2)
}


png(file="unif2.png",
    width=3200, height=2200, res = 400)
par(mfrow=c(1,2))
hist(cu,col='darkslategray4', main='',ylab="Frecuencias", xlab= " ", breaks=seq(0,28,3))
hist(rpois(repl,la),col='lavenderblush3', main='',ylab="Frecuencias", xlab= " ", breaks=seq(0,28,3))
dev.off()



gf<-goodfit(cu, type = "poisson", method = "MinChisq")
gf$par

#libro
alice <- gutenberg_download(c(11))
palabras = alice%>% unnest_tokens(word, text, "words") #tomar las palabras del libro.

tiempos<-numeric()
for (j in 1:26694){
  if(palabras$word[j]=='alice'){
    tiempos<-append(tiempos,j)
  }
}

diferencia<-numeric()
diferencia<-append(diferencia,tiempos[1])
for (i in 2:length(tiempos)-1){
  diferencia<-append(diferencia, tiempos[i]-tiempos[i-1])
}

png(file="libroexp.png",
    width=2200, height=2200, res = 400)
hist(diferencia/100,breaks=seq(0,5,1),col='darkslategray4', main='',ylab="Frecuencias", xlab= " ")
dev.off()

png(file="generadosexp.png",
    width=2200, height=2200, res = 400)
hist(rpois(381,0.68), breaks = seq(0,5,1), col='lavenderblush3', main='',ylab="Frecuencias", xlab= " ")
dev.off()