library(gutenbergr)
library(dplyr)
library(tidytext)
library(fitdistrplus)


alice <- gutenberg_download(c(11))
letras = alice %>% unnest_tokens(chars, text, "characters") #tomar las letras del libro.
palabras = alice%>% unnest_tokens(word, text, "words") #tomar las palabras del libro.
oraciones = alice %>% unnest_tokens(sentences, text, "sentences")
oraciones$sentences <- gsub(x = oraciones$sentences, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = " ")
parrafos = alice %>% unnest_tokens(paragraphs, text, "paragraphs")

largo_oraciones<-c()
for (i in 1:1620) {
    largo_oraciones[i] <-sapply(strsplit(as.character(oraciones[i,2]), " "), length)
}

png(file="Largo_oraciones.png",
    width=3000, height=2000, res = 400)
hist(largo_oraciones, main=" ", ylab="Frecuencias", xlab="Largo de las oraciones", col = palette("Pastel 2"), breaks = seq(0,255,15)) 
dev.off()

fitdist(largo_oraciones, "geom")

png(file="Largo_oraciones_distribucion.png",
    width=3000, height=2000, res = 400)
hist(rgeom(length(largo_oraciones),0.04645561), breaks = seq(0,255,15), main= " ", ylab="Frecuencias", xlab=" ", col=palette("Pastel 2"))
dev.off()

largo_parrafos<-c()
for (i in 1:819){
  largo_parrafos[i]<-sapply(strsplit(as.character(parrafos[i,2]), " "), length)
}

png(file="Largo_parrafos",
    width=3000, height=2000, res = 400)
hist(largo_parrafos, main=" ", ylab="Frecuencias", xlab= "Largo de los parráfos", col=palette("Pastel 2"),breaks = seq(0,500,25))
dev.off()

fitdist(largo_parrafos, "geom")

png(file="Largo_parrafos_distribucion.png",
    width=3000, height=2000, res = 400)
hist(rgeom(length(largo_parrafos), 0.02887361), breaks = seq(0,500,25), main= " ", ylab="Frecuencias", xlab=" ", col=palette("Pastel 2"))
dev.off()







es_alicia<-function(palabra){
  if(palabra=="alice"){
    return(1)
  } else{
    return(0)
  }
}

palabras$exitos <- sapply(palabras$word, es_alicia)
a<-as.numeric(palabras$exitos)
r<-rle(a)
ceros<-as.numeric(r$lengths[r$values==0])

png(file="simulacion.png",
    width=3000, height=2000, res = 400)
hist(ceros,breaks = seq(0,500,50), col=palette("Pastel 2"), main=" ", ylab="Frecuencias", xlab= " ")
dev.off()

mean(ceros)

fitdist(ceros,"geom")

png(file="simulacion_distribucion.png",
    width=3000, height=2000, res = 400)
hist(rgeom(length(ceros),0.01490972), breaks = seq(0,500,50),col=palette("Pastel 2"), main=" ", ylab="Frecuencias", xlab= " ")
dev.off()

     