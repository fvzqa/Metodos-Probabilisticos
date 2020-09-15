library(gutenbergr)
library(dplyr)
library(tidytext)
library(curl)
library(stringr)
library(stringi)
library(tidyr)
library(tidyverse)
library(janeaustenr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)



alice <- gutenberg_download(c(11))
letras = alice %>% unnest_tokens(chars, text, "characters") #tomar las letras del libro.
palabras = alice%>% unnest_tokens(word, text, "words") #tomar las palabras del libro.




#Letras
tbletras<-as.data.frame(table(letras$chars))
lmu<-as.data.frame(tbletras[tbletras$Freq>1,]) #HAY QUE CAMBIAR LOS NOMBRES DE LAS COLUMNAS
lmuordenadas<-arrange(lmu,Freq) #ordenar los datos de menos a más.

png("Letras.png",
    width=3500, height=1800, res = 400)
barplot(lmuordenadas$Freq, names.arg = lmuordenadas$Var1, log="y",col=palette("Pastel 2"), ylab="Escala logarítmica")
dev.off()


#palabras
barplot(table(palabras$word))
tbpalabras<-as.data.frame(table(palabras$word))
pmu<-as.data.frame(tbpalabras[tbpalabras$Freq>100,])
pmuorden<-arrange(pmu,-Freq)

png(file="Palabras.png",
    width=4000, height=1800, res = 400)
barplot(pmuorden$Freq, names.arg=pmuorden$Var1, log="y", col=palette("Pastel 2"), ylab="Escala logarítmica", las=2)
dev.off()



#personajes
personajes<-arrange(as.data.frame(pmu[pmu$Var1=='rabbit'|pmu$Var1=='alice'|pmu$Var1=='hatter'|
      pmu$Var1=='queen'|pmu$Var1=='cat'|pmu$Var1=='caterpillar'|pmu$Var1=='march'
      |pmu$Var1=='duchess'|pmu$Var1=='mouse'|pmu$Var1=='dodo'|pmu$Var1=='lory'
      |pmu$Var1=='bill'|pmu$Var1=='dormouse'|pmu$Var1=='gryphon'|pmu$Var1=='mock'|pmu$Var1=="king",]),-Freq)

png(file="Personajes.png",
    width=2100, height=1800, res = 400)
barplot(personajes$Freq, names.arg = personajes$Var1, col=palette("Pastel 2"), las=2)
dev.off()


#Palabras sin articulos y pronombres
tidy_alice<-alice%>%unnest_tokens(word,text)%>%anti_join(stop_words)
palabrasmas<-tidy_alice%>%count(word,sort = TRUE)
maspalabras<-as.data.frame(palabrasmas[palabrasmas$n>15&palabrasmas$n<100,])
png(file="Palabramss.png",
    width=5000, height=2000, res = 400)
barplot(maspalabras$n, names.arg=maspalabras$word, col=palette("Pastel 2"), las=2)
dev.off()




#Sentimientos

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")

nrc_angry <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")

alice_angry<-tidy_alice %>%
  inner_join(nrc_angry) %>%
  count(word, sort = TRUE)

alice_joy<-tidy_alice%>%
  inner_join(nrc_joy)%>%
  count(word,sort = TRUE)

bing_word_counts <- tidy_alice %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

png(file="positivo-negativo.png",   
    width=2000, height=1500, res = 400)
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment, colors(palette("Pastel 2")))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Frecuencia",
       x = "Sentimientos") +
  coord_flip()
dev.off()

png(file="cloudpalabrasmas.png",   
    width=1500, height=1500, res = 400)
tidy_alice %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette("Tableau 10")))
dev.off()


library(reshape2)

png(file="cloud.png",   
    width=1500, height=2000, res = 400)
tidy_alice %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
dev.off()
