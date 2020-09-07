Poblacion <- read.csv("/Users/fvzqa/OneDrive/Maestría/1 Semestre/Probabilidad/Tarea 1/Poblacion.csv", header = TRUE)
PobTotal <-as.numeric(Poblacion[1,2:6])
data<-Poblacion[2:33,2:6]
data2<-t(data)
Estados<-Poblacion[2:33,"X"]
n<-1000000
library(xtable)
print(xtable(Poblacion[2:6,]))


boxplot(data/n, main="Población en México",
        col=c("cadetblue4","coral3","lemonchiffon", 
              "darkgoldenrod","lightskyblue4"), xlab="Años", 
        names=c("1990", "1995", "2000","2005","2010"),
        las=1, ylab="Población en millones")

library(dplyr)
mayor1990<-arrange(Poblacion,-X1990)
mayor1995<-arrange(Poblacion,-X1995)
mayor2000<-arrange(Poblacion,-X2000)
mayor2005<-arrange(Poblacion,-X2005)
mayor2010<-arrange(Poblacion,-X2010)
print(xtable(mayor1990[2:6,]))

barplot(PobTotal/n,main = "Población de México", cex.axis=0.8,
        names=c("1990", "1995", "2000","2005","2010"), 
        ylab="Población en millones", xlab="Años",las=1, col=c("cadetblue4","coral3","lemonchiffon", "darkgoldenrod","lightskyblue4"))

boxplot(data2/n, names=Estados,las=2,cex.axis=0.5,ylab="Población en millones", main="Población de México", col="cadetblue4")

#Ordenar las celas para buscar los estados con mayor población
PoblacionMAYOR<-arrange(Poblacion,-X1990)
México<-as.numeric(Poblacion[16,2:6]/n)
CDMX<-as.numeric(Poblacion[10,2:6]/n)
Veracruz<-as.numeric(Poblacion[31,2:6]/n)
Jalisco<-as.numeric(Poblacion[15,2:6]/n)
Puebla<-as.numeric(Poblacion[22,2:6]/n)
Guanajuato<-as.numeric(Poblacion[12,2:6]/n)
boxplot(México,col="cadetblue4", xlab="México",ylab="Población en millones",main="Estado con mayor población", las=1, width = 1)
boxplot(CDMX,Veracruz,Jalisco,Puebla,Guanajuato,
        main="Estados más poblados de México", 
        col=c("cadetblue4","coral3","lemonchiffon", 
              "darkgoldenrod","lightskyblue4"),
        names=c("Cd. México","Veracruz","Jalisco","Puebla",
                "Guanajuato"), las=1, ylab="Población total en millones")



POB1990<-arrange(Poblacion[, c(1,2)],-X1990)
POB1995<-arrange(Poblacion[, c(1,3)],-X1995)
POB2000<-arrange(Poblacion[, c(1,4)],-X2000)
POB2005<-arrange(Poblacion[, c(1,5)],-X2005)
POB2010<-arrange(Poblacion[, c(1,6)],-X2010)

barplot(POB1990$X1990[2:6]/n,names=POB1990$X[2:6],las=2)
barplot(POB1995$X1995[2:6]/n,names=POB1995$X[2:6],las=2)
barplot(POB2000$X2000[2:6]/n,names=POB2000$X[2:6],las=2)
barplot(POB2005$X2005[2:6]/n,names=POB2005$X[2:6],las=2)
barplot(POB2010$X2010[2:6]/n,names=POB2010$X[2:6],las=2)

library(ggplot2) #Gráficos de violín
data3<-data.frame(name=c(Poblacion[c(16,10,31,15,22),1],
                         Poblacion[c(16,10,31,15,22),1],
                         Poblacion[c(16,10,31,15,22),1],
                         Poblacion[c(16,10,31,15,22),1],
                         Poblacion[c(16,10,31,15,22),1]), 
                  value=c(Poblacion[c(16,10,31,15,22),2],
                          Poblacion[c(16,10,31,15,22),3],
                          Poblacion[c(16,10,31,15,22),4],
                          Poblacion[c(16,10,31,15,22),5],
                          Poblacion[c(16,10,31,15,22),6]))
violinestados<-ggplot(data3, aes(x=name, y=value, fill=name)) + 
        geom_violin()
data4<-data.frame(name=c("1990","1995","2000","2005","2010"),
                  value=c(data$X1990,data$X1995,data$X2000,
                          data$X2005,data$X2010))

violinanos<-ggplot(data4, aes(x=name, y=value, fill=name))+
        geom_violin()


