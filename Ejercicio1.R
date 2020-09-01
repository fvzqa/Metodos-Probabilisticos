#EJERCICIO

#inciso a) t1>m1


n<-100
v1 <- numeric(n)
v2 <- numeric(n)


w <-ceiling(n/2)
q <-w+1
r <-q+1
s <-q-1
y <-s+1
for(i in 1:q){
  v1[i] <- ceiling(runif(1,200,300))
}

for (i in r:n) {
  v1[i] <- ceiling(runif(1,750,800))
}

t1 <- mean(v1)
m1 <- median(v1)

########### b) t2<m2

for (i in 1:s){
  v2[i] <- ceiling(runif(1,200,600))
}

for (i in y:n){
  v2[i] <- ceiling(runif(1,601,800))
}

t2 <- mean(v2)
m2 <- median(v2)


#### 

boxplot(v1,v2)

####

library(ggplot2)


