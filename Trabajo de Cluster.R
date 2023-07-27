library(foreign) 
library(cluster)

file.choose()

data1 <- read.csv(file="C:\\Users\\PAOLA\\Desktop\\Pendrive\\analisis multivariado\\bancos.csv", sep=";", dec = ",")
data1 <- data1[c(-22),]

attach(data1)

x <- data1
c1 <- na.omit(x)    #tratar datos perdidos en este caso no hay
dfc1 <- data.frame(c1)

# K-Means Cluster No Jerarquico

mc1 <- kmeans(dfc1[2:5], 4) # 3 cluster solution

mc1

# Medias de los cl?sters 

aggregate(dfc1[2:5],by=list(mc1$cluster),FUN=mean)

# Asignacion de Clusters

mydata <- data.frame(dfc1, mc1$cluster)


plot(dfc1[c("ATP_PROD","MOR_CARTERA")], 
     
     col = mc1$cluster)

points(mc1$centers[,c("ATP_PROD","MOR_CARTERA")],
       
       col = 1:2, pch = 8,cex=2)

names(data1)


plot(dfc1[c("GAST_TOT_PROM","FOND_DIS_TOTAL_DEPOSITOS")], 
     
     col = mc1$cluster)

points(mc1$centers[,c("ATP_PROD","FOND_DIS_TOTAL_DEPOSITOS")],
       
       col = 1:2, pch = 8,cex=2)


clusplot(dfc1[2:5],
         
         mc1$cluster, 
         
         color=TRUE, 
         
         shade=TRUE, 
         
         labels=2, 
         
         lines=0)

row.names(c1) <- c1[,1] 

# Agrupacion Jerarquica = el programa define el numero de cluster

d <- dist(c1[2:5], method = "euclidean") # distancia

fit <- hclust(d, method="median") 

plot(fit, cex=0.8) # dendograma


groups <- cutree(fit, k=4) # corte en 4 clusters

rect.hclust(fit, k=4, border="red")

groups


