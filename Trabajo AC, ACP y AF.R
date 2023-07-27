library(foreign)
library(openxlsx)
library(rmarkdown)
library(knitr)
library(psych)
library(ggplot2)

file.choose()


ACP1 <- read.spss(file="C:\\Users\\PAOLA\\Desktop\\Pendrive\\analisis multivariado\\soccer_fan_satisfaction.sav",use.value.labels=FALSE,to.data.frame=TRUE)

attach(ACP1)
names(ACP1)
cor(ACP1)

data <- data.frame(na.omit(ACP1))
data

#Alfa de Cronbach

psych::alpha(data)


#KMO

KMO(data)

#puesto que KMO es mayor que 0.5 puedo continuar 

#utilizar ACP, sin embargo, realizamos el analisis a manera de ejemplo, utilizando pincomp
#princomp viene por default en la base de R

macp1 <- princomp(data, scores=TRUE, cor=TRUE)

summary(macp1)

##Notas
#El Standard Deviation hay que elevarlo al cuadrado para encontrar los valores propios
#El criterio es el siguiente: se retiene el numero de componentes principales si el valor propio es mayor que uno


# Cargas de los componentes principales

loadings(macp1)

#Devuelve dos matrices: una de los componentes con sus cargas
#Otra con la comunalidad SS Loadings


# Grafico de sedimentacion o screeplot

plot(macp1)

abline(h=1, v=3)


# Biplot of score variables

biplot(macp1)

# Scores of the components

macp1$scores[1:10,]

score <- data.frame(macp1$scores)

score

#Analisis Factorial

maf1 <- factanal(data, factor=3,rotation="none")

maf1$loadings



maf2 <- factanal(data, factors=3,
                 
                 rotation="varimax",
                 
                 scores="regression")

maf2$loadings


fit.pca=principal(data,nfactors=3,rotate="varimax",scores=TRUE)

fit.pca$scores


data[,8:10] <- fit.pca$scores
