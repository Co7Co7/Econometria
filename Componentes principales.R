library(foreign)
library(openxlsx)
library(rmarkdown)
library(knitr)
library(psych)
library(ggplot2)

file.choose()
setwd("F:\\Descargas\\analisis multivariado\\tutoria")

library(readxl)
DATAPRINCIPAL <- read_excel("tutoria/componentes.xlsx")
View(DATAPRINCIPAL)

attach(DATAPRINCIPAL)

names(DATAPRINCIPAL)

cor(DATAPRINCIPAL)


#Alfa de Cronbach

psych::alpha(DATAPRINCIPAL)

#el raw_alpha es 0.65, por tanto si se acepta porque es mayor a 0.5-

#KMO

KMO(DATAPRINCIPAL)


#puesto que KMO(0.59) es mayor que 0.5 puedo continuar 


#Analisis para componentes principales

macp1 <- princomp(DATAPRINCIPAL, scores=TRUE, cor=TRUE)

summary(macp1)

#Solo nos quedamos con los componentes que tengan varianza mayor a 1
# Varianza es Standard Deviation al cuadrado

#Por tanto, nos quedamos con 2 componentes para la continuacion del proceso. 

#cargas de los componentes principales

loadings(macp1)

# grafico de sedimentacion o screeplot

plot(macp1)

abline(h=1, v=2)


# Biplot of score variables

biplot(macp1)


# Scores of the components

macp1$scores[1:8,]

score <- data.frame(macp1$scores)

score

#Analisis Factorial

maf1 <- factanal(DATAPRINCIPAL, factor=2,rotation="none")

maf1$loadings



maf2 <- factanal(DATAPRINCIPAL, factors=2,
                 
                 rotation="varimax",
                 
                 scores="regression")

maf2$loadings
#como despues de rotar, aun no se cumple los requisitos se debe analizar si es posible eliminar alguna de las variables.



#Analisis para componentes principales, aqui se analiza cual eliminar

loadings(macp1)

#segun mi analisis el menor valor entre las mayores cargas es 0.445 y se encuentra en x5 
DATA2<-DATAPRINCIPAL[1:70,1:4]
macp2 <- princomp(DATA2, scores=TRUE, cor=TRUE)
psych::alpha(DATA2)
KMO(DATA2)
summary(macp2)
#por tanto, me quedo con 2 componentes

loadings(macp2)

# Scores of the components 2

score2 <- data.frame(macp2$scores)

score2

#Analisis Factorial 2

maf3 <- factanal(DATA2, factor=2,rotation="none")

maf3$loadings



maf4 <- factanal(DATA2, factors=2,
                 
                 rotation="varimax",
                 
                 scores="regression")

maf4$loadings

#Por lo tanto, no se puede aplicar con 2 factores a pesar de que  anteriormente aparece que deberiamos usar 2 componentes