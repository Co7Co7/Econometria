library(foreign)
library(openxlsx)
library(knitr)
library(psych)
library(ggplot2)
library(tidyverse)
library(car)
library(modeest) #moda
library(raster) #cuantiles, coefificiente de variacion
library(moments) #asimetria, curtosis
library(dplyr)
library(datasets)
library(corrplot)
library(rmarkdown)
library(caret)
library(MASS)
library(tidyverse) #
library(lmtest) #PARA MULTICOLINALIDAD
library(stargazer) 
library(nortest) 
library(outliers)
library(stats)
library(vcd)
library(ResourceSelection)
library(sjPlot)
library(effects)
library(ggeffects)

install.packages("ResourceSelection")
install.packages("ggplot2")
install.packages("effects")
install.packages("ggeffects")

file.choose()

#Modelo logit para realizar un modelo de probabilidad segun las variables estadisticas significativas

DATA <- read.spss(file="C:\\Users\\PAOLA\\Desktop\\Pendrive\\analisis multivariado\\tree_credit.sav",
                  use.value.labels=FALSE,to.data.frame=TRUE)

library(haven)
DATA <- read_sav("tree_credit.sav")
View(tree_credit)

attach(DATA)

names(DATA)

logit <- glm( DATA$Valoración_credito ~ Edad + Ingresos + Tarjetas_crédito + Educación + Créditos_coche, 
              
             data = DATA, family = binomial("logit"))

summary(logit)

#Usando solo las variables significativas 

logit <- glm( DATA$Valoración_credito ~ Edad + Ingresos + Tarjetas_crédito, 
              
              data = DATA, family = binomial("logit"))

summary(logit)


#Chi cuadrada

logitchi <- logit$null.deviance - logit$deviance

gl<- logit$df.null - logit$df.residual

p_valuelogitchi <- pchisq(q = logitchi,df = gl, lower.tail = FALSE)

p_valuelogitchi

#p valor es <0.05 se rechaza la ho. Por tanto tiene un buen ajuste y el modelo es significativo.


#Matriz de clasificacion

predicciones <- ifelse(test = logit$fitted.values > 0.5, yes = 1, no = 0)

matriz_clasificacion <- table(logit$model$`DATA$Valoración_credito`, predicciones,
                              
                              dnn = c("observaciones", "predicciones"))

(matriz_clasificacion[1,1] + matriz_clasificacion[2,2])/ sum(matriz_clasificacion) 

#el modelo esta clasificando bien los datos. El 80,6% esta bien clasificado.


#Hosmer - Lemeshow

#ho: correcto ajuste
#h1: incorrecto ajuste

hoslem.test(DATA$Valoración_credito, fitted(logit))


# Calcular los efectos marginales utilizando ggpredict()
marginal_effects <- ggpredict(logit, terms = "Edad")

# Graficar los efectos marginales
plot(marginal_effects)


logitescala <- mean(dlogis(predict(logit, type="link")))

logitescala * coef(logit)
#probabilidades de cada una de las variables


#Oddsratio

exp(coefficients(logit))

plot_model(logit)

log.odds <- predict(logit, data.frame(Edad = 40,
                                      
                                      Ingresos= 3,
                                      
                                      Tarjetas_crédito= 1))

exp(log.odds)/(1+exp(log.odds))