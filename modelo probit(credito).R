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


file.choose()

library(haven)
DATA <- read_sav("tree_credit.sav")
View(tree_credit)

attach(DATA)

names(DATA)

probit <- glm( DATA$Valoración_credito ~ Edad + Ingresos + Tarjetas_crédito + Educación + Créditos_coche, 
              
              data = DATA, family = binomial("probit"))

summary(probit)

probit <- glm( DATA$Valoraci?n_credito ~ Edad + Ingresos + Tarjetas_cr?dito + Educaci?n + Cr?ditos_coche, 
               
               data = DATA, family = binomial("probit"))

summary(probit)


#Usando solo las variables significativas 

probit <- glm( DATA$Valoración_credito ~ Edad + Ingresos + Tarjetas_crédito, 
              
              data = DATA, family = binomial("probit"))

summary(probit)


#Chi cuadrada

probitchi <- probit$null.deviance - probit$deviance

gl<- probit$df.null - probit$df.residual

p_valueprobitchi <- pchisq(q = probitchi,df = gl, lower.tail = FALSE)

p_valueprobitchi

#p valor es <0.05 se rechaza la ho. Por tanto tiene un buen ajuste y el modelo es significativo.


#Matriz de clasificacion

predicciones <- ifelse(test = probit$fitted.values > 0.5, yes = 1, no = 0)

matriz_clasificacion <- table(probit$model$`DATA$Valoración_credito`, predicciones,
                              
                              dnn = c("observaciones", "predicciones"))

(matriz_clasificacion[1,1] + matriz_clasificacion[2,2])/ sum(matriz_clasificacion) 

#el modelo esta clasificando bien los datos. El 80,5% esta bien clasificado.


#Hosmer - Lemeshow

#ho: correcto ajuste
#h1: incorrecto ajuste

hoslem.test(DATA$Valoración_credito, fitted(probit))


# Calcular los efectos marginales utilizando ggpredict()
marginal_effects <- ggpredict(probit, terms = "Edad")

# Graficar los efectos marginales
plot(marginal_effects)

#Efectos marginales


probitescala <- mean(dnorm(predict(probit, type="link")))

probitescala * coef(probit)


#Probabilidades

probprobit <- round(pnorm(((probit$coefficients[1])+(probit$coefficients[2])*39+(probit$coefficients[3])*2+(probit$coefficients[4])*2)),2)

probprobit