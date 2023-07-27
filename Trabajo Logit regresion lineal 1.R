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


#INGRESO DE DATOS
datos <- read_excel("chongon2.xlsx")
View(datos)
attach(datos)
names(datos)

##modelo inicial 

logit <- glm(  S_basico ~ hombre + edad + casado + separado +viudo + divorciado + union_libre
               + bono+  pobreza + clases + Primaria +ningun0+ secundaria + universitario
               + minorias+ indigenas + mulatos + Blancos+ horas_trabajo+ experiencia+ contento ,
               data = datos, family = binomial("logit"))

summary(logit)

###modelo reducido con mis variables estadisticamente significativas, como aclaracion secundaria solo lo utilizamos porque aumentamos la significancia normalmente no se aceptaria al trabajar con un 0.05


logit2 <- glm(  S_basico~  casado   + secundaria + horas_trabajo
                , data = datos, family = binomial("logit"))

summary(logit2)

#Chi cuadrada

logitchi <- logit2$null.deviance - logit2$deviance

gl<- logit2$df.null - logit2$df.residual

p_valuelogitchi <- pchisq(q = logitchi,df = gl, lower.tail = FALSE)

p_valuelogitchi

#valor p menor a  0.05 se ajusta al modelo



#Matriz de clasificacion

predicciones <- ifelse(test = logit2$fitted.values > 0.5, yes = 1, no = 0)

matriz_clasificacion <- table(logit2$model$S_basico, predicciones,
                              
                              dnn = c("observaciones", "predicciones"))

(matriz_clasificacion[1,1] + matriz_clasificacion[2,2])/ sum(matriz_clasificacion) 


#Hosmer - Lemeshow

hoslem.test(datos$S_basico, fitted(logit2))

##Dado que el valor p del estadistico asociado 
# es mayor a 0.05 no hay evidencia para rechazar Ho
# el modelo se ajusta de buena manera a  nuestros dato




#Probabilidad segun variables

plot_model(logit2, type="eff")$horas_trabajo

#efectos marginales

logitescala <- mean(dlogis(predict(logit2, type="link")))
logitescala * coef(logit2)

#Nos permite ver propabilidad que tiene una variable respecto a otra
#si es casado tiene la probabilidad de ganar el sueldo basico de 73%

#Oddsratio

exp(coefficients(logit2))


#se demuestra que las variables casado tiene un impacto de 2.35 frente a 1
#una persona casada tiene la probabilidad de 2 veces mas de ganar mas del salario
#basico

plot_model(logit2)

#en el grafico podemos observar que casado tiene mayor incidencia en la probabilidad ya que esta mas lejos de 1

log.odds <- predict(logit2, data.frame(casado = 1,
                                       secundaria= 1,
                                       horas_trabajo= 40))

exp(log.odds)/(1+exp(log.odds))

#nos da la probabilidad de que una persona gane mas del salario basico