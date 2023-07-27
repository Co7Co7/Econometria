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
library(foreign)
library(parameters)
library(rstatix)

file.choose()
setwd("F:\\Descargas\\analisis multivariado\\TALLER")
DATAAGRICULTURA <- read.xlsx(xlsxFile = 'agricultura.xlsx')

boxplot(DATAAGRICULTURA$`Suma.de.Agricultura(2015)`)
outlier(DATAAGRICULTURA$`Suma.de.Agricultura(2015)`)
chisq.out.test(DATAAGRICULTURA$`Suma.de.Agricultura(2015)`)
dixon.test(DATAAGRICULTURA$`Suma.de.Agricultura(2015)`)

#se aplica log a las dos variables a tratar para podeer disminuir los margenes, y tratar los datos atipicos
DATAAGRICULTURA$LOGAgricultura2015<-log(DATAAGRICULTURA$`Suma.de.Agricultura(2015)`)
DATAAGRICULTURA$LOGAgricultura2018<-log(DATAAGRICULTURA$`Suma.de.Agricultura(2018)`)
boxplot(DATAAGRICULTURA$LOGAgricultura2015)
outlier(DATAAGRICULTURA$LOGAgricultura2015)
chisq.out.test(DATAAGRICULTURA$LOGAgricultura2015)

boxplot(DATAAGRICULTURA$LOGAgricultura2018)
outlier(DATAAGRICULTURA$LOGAgricultura2018)
chisq.out.test(DATAAGRICULTURA$LOGAgricultura2018)

#como en ambas pruebas el pvalor>0,05 se acepta la ho que no tienen valor atipico 

#determinar si es parametrica o no parametrica.
#Ho: normal
#H1: no es normal
shapiro.test(DATAAGRICULTURA$LOGAgricultura2015)
shapiro.test(DATAAGRICULTURA$LOGAgricultura2018)

#como ambos test indica que hay normalidad, se revisa si existe homogeneidad en varianza.

#Homogeneidad de Varianza


#Como no existe normalidad. Se utiliza pruebas no parametricas. 
#Prueba t para muestras relacionadas
#ho: antes = despues
#h1: antes diferente despu?s

bartlett.test(DATAAGRICULTURA$LOGAgricultura2015, DATAAGRICULTURA$CATEGORIZACION)
bartlett.test(DATAAGRICULTURA$LOGAgricultura2018, DATAAGRICULTURA$CATEGORIZACION)

#como en ambas pruebas el pvalor>0,05 se acepta la ho. Por lo tanto existe homogeneidad de varianza. 
#por lo tanto se utiliza una prueba parametrica, en este caso por ser observaciones de tipo longitudinal.
#se debe utilizar la prueba t para muestras relacionadas. 

#Prueba t para muestras relacionadas
#ho: antes = despues
#h1: antes diferente despu?s
prueba_t_pareadas <- t.test(x= DATAAGRICULTURA$LOGAgricultura2015, y= DATAAGRICULTURA$LOGAgricultura2018, paired=T, mu=0, conf.level=0.95, alternative="t")
parameters(prueba_t_pareadas)
#como el pvalor>0,05 se acepta la ho. Por lo tanto, y no hubo un crecimiento significativo segun provincias entre el 2015 y 2018 en la agriculturas.

#ho: antes <= despues

#h1: antes > despues

prueba_t_pareadas <- t.test(x= DATAAGRICULTURA$LOGAgricultura2015, y= DATAAGRICULTURA$LOGAgricultura2018, paired=T, mu=0, conf.level=0.95, alternative="g")

parameters(prueba_t_pareadas)

#como el pvalor>0,05 se acepta la ho. Por lo tanto, el sector de la agricultura del 2015 es menor o igual al del 2018

boxplot(DATAAGRICULTURA$LOGAgricultura2015, DATAAGRICULTURA$LOGAgricultura2015)

#como se puede observar en el grafico se cumplen las concluciones sacadas en las pruebas t. 


#Para la segunda parte del ejercicio
#Ya se demostro en la primera parte que si hubo normalidad y homogenidad en las varianzas. 
#por lo tanto sabemos que se debe realizar una prueba parametrica.
#pero para este caso de observaciones transversales se utiliza la prueba ANOVA

anova2015<- aov(LOGAgricultura2015 ~ CATEGORIZACION, DATAAGRICULTURA)
summary(anova2015)
##Ho: u1 = u2 = u3
#H1: las u no son iguales 

#por el p valor<0.05 es se rechaza la ho.Por tanto,
#Por las medias entre las regiones del Ecuador en el 2015 no fueron iguales.

prueba_anova2015 <- pairwise.t.test(DATAAGRICULTURA$LOGAgricultura2015, DATAAGRICULTURA$CATEGORIZACION, p.adjust="bonferroni", pool.sd = T, data=anova2015)
parameters(prueba_anova2015)

#El test de bonferoni como se divide las medias del 2015 segun la region.
#Conclusion1 que podemos sacar. Que region sierra(2) tiene una mayor media en produccion en agricultura que en el oriente.
#COnclusion2. Que region Costa(1) tiene una mayor media en produccion en agricultura que en el oriente.
#COnclusion3 Que region sierra(2) tiene una menor media en produccion en agricultura que la Costa.




#Ahora con el a?o 2018
anova2018<- aov(LOGAgricultura2018 ~ CATEGORIZACION, DATAAGRICULTURA)
summary(anova2018)
##Ho: u1 = u2 = u3
#H1: las u no son iguales 

#Por las medias entre las regiones del Ecuador no fueron iguales.
#por el p valor<0.05 es se rechaza la ho.Por tanto,
#Por las medias entre las regiones del Ecuador en el 2018 no fueron iguales.

prueba_anova2018 <- pairwise.t.test(DATAAGRICULTURA$LOGAgricultura2018, DATAAGRICULTURA$CATEGORIZACION, p.adjust="bonferroni", pool.sd = T, data=anova2018)
parameters(prueba_anova2018)

#El test de bonferoni como se divide las medias del 2018 segun la region.
#Conclusion1 que podemos sacar. Que region sierra(2) tiene una mayor media en produccion en agricultura que en el oriente.
#COnclusion2. Que region Costa(1) tiene una mayor media en produccion en agricultura que en el oriente.
#COnclusion3 Que region sierra(2) tiene una menor media en produccion en agricultura que la Costa.

