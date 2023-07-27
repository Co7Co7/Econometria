install.packages("gmodels")
library(MASS) # discriminante
library(foreign) #spss, stata
library(gmodels) # tablas de contingencia
library(readxl)


# D=1 no default

# D=0 default


file.choose()

db <- read_excel("scoring.xlsx")
View(db)
names(db)
attach(db)


# FUNCION DISCIMINANTE

zlin=lda(df~.,data=db)

zlin


# Matriz de confusion:

table(predict(zlin)$class, df)

CrossTable(predict(zlin)$class,db$df
           
           ,expected=FALSE, prop.r=TRUE, prop.c=TRUE,
           
           prop.t=F, prop.chisq=F, chisq = FALSE, fisher=FALSE)


ct <- table(predict(zlin)$class, df)


# porcentaje correcto

sum(diag(prop.table(ct)))


names(db)


predict(zlin,newdata=data.frame(
  
  l_ingresos=7.2,dt_activos=0.48,
  
  numero_protestos=0,ant_lab_cal_a=0)) # proyeccion por casos



predict(zlin,newdata=data.frame(db)) # proyeccion para toda la base


#1 es buen pagador

#0 es mal pagador

