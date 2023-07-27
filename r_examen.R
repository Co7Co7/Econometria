library(rmarkdown)

library(tidyverse)

library(broom) 

library(scales) 

library(modelsummary)

library(foreign)

library(dplyr)

file.choose()

data <- read.dta("D:\\ECONOMETRIA 2\\examen.dta")

attach(data)


data[,6] <- data.frame(lco2=log(co2))

names (data) [3] = "after" #la variable de año

tratamiento <- (data[5])   
names (tratamiento) [1] = "tratamiento"

data <- data.frame(data,tratamiento)

plot_data <- data %>% ##MUTATE ES PARA CONSIDERAR DIFERENTES TIPOS DE FILTROS 
  
  mutate(tratamiento = factor(tratamiento, labels = c("control", "Tratado")), ##PONGO PRIMERO CONTROL PORQUE EMPIEZA EN 0 QUE ES CONTROL
         
         after = factor(after, labels = c("Antes", "Despues"))) %>%  #control:0, tratamiento: 1
  
  group_by(tratamiento, after) %>% ##GROUPBY ES PARA AGRUPAR DOS FILTROS QUE SE HAN REALIZADO
  
  summarize(mean_lco2 = mean(lco2), ##SE ESCOGE BAJO EL CRITERIO DE MEDIA
            
            se_lco2 = sd(lco2) / sqrt(n()),
            
            upper = mean_lco2 + (1.96 * se_lco2),
            
            lower = mean_lco2 + (-1.96 * se_lco2))



ggplot(plot_data, aes(x = tratamiento, y = mean_lco2)) +
  geom_pointrange(aes(ymin = lower, ymax = upper),
                  color = "darkgreen", size = 1) +
  facet_wrap(vars(after))

# Del grafico se puede analizar que el impuesto verde no sirvio para reducir las emisiones de co2. Y vemos que años despues nuestra variable de control incluso a aumentado. 


ggplot(plot_data, aes(x = after, y = mean_lco2, color = tratamiento)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = tratamiento))

#La grafica nos indica que no ha tenido impacto y el grupo tratado continua siendo casi igual. 

data2 <- data %>%
  
  mutate(
    
    trat_aux = case_when(
      
      ambiental == "1" & after == "1" ~ 1,   #donde dice dfmfd va la variable que estamos analizando
      
      ambiental == "0" & after == "1" ~ 0))  #donde dice dfmfd va la variable que estamos analizando


data2$trat_aux[is.na(data2$trat_aux)]= 0   #aqui se cambia los datos perdidos (n.a) por 0

data3 <- within(data2, {trat = ave(data2$trat_aux,data2$id)})
#AQUI ESTAMOS CREANDO UNA MEDIA PONDERADA DONDE TENEMOS LA VARIABLE DE LOS mujeres 
#MEDIA PONDERADA ES EL PORCENTAJE DE PARTICIPACIÓN O INCIDENCIA SEGÚN LA FAMILIA
#nh es la variable de identificacion  

data4 <- data3 %>%
  
  mutate(
    trat = case_when(trat == "0.5" ~ 1,
                     
                     trat == "0"~ 0))

#modelo 


modelo1 <- lm(lco2 ~ trat + after + trat * after,
              data = data4)
#El grupo de tratamiento

summary(modelo1)

# Analisis

## Comenzamos diciendo que la variables no han sido estadisticamente significativas. 
## trat habla acerca del nivel de de emision de co2 segun Ecuador y el resto. Donde vemos que el grupo tratado 1 mantiene un mayor nivel de emision de co2
## after vemos que despues de la politica hay un ligero aumento en emisiones de co2. 
## Como el trat:after vemos que la politica no ha sido efectiva. 


