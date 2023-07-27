library(openxlsx)
library(foreign)
library(FactoMineR)
library(factoextra)
library(vctrs)

file.choose()

#Abrir la data y arreglar filas y columnas
setwd("C:\\Users\\PAOLA\\Desktop\\Pendrive\\analisis multivariado")
data <- read.xlsx(xlsxFile = 'index2021_data.xlsx')
row.names(data) <- data[,1] 
data <- data[,-1] 

#Prueba chi cuadrado
#Nota
#Recuerda se esta buscando que no exista igualdad de medias

chisq <- chisq.test(data)
chisq #Como el p valor es <0.05 se rechaza la ho. Por tanto no hay igualdad de medias


#Analisis de correspondencia

res.ca <- CA(data, graph = FALSE)

print(res.ca)

res.ca$eig


#Analisis grafico

fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))

fviz_ca_biplot(res.ca)

#Coordenadas

res.desc <- dimdesc(res.ca, axes = c(1,2))

head(res.desc[[2]]$row, 6)

head(res.desc[[2]]$col, 10)


#Contribucion de filas y columnas

row <- get_ca_row(res.ca)

row

head(row$contrib)

col <- get_ca_col(res.ca)

col

head(col$contrib)



