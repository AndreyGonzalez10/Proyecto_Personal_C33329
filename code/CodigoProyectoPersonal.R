
#El presente script es donde se desarrolla el código del proyecto
#personal en su totalidad

#Primero se crean los dataframes con los autos eléctricos más vendidos
#en Costa Rica

df.dataframe.electricos <- data.frame(
  
  "Modelo" = modelo <-  c("GEELY GEOMETRY E", "BYD YUAN S1 PRO",
                          "BYD SEAGULL","CHERY EQ7", "BYD YUAN PLUS", 
                          "CHERY ICAR 03","VOLVO EX30","JAC EJS4"),
  "Precio" = precio <- c(22900, 29990, 21990, 34990, 
                         26500, 29990, 38000,25700),
  
  "Prima" = prima <- c(precio*(20/100)),
  
  "Deuda" = deuda <- c(precio-prima),
  
  "Pago mensual ($)" = pagos.mensuales.electricos

)


View(df.dataframe.electricos)



df.dataframe.gasolina <- data.frame (
  
  "Modelo" = modelo.gasolina <- c("Toyota RAV4", "Nissan Frontier", 
                                  "Toyota Hilux", "Suzuki Vitara",
                                  "Chery Tiggo 2","Toyota Raize",
                                  "Suzuki Jimny","Toyota Yaris Cross"),
  
  "Precio" = precio.gasolina <- c(34700, 42900, 48700, 26490, 22490, 
                                  20800,29990 ,29900),
  
  "Prima" = prima.gasolina <- c(precio.gasolina*20/100), 
  
  "Deuda" = deuda.gasolina <- c(precio.gasolina-prima.gasolina),
  
  "Pago mensual ($)" = pagos.mensuales.gasolina
)

View(df.dataframe.gasolina)

#Ahora con teoría del interés podemos crear una función de tal manera
#que según el plazo (96 meses), la deuda y la tasa de interés, podemos 
#calcular el pago mensual por cada auto 

calculadora.credito <- function(deuda, tasa.anual, periodo){
  r = tasa.anual/100
  i = (1 + r)^(1/12) - 1
  c = deuda*(i/(1-((1+i)^(-periodo))))
  return(c)
}


#Saquemos los pagos mensuales de autos eléctricos y de gasolina

pagos.mensuales.electricos <- c(calculadora.credito(deuda,10.70,96))
pagos.mensuales.electricos

pagos.mensuales.gasolina <- c(calculadora.credito(deuda.gasolina, 10.70, 96))
pagos.mensuales.gasolina

#Obtenemos los promedios
prom.pagos.mensuales.electricos <- mean(pagos.mensuales.electricos)
prom.pagos.mensuales.gasolina <- mean(pagos.mensuales.gasolina)


prom.pagos.mensuales.electricos
prom.pagos.mensuales.gasolina

#De lo anterior se puede ver cómo el adquirir un vehículo eléctrico
#representa un menor gasto mensual en el pago del crédito del vehículo

#Se importan librerías más pesadas para manejar data más pesada

library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)

#Resulta importante conocer el consumo eléctrico de los autos, 
#nos basaremos en las tarifas del ICE Residencial 

#Costo fijo en colones
costo.fijo.ICE <- 1348.35

#Costo por kwH en colones en el rango 0-140 (todos los autos lo cumplen)
costo.kwh <- 65.97

#Ahora se busca la capacidad de cada batería de cada auto de nuestra lista
#y se agrega como una nueva columna en el df original de eléctricos

df.dataframe.electricos <- df.dataframe.electricos %>%
  mutate(
    capacidad.bateria = case_when(
    modelo == "GEELY GEOMETRY E" ~ 39.4,
    modelo == "BYD YUAN S1 PRO" ~ 45.12,
    modelo == "BYD SEAGULL" ~ 38.88,      
    modelo == "CHERY EQ7" ~ 67.12,
    modelo == "BYD YUAN PLUS" ~ 60.48,     
    modelo == "CHERY ICAR 03" ~ 69.7,      
    modelo == "VOLVO EX30" ~ 69.0,         
    modelo == "JAC EJS4" ~ 55.0,
    TRUE ~ NA_real_
  ))
  
View(df.dataframe.electricos)

#Se hace ahora una nueva columna con el costo de cargar el auto 
#con las tarifas residenciales del ICE

costo.carga <- function(vector){
  resultado = costo.fijo.ICE + vector*65.97
  return (resultado)
}

df.dataframe.electricos <- df.dataframe.electricos %>% mutate(
  pago.carga = costo.carga(capacidad.bateria)
) 

View(df.dataframe.electricos)


#Se corrige el error que se pusieron dos columnas con capacidad 
#de la batería

df.dataframe.electricos <- df.dataframe.electricos %>% select(-capacidad_bateria)

#Se añade una columna con la autonomía de los carros eléctricos
#que también es importante para el análisis

df.dataframe.electricos <- df.dataframe.electricos %>%
  mutate(autonomia.km = case_when(
    modelo == "GEELY GEOMETRY E" ~ 380,
    modelo == "BYD YUAN S1 PRO" ~ 401,
    modelo == "BYD SEAGULL" ~ 405,
    modelo == "CHERY EQ7" ~ 512,
    modelo == "BYD YUAN PLUS" ~ 480,
    modelo == "CHERY ICAR 03" ~ 501,
    modelo == "VOLVO EX30" ~ 476,
    modelo == "JAC EJS4" ~ 385,
    TRUE ~ NA_real_
  ))

#Ahora se incluye la capacidad en litros de gasolina de los carros
#a gasolina 

df.dataframe.gasolina <- df.dataframe.gasolina %>%
  mutate(capacidad.litros = case_when(
    modelo.gasolina == "Toyota RAV4" ~ 55,
    modelo.gasolina == "Nissan Frontier" ~ 80,
    modelo.gasolina == "Toyota Hilux" ~ 80,
    modelo.gasolina == "Suzuki Vitara" ~ 47,
    modelo.gasolina == "Chery Tiggo 2" ~ 50,
    modelo.gasolina == "Toyota Raize" ~ 36,
    modelo.gasolina == "Suzuki Jimny" ~ 40,
    modelo.gasolina == "Toyota Yaris Cross" ~ 42,
    TRUE ~ NA_real_
  ))

View(df.dataframe.gasolina)


#Algunos gráficos interesantes por el momento 

#Autonomía con precio, tendrán relación?

df.dataframe.electricos %>% ggplot(
  aes(x=autonomia_km, y=Precio))+
    geom_smooth(se=FALSE)

cor(df.dataframe.electricos$Precio, df.dataframe.electricos$autonomia_km)
#La correlación es despreciable















#De esta base de datos de RECOPE se puede extraer los precios
#históricos de la gasolina súper 

datos.gasolina <- read_excel("data/PRECIOS-HISTORICOS-CONSUMIDOR-FINAL.xls")
View(PRECIOS_HISTORICOS_CONSUMIDOR_FINAL)

precio.super <- datos.gasolina$...5


#Tómense los datos de precios de gasolina super de setiembre del 2020
#a setiembre del 2025 (5 años), no nos interesa el precio hace 10 o 20 años
precio.super.recientes <- precio.super[352:435]

#Se nota que los datos numéricos están como tipo string, por lo que pasan

precio.super.recientes <- as.integer(precio.super.recientes)

typeof(precio.super.recientes)

#Acá revisando la data se nota como en el año 2022 el precio
#de la gasolina estuvo por los cielos, por lo que se quitarán esos outilers

remover.outliers <- function(x) {
  iqr <- IQR(x)
  q <- quantile(x, c(0.25, 0.75))
  x[x >= (q[1] - 1.5 * iqr) & x <= (q[2] + 1.5 * iqr)]
}

#Acá ya se le quitan los outliers al vector de los precios del super

precio.super.recientes <- remover.outliers(precio.super.recientes)

precio.super.recientes

mean(precio.super.recientes)




