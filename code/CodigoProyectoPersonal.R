
#El presente script es donde se desarrolla el código del proyecto
#personal en su totalidad

#Primero se crean los dataframes con los autos eléctricos más vendidos
#en Costa Rica

df.dataframe.electricos <- data.frame(
  
  "Modelo" = modelo <-  c("GEELY GEOMETRY E", " BYD YUAN S1 PRO",
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

#De esta base de datos de RECOPE se puede extraer los precios
#históricos de la gasolina súper 

datos.gasolina <- read_excel("data/PRECIOS-HISTORICOS-CONSUMIDOR-FINAL.xls")
View(PRECIOS_HISTORICOS_CONSUMIDOR_FINAL)

precio.super <- datos.gasolina$...5

precio.super



