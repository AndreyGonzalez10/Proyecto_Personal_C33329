
#El presente script es donde se desarrolla el código del proyecto
#personal en su totalidad

#Primero se crean los dataframes con los autos eléctricos más vendidos
#en Costa Rica
library(dplyr)

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
  resultado = costo.fijo.ICE + vector*costo.kwh
  return (resultado)
}

df.dataframe.electricos <- df.dataframe.electricos %>% mutate(
  pago.carga = costo.carga(capacidad.bateria)
) 

View(df.dataframe.electricos)


#Se corrige el error que se pusieron dos columnas con capacidad 
#de la batería

df.dataframe.electricos <- df.dataframe.electricos %>% select(-capacidad.bateria)

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

View(df.dataframe.electricos)

#Ahora se incluye la capacidad en litros de gasolina de los carros
#a gasolina 

df.dataframe.gasolina <- df.dataframe.gasolina %>%
  mutate(
    capacidad.litros = case_when(
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

#Cuantas veces es necesario cargar el carro, suponiendo un uso promedio de 1200 km mensuales

df.dataframe.electricos <- df.dataframe.electricos %>% mutate(
  cargas.por.mes = 1200/autonomia.km
)

View(df.dataframe.electricos)


#Costo por cargas mensuales

df.dataframe.electricos <- df.dataframe.electricos %>% 
  mutate(
    carga.mensual = pago.carga*cargas.por.mes
  )

View(df.dataframe.electricos)

#Se ve la autonomía (kilometraje en este caso) por cada tanque lleno

df.dataframe.gasolina <- df.dataframe.gasolina %>% 
  mutate(
  kilometraje = capacidad.litros*8  
)

#Se calcula cuantas veces habría que llenar el tanque 

df.dataframe.gasolina <- df.dataframe.gasolina %>% mutate(
  rellenos = 1200/kilometraje
)

View(df.dataframe.gasolina)

#Se importan los datos necesarios para el tipo de cambio y los precios de la gasolina

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(readr)

datos.dolar.1 <- read_csv("data/Datos históricos USD_CRC (2).csv")
View(Datos_históricos_USD_CRC_2_)

#Se obtiene con coma y todo ya bien separado dividiendo entre 100

datos.dolar <- datos.dolar.1[,2]/100 #Esto para corregir 

#Se pasa el vector a un dataframe

datos.dolar <- as.data.frame(datos.dolar)
View(datos.dolar)

#Ahora si se procederá a hacer el modelo ARIMA para poder proyectar
#el tipo de cambio en el plazo de los 96 meses del crédito y los 
#precios de la gasolina. Información tomada de RPUBS. Link: https://rpubs.com/stefens07/Arima


#Se importan las librerías necesarias y se instalan los paquetes 

library(forecast) #contiene el modelo ARIMA
library(tseries) #contiene contenido de series de tiempo
library(TSA) #para series de tiempo
library(urca) #para el test para comprobar estacionariedad
library (ggplot2) #para graficar
library(stats) #para pruebas de estadística
library(seasonal) #para la serie ajustada de estacionalidad


#Se hace datos.dolar como una serie de tiempo

serie.dolar <- ts(datos.dolar$Último, start=c(2015,3), frequency = 12)


#Se grafica el tipo de cambio por 10 años

autoplot(serie.dolar, frequency=12, xlab="Años", ylab="USD/CRC",main="Figura 1. Tipo de Cambio")

#Ahora vamos a "descomponer" la información del tipo de cambio

dolar.decom <- decompose(serie.dolar)

par(mfrow=c(2,2))

plot(dolar.decom$x, main="Tipo de cambio original", col = "black", ylab="Serie de tiempo")
plot(dolar.decom$trend, main="Tendencia", col="blue",ylab="Valores")
plot(dolar.decom$seasonal, main="Estacionalidad", col="red",ylab="Valores")
plot(dolar.decom$random, main = "Irregularidad", col="green", ylab="Valores")


#Aplicación del modelo ARIMA

#Para usar ARIMA, la serie de tiempo debe ser estacionaria, por
#lo que se le aplica la prueba de Dickey-Fuller

adf.test(serie.dolar) #con esto se ve que la serie NO es estacionaria

# Se diferencia la serie para poder hacerla estacionaria

dolar.d1 <- diff(serie.dolar,differences = 1)
dolar.d1 <- ts(dolar.d1, frequency=12)
adf.test(dolar.d1)

#En este caso bastó con únicamente haberla diferenciado una vez
#y ya tira que es estacionaria

#Ahora falta tomar los valores p y q del ARIMA, los cuales se obtienen
#con la función de autocorrelación(ACF) y autocorrelación parcial (PACF)

par(mfrow= c(1,1))

acf(dolar.d1,lag.max = 120, main="Figura 4. Función de autocorrelación TDC diferenciado")
pacf(dolar.d1,lag.max = 120, main="Figura 5. Función de autocorrelación parcial TDC diferenciado")



#Después de 1000 pruebas se escoge el modelo ARIMA (0,1,3) ya que el residuo es ruido blanco
    
auto.arima(serie.dolar)

Modelo.arima <-  Arima(serie.dolar, order = c(1,1,2), include.drift = TRUE)

Box.test(Modelo.arima$residuals, lag=20, type="Ljung-Box")

shapiro.test(Modelo.arima$residuals)


#Pronóstico 

#El “drift” (deriva) agrega una tendencia promedio al pronóstico, haciendo que crezca o disminuya suavemente en lugar de ser plano.

pronostico96 <- forecast(Modelo.arima, level = c(90), h=96)
plot(pronostico96)

pronostico96


#Ahora se hace exactamente lo mismo pero ahora necesitamos predecir la gasolina

library(lubridate)
library(readxl)
library(dplyr)
library(stringr)

df.precios.gasolina <- read_excel("data/PreciosImportantes.xlsx")

df.precios.gasolina <- df.precios.gasolina %>%
  mutate(Fecha.de.cambio = 
           dmy(str_replace_all(df.precios.gasolina$Fecha, 
c("DE " = "", "DEL " = "", "ENERO" = "01", "FEBRERO" = "02", "MARZO" = "03",
"ABRIL" = "04", "MAYO" = "05", "JUNIO" = "06", "JULIO" = "07", "AGOSTO" = "08",
"SETIEMBRE" = "09", "SEPTIEMBRE" = "09", "OCTUBRE" = "10", "NOVIEMBRE" = "11",
"DICIEMBRE" = "12"))))

View(df.precios.gasolina)

df.precios.gasolina <- df.precios.gasolina %>% select(
  Fecha.de.cambio, Final
)

View(df.precios.gasolina)

#Verificar que sea tipo fecha

df.precios.gasolina <- df.precios.gasolina %>%
  mutate(Fecha.de.cambio = as.Date(Fecha.de.cambio))

df.precios.gasolina <- df.precios.gasolina %>%
  mutate(Mes = floor_date(Fecha.de.cambio, unit = "month"))

# Calcular diferencia con el primer día del mes
df.precios.gasolina <- df.precios.gasolina %>%
  mutate(dias.desde.inicio = abs(day(Fecha.de.cambio) - 1))

# Para cada mes, seleccionar la fila con menor diferencia
resultado <- df.precios.gasolina %>%
  group_by(Mes) %>%
  slice_min(order_by = dias.desde.inicio, n = 1) %>%
  ungroup() %>%
  select(Fecha.de.cambio, Final)

# Mostrar resultado

precios.gasolina.mensual <- as.data.frame(resultado)

View(precios.gasolina.mensual)

# Ahora si se procede a hacer lo mismo del ARIMA pero ahora con la gasolina para tomar un promedio de las proyecciones

serie.gasolina <- ts(precios.gasolina.mensual$Final, start(2015,3),frequency = 12)

autoplot(serie.gasolina, frequency=12, xlab="Años",ylab="Precio", main = "Figura 7. Precio Gasolina Super")

gasolina.descompuesta <- decompose(serie.gasolina)

par(mfrow=c(2,2))

plot(gasolina.descompuesta$x, main="Precios originales",col="black",ylab="Serie de tiempo")
plot(gasolina.descompuesta$trend, main="Tendencia",col="blue",ylab="Valores")
plot(gasolina.descompuesta$seasonal, main="Estacionalidad",col="red",ylab="Valores")
plot(gasolina.descompuesta$random, main="Irregularidad",col="green",ylab="Valores")

#Prueba de Dickey-Fuller

adf.test(serie.gasolina) #no es estacionaria

seried1.gasolina <- diff(serie.gasolina, differences = 1)

adf.test(seried1.gasolina)#Ahora si

#Lo mismo para determinar p y q

par(mfrow=c(1,1))

acf(seried1.gasolina, main="Figura 8. Función de autocorrelación -Gasolina diferenciada")
pacf(seried1.gasolina, main="Figura 9. Función de autocorrelación parcial -Gasolina diferenciada")

#Veamos cual recomienda el auto.arima

auto.arima(serie.gasolina)

Arima.gasolina <- Arima(serie.gasolina, order = c(0,1,0), include.drift = TRUE)

Box.test(Arima.gasolina$residuals, lag=20, type="Ljung-Box")

Pronostico.gasolina <- forecast(Arima.gasolina, level=c(95), h=96)
plot(Pronostico.gasolina)

Pronostico.gasolina

#Se hace un df para ver mejor los datos

df.pronostico.gasolina <- as.data.frame(Pronostico.gasolina)
View(df.pronostico.gasolina)

#Se obtiene el promedio

promedio.futuro.gasolina <- mean(df.pronostico.gasolina$`Point Forecast`)
promedio.futuro.gasolina

#Se obtiene el gasto por gasolina

df.dataframe.gasolina <- df.dataframe.gasolina %>% mutate(
  pago.por.gasolina = promedio.futuro.gasolina*capacidad.litros*rellenos
)

View(df.dataframe.gasolina)


#Ahora que sabemos el tipo de cambio, se puede proyectar cuanto pagará la persona en su crédito en colones

pronostico.tc.df <- as.data.frame(pronostico96)

#Obtenemos el promedio del tipo de cambio pronosticado y lo que se pagará en colones

promedio.tdc <- mean(pronostico.tc.df$`Point Forecast`)


df.dataframe.electricos <- df.dataframe.electricos %>% mutate(
  pago.credito = Pago.mensual....*promedio.tdc
)

df.dataframe.gasolina <- df.dataframe.gasolina %>% mutate(
  pago.credito = Pago.mensual....*promedio.tdc
)


#Ahora se saca la última columna que será el gasto total mensual

df.dataframe.electricos <- df.dataframe.electricos %>% mutate(
  gasto.total.mensual = pago.credito+carga.mensual
)

df.dataframe.gasolina <- df.dataframe.gasolina %>% mutate(
  gasto.total.mensual = pago.credito+pago.por.gasolina
)




#Por último el salario recomendado según expertos (el objetivo de la investigación)
#https://www.investopedia.com/how-much-should-i-spend-on-a-car-5187853

df.dataframe.electricos <- df.dataframe.electricos %>% 
  mutate(
    ingresos.necesarios = gasto.total.mensual/0.2
  )

df.dataframe.gasolina <- df.dataframe.gasolina %>% 
  mutate(
    ingresos.necesarios = gasto.total.mensual/0.2
  )


View(df.dataframe.electricos)
View(df.dataframe.gasolina)


