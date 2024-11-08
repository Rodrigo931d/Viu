# instalamos los paquetes si hace falta
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("tseries")) install.packages("tseries")
if (!require("stats")) install.packages("stats")
if (!require("forecast")) install.packages("forecast")

library(dplyr)
library(ggplot2)
library(tidyr)
library(tseries)
library(stats)
library(forecast)


data <- read.csv("https://github.com/Rodrigo931d/Viu/raw/refs/heads/main/House_Rent.csv", header = TRUE)
names(data)
cat("Número de filas:", nrow(data), "\n");
str(data)

#Al verificar los tipos de datos, se optar por transformar la variable Posted.On de "character" a "Date"
data$Posted.On <- as.Date(data$Posted.On)


#2. Dividir el dataset de esta manera ya que la data es estática netamente académica
set.seed(129);
entrenamiento <- data[sample(nrow(data), 0.8 * nrow(data)), ]
prueba <- data[-sample(nrow(data), 0.8 * nrow(data)), ]
entrenamiento <- entrenamiento[order(entrenamiento$Posted.On), ]
prueba <- prueba[order(prueba$Posted.On), ]


#3. Verificación de datos nulos
contar_filas_nulas <- function(dataset) {
  nulos_filas <- sum(rowSums(is.na(dataset)) > 0) 
  print(paste("Cantidad de filas con datos nulos:", nulos_filas))
}

contar_filas_nulas(entrenamiento)
contar_filas_nulas(prueba)


#4 Verificación de duplicados

contar_duplicados <- function(dataset) {
  duplicados <- sum(duplicated(dataset)) 
  print(paste("Cantidad de filas duplicadas:", duplicados))
}

contar_duplicados(entrenamiento)
contar_duplicados(prueba)

#5. Calcular estadísticas descriptivas
estadisticas <- summary(entrenamiento)
print(estadisticas)


#Se elimina variables que no aportan al modelo
entrenamiento <- subset(entrenamiento, select = -c(Point.of.Contact, Tenant.Preferred, Area.Locality, Floor))
prueba <- subset(prueba, select = -c(Point.of.Contact, Tenant.Preferred, Area.Locality, Floor))


#6. Verificar outliers

generar_boxplot <- function(datos, variable) {
  datos %>%
    select(any_of(variable)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
    ggplot(aes(x = Variable, y = Valor, fill = Variable)) +
    geom_boxplot() +
    labs(title = paste("Boxplot de la Variable:", variable),
         x = "Variable", y = "Valor") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.8),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

generar_boxplot(entrenamiento, "BHK")
generar_boxplot(entrenamiento, "Rent")
generar_boxplot(entrenamiento, "Bathroom")
generar_boxplot(entrenamiento, "Size")


#7. Descomposicion de la serie temporal

st <- ts(entrenamiento$Rent, start = min(entrenamiento$Posted.On), end = max(entrenamiento$Posted.On), frequency = 365)
descomposicion <- stl(st, s.window = "periodic")
plot(descomposicion)

tendencia <- descomposicion$time.series[, "trend"] 
estacionalidad <- descomposicion$time.series[, "seasonal"]
residuo <- descomposicion$time.series[, "remainder"]



#8. Encodear variables

codificar_variables_categoricas <- function(entrenamiento, prueba, variables_nominales) {
  transformar <- function(data) {
    data %>%
      mutate(across(all_of(variables_nominales), as.factor)) %>%
      mutate(across(where(is.factor), ~as.integer(factor(.))))
  }
  
  list(entrenamiento_codificado = transformar(entrenamiento), 
       prueba_codificada = transformar(prueba))
}

# Aplicar la función
datos_codificados <- codificar_variables_categoricas(entrenamiento, prueba, c("Area.Type", "Furnishing.Status", "City"))
entrenamiento <- datos_codificados$entrenamiento_codificado
prueba <- datos_codificados$prueba_codificada



#9. prueba de normalidad

shapiro.test(entrenamiento$Rent)
shapiro.test(entrenamiento$Size)
shapiro.test(entrenamiento$BHK)
shapiro.test(entrenamiento$Bathroom)
shapiro.test(entrenamiento$Area.Type)
shapiro.test(entrenamiento$City)
shapiro.test(entrenamiento$Furnishing.Status)



#9. Análisis de autocorrelación 

acf(st, lag.max = 50) 

resultado_acf <- acf(st, plot = FALSE)
coeficientes_autocorrelacion <- resultado_acf$acf
resultado_pacf <- pacf(st, lag.max = 50, plot = TRUE)

print(coeficientes_autocorrelacion) #Verificar acf
print(resultado_pacf$acf)          #Verificar pacf


# Prueba de Ljung-Box 
lag_max <- 50
residuos_componente_error <- residuo
resultado_prueba <- Box.test(residuos_componente_error, lag = lag_max, type = "Ljung-Box")
resultado_prueba

#Prueba de Dickey-Fuller aumentada (ADF) para ver si es estacionaria
adf_result <- adf.test(st)
print(adf_result)



#10. Creación del modelo adecuado

modelo_arima <- auto.arima(st)

predicciones_prueba <- forecast(modelo_arima, newdata = prueba)
summary(predicciones_prueba)


