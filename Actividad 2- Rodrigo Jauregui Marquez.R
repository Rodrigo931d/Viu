# instalamos los paquetes si hace falta
if (!require("sf")) install.packages("sf")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("giscoR")) install.packages("giscoR")
if (!require("eurostat")) install.packages("eurostat")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("elevatr")) install.packages("elevatr")
if (!require("terra")) install.packages("terra")
if (!require("ggnewscale")) install.packages("ggnewscale")
if (!require("tidyterra")) install.packages("tidyterra")
if (!require("dplyr")) install.packages("dplyr")







library(dplyr)
library(ggplot2)
library(tidyr)
library(tseries)
library(stats)
library(forecast)

#1. Importar el dataset y ver cantidad de datos
dataset2 <- read.csv("https://raw.githubusercontent.com/SergioGerman412/Datasets/main/House_Rent_Dataset.csv", header = TRUE)

cat("La cantidad de columnas es", ncol(dataset2), "y sus nombres son:", paste(names(dataset2), collapse = ", "), "\n");
cat("Número de filas:", nrow(dataset2), "\n");

#se transforma la variable Posted.On de "character" a "Date"
dataset2$Posted.On <- as.Date(dataset2$Posted.On)


#2. Se divide el dataset y se ordena para el análisis y evitar tocar datos no vistos (datos prueba)
set.seed(123);
dato_entrenamiento1 <- dataset2[sample(nrow(dataset2), 0.8 * nrow(dataset2)), ]
dato_prueba1 <- dataset2[-seq_len(nrow(dato_entrenamiento1)), ]

dato_entrenamiento1 <- dato_entrenamiento1[order(dato_entrenamiento1$Posted.On), ]
dato_prueba1 <- dato_prueba1[order(dato_prueba1$Posted.On), ]


#3 Verifica si hay datos nulos en el dataset original (solo verificar)
cantidad_nulos <- sum(is.na(dataset2))
cat("La cantidad de datos nulos es:", cantidad_nulos, "\n")


#4. Verifica duplicados en el dataset original (solo verificar)
duplicados <- dataset2[duplicated(dataset2), ]
cat("La cantidad de duplicados es:", nrow(duplicados), "\n")

###################################Se comienza el análisis de train##################

#5. Calcula estadísticas descriptivas
descriptive_stats1 <- summary(dato_entrenamiento1)
print(descriptive_stats1)

#6. boxplot para identificar outliers
set1 <- c("BHK", "Bathroom")  
dato_entrenamiento1 %>%
  select(any_of(set1)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = Variable, y = Valor, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Boxplot de Variables Numéricas",
       x = "Variable", y = "Valor") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.8),
        axis.text.x = element_text(angle = 45, hjust = 1))

set2 <- c("Rent", "Size")  
dato_entrenamiento1 %>%
  select(any_of(set2)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = Variable, y = Valor, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Boxplot de Variables Numéricas",
       x = "Variable", y = "Valor") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.8),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Se elimina 2 variables que se considera no aportan al objetivo.
#Adicional como Area Locality tiene demasiado datos únicos y distintos, se opta por no usarlo por la poca cantidad de datos
#La otra opción era agruparlos, pero preferí eliminarlo como primera instancia
unique(dato_entrenamiento1$Area.Locality)

dato_entrenamiento1 <- subset(dato_entrenamiento1, select = -c(Point.of.Contact, Tenant.Preferred, Area.Locality, Floor))
dato_prueba1 <- subset(dato_prueba1, select = -c(Point.of.Contact, Tenant.Preferred, Area.Locality, Floor))


#7. Descomposicion de la serie temporal

ts_data <- ts(dato_entrenamiento1$Rent, start = min(dato_entrenamiento1$Posted.On), end = max(dato_entrenamiento1$Posted.On), frequency = 365)
decomposition <- stl(ts_data, s.window = "periodic")
plot(decomposition)

trend_component <- decomposition$time.series[, "trend"] #hay regular variabilidad por la variable Rent
seasonal_component <- decomposition$time.series[, "seasonal"]
residual_component <- decomposition$time.series[, "remainder"]

#8. función para aplicar encoding para pasar a cuantitativa discreta

aplicar_dummy_train_test <- function(dato_entrenamiento1, dato_prueba1, variables_nominales) {
  train_data_codificado <- dato_entrenamiento1 %>%
    mutate(across(all_of(variables_nominales), as.factor)) %>%
    mutate(across(where(is.factor), ~as.integer(factor(.))))
  
  test_data_codificado <- dato_prueba1 %>%
    mutate(across(all_of(variables_nominales), as.factor)) %>%
    mutate(across(where(is.factor), ~as.integer(factor(.))))
  
  return(list(dato_entrenamiento1 = train_data_codificado, dato_prueba1 = test_data_codificado))
}

datos_codificados <- aplicar_dummy_train_test(dato_entrenamiento1, dato_prueba1, c("Area.Type", "Furnishing.Status", "City"))
dato_entrenamiento1 <- datos_codificados$dato_entrenamiento1
dato_prueba1 <- datos_codificados$dato_prueba1

#9. Análisis de autocorrelación 

acf(ts_data, lag.max = 50) 

resultado_acf <- acf(ts_data, plot = FALSE)
coeficientes_autocorrelacion <- resultado_acf$acf

resultado_pacf <- pacf(ts_data, lag.max = 50, plot = TRUE)

# Prueba de Ljung-Box 
lag_max <- 50
residuos_componente_error <- residual_component
resultado_prueba <- Box.test(residuos_componente_error, lag = lag_max, type = "Ljung-Box")

#corroboramos con Prueba de Dickey-Fuller aumentada (ADF) para ver si es estacionaria
adf_result <- adf.test(ts_data)
print(adf_result)


#10. Creación del modelo adecuado

modelo_arima <- auto.arima(ts_data)

predicciones_prueba <- forecast(modelo_arima, newdata = dato_prueba1)
summary(predicciones_prueba)