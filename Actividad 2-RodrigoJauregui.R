library(dplyr)
library(ggplot2)
library(tidyr)
library(tseries)
library(stats)
library(forecast)
library(corrplot)
install.packages("astsa")
library(astsa)

#1. Importar el dataset y ver cantidad de datos
dataset2 <- read.csv("https://raw.githubusercontent.com/Rodrigo931d/Viu/main/AirQuality.csv", header = TRUE, sep=";")
dataset2 <- dataset2 %>% select(-X, -X.1)
cat("La cantidad de columnas es", ncol(dataset2), "y sus nombres son:", paste(names(dataset2), collapse = ", "), "\n");
cat("Número de filas:", nrow(dataset2), "\n");

#se omite Time
dataset2 <- dataset2 %>% select(-Time)

#Se soluciona problemas en tipos de datos incorrectos
dataset2$Date <- as.Date(dataset2$Date, format = "%d/%m/%Y")
dataset2$CO.GT. <- as.numeric(gsub(",", ".", dataset2$CO.GT.))
dataset2$C6H6.GT. <- as.numeric(gsub(",", ".", dataset2$C6H6.GT.))
dataset2$T <- as.numeric(gsub(",", ".", dataset2$T))
dataset2$RH <- as.numeric(gsub(",", ".", dataset2$RH))
dataset2$AH <- as.numeric(gsub(",", ".", dataset2$AH))

str(dataset2)


# Fecha mínima y fecha máxima
min_date <- min(dataset2$Date, na.rm = TRUE)
max_date <- max(dataset2$Date, na.rm = TRUE)
min_date
max_date


#2. Se divide el dataset en datos de entrenamiento y prueba, así evitar tocar datos no vistos (datos prueba)
set.seed(123);
indices_entrenamiento <- sample(nrow(dataset2), 0.8 * nrow(dataset2))
dato_entrenamiento <- dataset2[indices_entrenamiento, ]
dato_prueba <- dataset2[-indices_entrenamiento, ]

#ordenar por fecha
dato_entrenamiento <- dato_entrenamiento[order(dato_entrenamiento$Date), ]
dato_prueba <- dato_prueba[order(dato_prueba$Date), ]


  
#3 Tratamiento de datos nulos

verificar_nulos <- function(df) {
  nombre_df <- deparse(substitute(df))
  cat("La cantidad total de datos nulos en el dataframe", nombre_df, "es:", sum(is.na(df)), "\n")
  print(sort(colSums(is.na(df))[colSums(is.na(df)) > 0]))}
  
verificar_nulos(dato_entrenamiento)
verificar_nulos(dato_prueba)

dato_prueba <- na.omit(dato_prueba)
dato_entrenamiento <- na.omit(dato_entrenamiento)


#4. Tratamiento de duplicados
cat("La cantidad de duplicados es:", nrow(dato_entrenamiento[duplicated(dato_entrenamiento), ]), "\n")
cat("La cantidad de duplicados es:", nrow(dato_prueba[duplicated(dato_prueba), ]), "\n")

dato_entrenamiento <- unique(dato_entrenamiento)
dato_prueba <- unique(dato_prueba)

###################################Se comienza el análisis de train##################
inconsistentes <- function(df) {
  df %>%
    filter(
      CO.GT. >= 0 &
        C6H6.GT. >= 0 &
        T >= 0 &
        RH >= 0 &
        AH >= 0 &
        NMHC.GT. >= 0 &
        NO2.GT. >= 0 &
        NOx.GT. >= 0
    )
}

dato_entrenamiento <- inconsistentes(dato_entrenamiento)
dato_prueba <- inconsistentes(dato_prueba)

#5. Calcula estadísticas descriptivas
descriptive_stats <- summary(dato_entrenamiento)
print(descriptive_stats)


#6. boxplot para identificar outliers
generar_boxplot <- function(data, variable) {
  data %>%
    select(any_of(variable)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
    ggplot(aes(x = Variable, y = Valor, fill = Variable)) +
    geom_boxplot() +
    labs(title = paste("Boxplot de", variable),
         x = "Variable", y = "Valor") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.8),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

generar_boxplot(dato_entrenamiento, "NO2.GT.")
generar_boxplot(dato_entrenamiento, "CO.GT.")
generar_boxplot(dato_entrenamiento, "AH")
generar_boxplot(dato_entrenamiento, "RH")
generar_boxplot(dato_entrenamiento, "T")
generar_boxplot(dato_entrenamiento, "PT08.S1.CO.")
generar_boxplot(dato_entrenamiento, "NMHC.GT.")
generar_boxplot(dato_entrenamiento, "C6H6.GT.")
generar_boxplot(dato_entrenamiento, "PT08.S2.NMHC.")
generar_boxplot(dato_entrenamiento, "NOx.GT.")
generar_boxplot(dato_entrenamiento, "PT08.S3.NOx.")
generar_boxplot(dato_entrenamiento, "PT08.S4.NO2.")
generar_boxplot(dato_entrenamiento, "PT08.S5.O3.")


#8. Verificar la normalidad mediante prueba de Kolmogorov-Smirnov complementado con Regla de Scott para definir bindwidth
binwidth_scott <- function(x) {
  3.5 * sd(x) / length(x)^(1/3)
}

#Definir la función para crear histogramas
crear_histograma <- function(data, variable) {
  ggplot(data, aes(x = !!sym(variable))) +
    geom_histogram(binwidth = binwidth_scott(data[[variable]]),
                   fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histograma de", variable),
         x = "Valor", y = "Frecuencia") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

crear_histograma(dato_entrenamiento, "NO2.GT.")
crear_histograma(dato_entrenamiento, "CO.GT.")
crear_histograma(dato_entrenamiento, "AH")
crear_histograma(dato_entrenamiento, "RH")
crear_histograma(dato_entrenamiento, "T")
crear_histograma(dato_entrenamiento, "PT08.S1.CO.")
crear_histograma(dato_entrenamiento, "NMHC.GT.")
crear_histograma(dato_entrenamiento, "C6H6.GT.")
crear_histograma(dato_entrenamiento, "PT08.S2.NMHC.")
crear_histograma(dato_entrenamiento, "NOx.GT.")
crear_histograma(dato_entrenamiento, "PT08.S3.NOx.")
crear_histograma(dato_entrenamiento, "PT08.S4.NO2.")
crear_histograma(dato_entrenamiento, "PT08.S5.O3.")

#Definir la función para la prueba de normalidad

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

ks_test <- function(data, variable) {
  standardized_data <- standardize(data[[variable]])
  ks.test(standardized_data, "pnorm")
}

ks_test(dato_entrenamiento, "NO2.GT.")
ks_test(dato_entrenamiento, "CO.GT.")
ks_test(dato_entrenamiento, "AH")
ks_test(dato_entrenamiento, "RH")
ks_test(dato_entrenamiento, "T")
ks_test(dato_entrenamiento, "PT08.S1.CO.")
ks_test(dato_entrenamiento, "NMHC.GT.")
ks_test(dato_entrenamiento, "C6H6.GT.")
ks_test(dato_entrenamiento, "PT08.S2.NMHC.")
ks_test(dato_entrenamiento, "NOx.GT.")
ks_test(dato_entrenamiento, "PT08.S3.NOx.")
ks_test(dato_entrenamiento, "PT08.S4.NO2.")
ks_test(dato_entrenamiento, "PT08.S5.O3.")

#9. Se analiza la matriz de correlación

numeric_data <- dato_entrenamiento[sapply(dato_entrenamiento, is.numeric)]
matriz_correlacion <- cor(numeric_data)

# Configurar los márgenes de la gráfica
par(mar = c(1, 1, 1, 1))

corrplot(matriz_correlacion, 
         method = "color", 
         type = "full", 
         tl.col = "black", 
         tl.srt = 45, 
         addCoef.col = "black")

#se omite PT08.S2.NMHC y PT08.S3.NOx 
dato_entrenamiento <- dato_entrenamiento %>% select(-PT08.S5.O3., -PT08.S2.NMHC.)
dato_prueba <- dato_prueba %>% select(-PT08.S5.O3., -PT08.S2.NMHC.)


#7. Descomposicion de la serie temporal

ts_data <- ts(dato_entrenamiento$NO2.GT., start = min(dato_entrenamiento$Date), end = max(dato_entrenamiento$Date), frequency = 365)
decomposition <- stl(ts_data, s.window = "periodic")

trend_component <- decomposition$time.series[, "trend"] 
seasonal_component <- decomposition$time.series[, "seasonal"]
residual_component <- decomposition$time.series[, "remainder"]
plot(decomposition)

print("Componente de Tendencia:")
print(trend_component)

print("Componente Estacional:")
print(seasonal_component)

print("Componente de Residuos:")
print(residual_component)


#9. Análisis de autocorrelación 

#Coeficientes de Autocorrelación (ACF) 
resultado_acf <- acf(ts_data, plot = TRUE)
print(resultado_acf$acf)
acf(ts_data, lag.max = 50, main = "Función de Autocorrelación (ACF)")

# Análisis de autocorrelación parcial (PACF)
resultado_pacf <- pacf(ts_data, lag.max = 50, plot = FALSE)
print(resultado_pacf$acf)
pacf(ts_data, lag.max = 50, main = "Función de Autocorrelación Parcial (PACF)")


# Prueba de Ljung-Box 
lag_max <- 50
residuos_componente_error <- residual_component
resultado_prueba <- Box.test(residuos_componente_error, lag = lag_max, type = "Ljung-Box")
resultado_prueba

#corroboramos con Prueba de Dickey-Fuller aumentada (ADF) para ver si es estacionaria
adf_result <- adf.test(ts_data)
print(adf_result)


#10. Creación del modelo 

# Ajustar el modelo SARIMA usando la función sarima
modelo_sarima_astsa <- sarima(ts_data, 
                              p = 2, d = 1, q = 2,        # Parámetros no estacionales
                              P = 1, D = 1, Q = 0,        # Parámetros estacionales
                              S = 12)                     # Periodo estacional 

print(modelo_sarima_astsa)

forecast_result_astsa <- forecast(modelo_sarima_astsa$fit, h = 30)

# Graficar las previsiones
plot(forecast_result_astsa)














