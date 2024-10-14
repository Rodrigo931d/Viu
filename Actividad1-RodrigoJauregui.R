#Si no se cuenta con alguna libreria toca instalarla con el siguiente comando como guía
#install.packages("ggplot2")

library(dplyr)
library(ggplot2)
library(tidyr)
library(stats)
library(corrplot)
library(nnet)
library(caret)

#1. Cargar el dataset y ver cantidad de datos
dataset <- read.csv("https://raw.githubusercontent.com/Rodrigo931d/Viu/main/AirQuality.csv", header = TRUE, sep=";")
dataset <- dataset %>% select(-X, -X.1)
cat("La cantidad de columnas es", ncol(dataset), "y sus nombres son:", paste(names(dataset), collapse = ", "), "\n");
cat("Número de filas:", nrow(dataset), "\n");

#se omite Time y Date
dataset <- dataset %>% select(-Time, -Date)

#Se soluciona problemas en tipos de datos incorrectos
dataset$CO.GT. <- as.numeric(gsub(",", ".", dataset$CO.GT.))
dataset$C6H6.GT. <- as.numeric(gsub(",", ".", dataset$C6H6.GT.))
dataset$T <- as.numeric(gsub(",", ".", dataset$T))
dataset$RH <- as.numeric(gsub(",", ".", dataset$RH))
dataset$AH <- as.numeric(gsub(",", ".", dataset$AH))

str(dataset)

#2. Se divide el dataset en datos para entrenamiento y prueba, y evitar tocar datos no vistos (datos prueba)
set.seed(123);
indices_entrenamiento <- sample(nrow(dataset), 0.8 * nrow(dataset))
dataset_entrenamiento <- dataset[indices_entrenamiento, ]
dataset_prueba <- dataset[-indices_entrenamiento, ]

#3 Tratamiento de datos nulos
verificar_nulos <- function(df) {
  nombre_df <- deparse(substitute(df))
  nulos_por_fila <- rowSums(is.na(df))
  total_filas_nulas <- sum(nulos_por_fila > 0)
  cat("La cantidad total de filas con datos nulos en el dataframe", nombre_df, "es:", total_filas_nulas, "\n")
}


verificar_nulos(dataset_entrenamiento)
verificar_nulos(dataset_prueba)

dataset_entrenamiento <- na.omit(dataset_entrenamiento)
dataset_prueba <- na.omit(dataset_prueba)


#4. Tratamiento de duplicados
cat("La cantidad de duplicados es:", nrow(dataset_entrenamiento[duplicated(dataset_entrenamiento), ]), "\n")
cat("La cantidad de duplicados es:", nrow(dataset_prueba[duplicated(dataset_prueba), ]), "\n")

dataset_entrenamiento <- unique(dataset_entrenamiento)
dataset_prueba <- unique(dataset_prueba)


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

dataset_entrenamiento <- inconsistentes(dataset_entrenamiento)
dataset_prueba <- inconsistentes(dataset_prueba)



#5. Calcular estadísticas descriptivas
descriptive_stats <- summary(dataset_entrenamiento)
print(descriptive_stats)


#6. Realizar diagrama de caja para identificar outliers
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

generar_boxplot(dataset_entrenamiento, "NO2.GT.")
generar_boxplot(dataset_entrenamiento, "CO.GT.")
generar_boxplot(dataset_entrenamiento, "AH")
generar_boxplot(dataset_entrenamiento, "RH")
generar_boxplot(dataset_entrenamiento, "T")
generar_boxplot(dataset_entrenamiento, "PT08.S1.CO.")
generar_boxplot(dataset_entrenamiento, "NMHC.GT.")
generar_boxplot(dataset_entrenamiento, "C6H6.GT.")
generar_boxplot(dataset_entrenamiento, "PT08.S2.NMHC.")
generar_boxplot(dataset_entrenamiento, "NOx.GT.")
generar_boxplot(dataset_entrenamiento, "PT08.S3.NOx.")
generar_boxplot(dataset_entrenamiento, "PT08.S4.NO2.")
generar_boxplot(dataset_entrenamiento, "PT08.S5.O3.")

# Se complementa con el método del IQR
identify_outliers <- function(data) {
  outlier_indices <- list()  # Lista para almacenar los índices de outliers
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {  # Solo procesar columnas numéricas
      # Calcular Q1, Q3 e IQR
      Q1 <- quantile(data[[col]], 0.25)
      Q3 <- quantile(data[[col]], 0.75)
      IQR <- Q3 - Q1
      # Definir límites inferior y superior
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      # Identificar índices de outliers
      outliers <- which(data[[col]] < lower_bound | data[[col]] > upper_bound)
      outlier_indices[[col]] <- outliers  # Almacenar índices por columna
    }
  }
  return(outlier_indices)
}

outliers <- identify_outliers(dataset_entrenamiento)
print(outliers)


# Función para eliminar filas con outliers
eliminar_filas_outliers <- function(data, outliers) {
  filas_outliers <- unique(unlist(outliers))  # Unir todos los índices de las filas con outliers
  return(data[-filas_outliers, ])  
}

dataset_entrenamiento <- eliminar_filas_outliers(dataset_entrenamiento, outliers)
dataset_prueba <- eliminar_filas_outliers(dataset_prueba, outliers)




#7. Verificar la normalidad mediante prueba de Kolmogorov-Smirnov complementado con Regla de Scott para definir bindwidth

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

crear_histograma(dataset_entrenamiento, "NO2.GT.")
crear_histograma(dataset_entrenamiento, "CO.GT.")
crear_histograma(dataset_entrenamiento, "AH")
crear_histograma(dataset_entrenamiento, "RH")
crear_histograma(dataset_entrenamiento, "T")
crear_histograma(dataset_entrenamiento, "PT08.S1.CO.")
crear_histograma(dataset_entrenamiento, "NMHC.GT.")
crear_histograma(dataset_entrenamiento, "C6H6.GT.")
crear_histograma(dataset_entrenamiento, "PT08.S2.NMHC.")
crear_histograma(dataset_entrenamiento, "NOx.GT.")
crear_histograma(dataset_entrenamiento, "PT08.S3.NOx.")
crear_histograma(dataset_entrenamiento, "PT08.S4.NO2.")
crear_histograma(dataset_entrenamiento, "PT08.S5.O3.")

#Definir la función para la prueba de normalidad

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

ks_test <- function(data, variable) {
  standardized_data <- standardize(data[[variable]])
  ks.test(standardized_data, "pnorm")
}

ks_test(dataset_entrenamiento, "NO2.GT.")
ks_test(dataset_entrenamiento, "CO.GT.")
ks_test(dataset_entrenamiento, "AH")
ks_test(dataset_entrenamiento, "RH")
ks_test(dataset_entrenamiento, "T")
ks_test(dataset_entrenamiento, "PT08.S1.CO.")
ks_test(dataset_entrenamiento, "NMHC.GT.")
ks_test(dataset_entrenamiento, "C6H6.GT.")
ks_test(dataset_entrenamiento, "PT08.S2.NMHC.")
ks_test(dataset_entrenamiento, "NOx.GT.")
ks_test(dataset_entrenamiento, "PT08.S3.NOx.")
ks_test(dataset_entrenamiento, "PT08.S4.NO2.")
ks_test(dataset_entrenamiento, "PT08.S5.O3.")


#9. Diagrama de dispersión

# Crear función para diagrama de dispersión con línea de ajuste
crear_diagrama_dispersión <- function(dataset, x_variable, y_variable) {
  ggplot(dataset, aes_string(x = x_variable, y = y_variable)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +  # Línea de regresión
    labs(title = paste("Diagrama de Dispersión:", x_variable, "vs", y_variable),
         x = x_variable, y = y_variable) +
    theme_minimal()
}

# Crear diagramas de dispersión con línea de ajuste
crear_diagrama_dispersión(dataset_entrenamiento, "CO.GT.", "NO2.GT.")
crear_diagrama_dispersión(dataset_entrenamiento, "AH", "NO2.GT.")
crear_diagrama_dispersión(dataset_entrenamiento, "RH", "NO2.GT.")
crear_diagrama_dispersión(dataset_entrenamiento, "T", "NO2.GT.")
crear_diagrama_dispersión(dataset_entrenamiento, "PT08.S1.CO.", "NO2.GT.")
crear_diagrama_dispersión(dataset_entrenamiento, "NMHC.GT.", "NO2.GT.")
crear_diagrama_dispersión(dataset_entrenamiento, "C6H6.GT.", "NO2.GT.")
crear_diagrama_dispersión(dataset_entrenamiento, "PT08.S2.NMHC.", "NO2.GT.")
crear_diagrama_dispersión(dataset_entrenamiento, "NOx.GT.", "NO2.GT.")
crear_diagrama_dispersión(dataset_entrenamiento, "PT08.S3.NOx.", "NO2.GT.")
crear_diagrama_dispersión(dataset_entrenamiento, "PT08.S4.NO2.", "NO2.GT.")
crear_diagrama_dispersión(dataset_entrenamiento, "PT08.S5.O3.", "NO2.GT.")


#10. Matriz de correlación

matriz_correlacion <- cor(dataset_entrenamiento)

# Configurar los márgenes de la gráfica
par(mar = c(1, 1, 1, 1))

# Visualizar la matriz de correlación
corrplot(matriz_correlacion, 
         method = "color", 
         type = "full", 
         tl.col = "black", 
         tl.srt = 45, 
         addCoef.col = "black")


###################################MODELO DE REGRESION LINEAL##################

#10.5. Algoritmo de Regresión lineal simple

modelo_lineal <- lm(NO2.GT. ~ C6H6.GT., data = dataset_entrenamiento)
predicciones <- predict(modelo_lineal, newdata = dataset_prueba)

# Mostrar un resumen del modelo
summary(modelo_lineal)
residuos <- residuals(modelo_lineal)

# Graficar los residuos vs valores ajustados
plot(fitted(modelo_lineal), residuos, 
     xlab = "Valores Ajustados", 
     ylab = "Residuos",
     main = "Gráfico de Residuos vs Valores Ajustados")
abline(h = 0, col = "red")  # Añadir una línea horizontal en 0

# Calcular el Error Absoluto Medio (MAE)
MAE <- mean(abs(dataset_prueba$NO2.GT. - predicciones))
print(MAE)


###################################MODELO DE REGRESION MULTILINEAL##################

#11. Algoritmo de Regresión multilineal

modelo_regresion_multilineal <- lm(NO2.GT. ~ CO.GT. + PT08.S1.CO. + NMHC.GT. + C6H6.GT. + 
                                     PT08.S2.NMHC. + NOx.GT. + PT08.S3.NOx. + 
                                     PT08.S4.NO2. + PT08.S5.O3. + T + RH + AH, 
                                   data = dataset_entrenamiento)

# Mostrar un resumen del modelo 
summary(modelo_regresion_multilineal)
predicciones <- predict(modelo_regresion_multilineal, newdata = dataset_prueba)

# Ver las primeras predicciones
head(predicciones)
mae <- mean(abs(predicciones - dataset_prueba$NO2.GT.))
print(mae)

###################################MODELO DE REGRESION LOGISTICA##################

#12. Preparación y creación del algoritmo Regresión Logística

dataset_entrenamiento_logistica <- dataset_entrenamiento
dataset_prueba_logistica <- dataset_prueba

#función para categorizar variable objetivo 
convertir_NO2_a_categoria <- function(data, columna_NO2) {
  # Definir los umbrales
  umbral_aceptable <- 40
  umbral_danino <- 90
  # Reemplazar la columna original con la categórica
  data[[columna_NO2]] <- ifelse(data[[columna_NO2]] <= umbral_aceptable, "Aceptable", 
                                ifelse(data[[columna_NO2]] <= umbral_danino, "Dañino", "Muy peligroso"))
  return(data)
}

# Usar la función en el dataset
dataset_entrenamiento_logistica <- convertir_NO2_a_categoria(dataset_entrenamiento_logistica, "NO2.GT.")
dataset_prueba_logistica <- convertir_NO2_a_categoria(dataset_prueba_logistica, "NO2.GT.")


table(dataset_entrenamiento_logistica$NO2.GT.)
table(dataset_prueba_logistica$NO2.GT.)
ncol(dataset_entrenamiento_logistica)
ncol(dataset_prueba_logistica)

dataset_entrenamiento_logistica$NO2.GT. <- factor(dataset_entrenamiento_logistica$NO2.GT., levels = c("Aceptable", "Dañino", "Muy peligroso"))
dataset_prueba_logistica$NO2.GT. <- factor(dataset_prueba_logistica$NO2.GT., levels = c("Aceptable", "Dañino", "Muy peligroso"))


#Se crea el modelo de regresión logística
modelo_logistica_multinomial <- multinom(NO2.GT. ~ CO.GT. + PT08.S1.CO. + NMHC.GT. + C6H6.GT. +
                                           PT08.S2.NMHC. + NOx.GT. + PT08.S3.NOx. + PT08.S4.NO2. + 
                                           PT08.S5.O3. + T + RH + AH, 
                                         data = dataset_entrenamiento_logistica)
summary(modelo_logistica_multinomial)


# Realizar predicciones en el conjunto de prueba
predicciones_prob <- predict(modelo_logistica_multinomial, newdata = dataset_prueba_logistica, type = "class")
matriz_confusion <- confusionMatrix(predicciones_prob, dataset_prueba_logistica$NO2.GT.)

# Imprimir la matriz de confusión y la precisión
print(matriz_confusion)
accuracy <- matriz_confusion$overall['Accuracy']
print(paste("Precisión del modelo:", round(accuracy * 100, 2), "%"))

#gráficarlo
matriz_df <- as.data.frame(matriz_confusion$table)

# Crear la gráfica de la matriz de confusión
ggplot(matriz_df, aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(title = "Matriz de Confusión", x = "Predicción", y = "Real") +
  theme_minimal()




