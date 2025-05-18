## Parcial 2 Jhon Ramírez y Javier Rodriguez
### Punto 2
library(readxl)
library(tseries)
library(forecast)

# Cargar los datos desde el archivo Excel
P2 <- read_excel("P2.xls")

# Convertir a serie de tiempo trimestral (desde 1977, frecuencia = 4)
P2 <- log(ts(P2$DESEMP, start = c(1977, 1), frequency = 4))

# Gráfico de la serie original
plot(P2, main = "Tasa de Desempleo Nacional Urbano en Colombia", 
     ylab = "Tasa de Desempleo", xlab = "Año", col = "blue", lwd = 2)

# Diferenciación regular (elimina tendencia)
P2d1 <- diff(P2)
plot(P2d1, main = "Serie Diferenciada (Orden 1)", 
     ylab = "Cambio en la Tasa de Desempleo", xlab = "Año", col = "red", lwd = 2)
# Pruebas de estacionariedad (Dickey-Fuller Aumentado)
adf_test_original <- adf.test(P2, alternative = "stationary")
print(adf_test_original)

adf_test_d1 <- adf.test(P2d1, alternative = "stationary")
print(adf_test_d1)

# Autocorrelación y autocorrelación parcial
acf(P2d1, main = "Función de Autocorrelación (ACF) de la Serie Diferenciada")
pacf(P2d1, main = "Función de Autocorrelación Parcial (PACF) de la Serie Diferenciada")

# Modelo de tendencia lineal en diferentes transformaciones
time_index <- time(P2)  
modelo_tendencia_original <- lm(P2 ~ time_index)
summary(modelo_tendencia_original)

time_index <- time(P2d1)
modelo_tendencia_d1 <- lm(P2d1 ~ time_index)
summary(modelo_tendencia_d1)


# Ajuste de modelo ARIMA automáticamente diferencida
modelo1 <- auto.arima(P2d1)
summary(modelo1)
# Realizar pronóstico a 1 año (4 trimestres)
pronostico <- forecast(modelo1, h = 4)
# Último valor de P2, necesario para la reconversión
ultimos_P2 <- tail(P2, 1) 

# Cantidad de valores pronosticados
n_pron <- length(pronostico$mean)

# Crear series de tiempo para las predicciones, asegurando la correcta alineación temporal
pronostico_ts <- ts(pronostico$mean, start = end(P2) + c(0, 1/4), frequency = frequency(P2))
pronostico_lower_ts <- ts(pronostico$lower[,1], start = end(P2) + c(0, 1/4), frequency = frequency(P2))
pronostico_upper_ts <- ts(pronostico$upper[,1], start = end(P2) + c(0, 1/4), frequency = frequency(P2))

# Reconvertir las predicciones a la escala original usando cumsum
pronostico_original_mean <- ts(cumsum(c(ultimos_P2, pronostico_ts))[-1], 
                               start = start(pronostico_ts), frequency = frequency(pronostico_ts))

pronostico_original_lower <- ts(cumsum(c(ultimos_P2, pronostico_lower_ts))[-1], 
                                start = start(pronostico_lower_ts), frequency = frequency(pronostico_lower_ts))

pronostico_original_upper <- ts(cumsum(c(ultimos_P2, pronostico_upper_ts))[-1], 
                                start = start(pronostico_upper_ts), frequency = frequency(pronostico_upper_ts))

# Definir límites del gráfico para mayor claridad
x_lim <- c(start(P2)[1], end(P2)[1] + n_pron / frequency(P2))
y_lim <- range(c(P2, pronostico_original_lower, pronostico_original_upper))

# Graficar la serie original
plot(P2, main = "Tasa de Desempleo con Predicciones y Bandas de Confianza",
     ylab = "Tasa de Desempleo", xlab = "Año", col = "blue", lwd = 2,
     xlim = x_lim, ylim = y_lim)  

# Rellenar la banda de confianza
time_index <- time(pronostico_original_mean)
polygon(c(time_index, rev(time_index)), 
        c(pronostico_original_upper, rev(pronostico_original_lower)), 
        col = rgb(1, 0, 0, 0.2), border = NA)  # Banda de confianza en rojo semitransparente

# Agregar líneas de predicción y bandas de confianza
lines(pronostico_original_mean, col = "green", lwd = 2)
lines(pronostico_original_lower, col = "red", lty = 2, lwd = 1.5)
lines(pronostico_original_upper, col = "red", lty = 2, lwd = 1.5)

# Agregar leyenda mejor posicionada
legend("topleft", legend = c("Datos Originales", "Predicciones", "Bandas de Confianza"),
       col = c("blue", "green", "red"), lty = c(1, 1, 2), lwd = c(2, 2, 1.5),
       bg = "white")

## ajuste modelo sin diferencia 
modelo2=auto.arima(P2)
summary(modelo2)

pronostico <- forecast(modelo2, h = 4)

# Gráfico del pronóstico
plot(pronostico, main = "Pronóstico de la Tasa de Desempleo (ARIMA)", 
     ylab = "Tasa de Desempleo", xlab = "Año", col = "blue")




###Punto 4 Filtro KAlman 
set.seed(123)

# Simulación de una serie AR(1)
n <- 100  # Número de observaciones
phi_true <- 0.13
sigma_epsilon <- 1

y <- numeric(n)
y[1] <- rnorm(1, mean = 0, sd = sigma_epsilon)

for (t in 2:n) {
  y[t] <- phi_true * y[t - 1] + rnorm(1, mean = 0, sd = sigma_epsilon)
}

# Filtro de Kalman para estimar phi
kalman_filter <- function(y, phi_initial = 0, sigma_epsilon = 1) {
  n <- length(y)
  phi_estimates <- numeric(n)
  phi_estimates[1] <- phi_initial
  P <- 1  # Estimación inicial de la varianza del error
  Q <- 0  # Supuesto de que el modelo es lineal y no tiene incertidumbre de proceso
  R <- sigma_epsilon^2  # Varianza del ruido de medición
  
  for (t in 2:n) {
    # Predicción
    phi_pred <- phi_estimates[t-1]
    P_pred <- P + Q
    
    # Innovación (residual)
    innovation <- y[t] - phi_pred * y[t-1]
    
    # Ganancia de Kalman
    K <- P_pred * y[t-1]^2 / (y[t-1]^2 * P_pred + R)
    
    # Actualización
    phi_estimates[t] <- phi_pred + K * innovation
    P <- (1 - K * y[t-1]^2) * P_pred
  }
  
  return(phi_estimates)
}

# Estimación de phi utilizando el filtro de Kalman
phi_estimates <- kalman_filter(y, phi_initial = 0, sigma_epsilon = sigma_epsilon)

# Gráfica de los valores estimados de phi
plot(phi_estimates, type = "l", col = "blue", lwd = 2, main = "Estimación de phi usando el Filtro de Kalman", ylab = "Valor estimado de phi", xlab = "Tiempo")
abline(h = phi_true, col = "red", lwd = 2, lty = 2)
legend("right", legend = c("Estimación", "Valor verdadero"), col = c("blue", "red"), lwd = 2, lty = c(1, 2))
