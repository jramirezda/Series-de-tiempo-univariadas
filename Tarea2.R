##taller 2 jramirezda
#cargar paquetes
# Crear un dataframe con los datos
produccion <- data.frame(
  Año = 1947:2023,
  Unidades = c(3, 5, 21, 26, 33, 44, 57, 58, 61, 81, 113, 183, 248, 306, 441, 493, 589, 654, 740, 665, 706, 729, 619, 928,
               1246, 1844, 1772, 1436, 1337, 1426, 1798, 1939, 2221, 2470, 2565, 2209, 2366, 2842, 3288, 3640, 3902, 4001,
               3821, 4293, 4487, 3384, 2345, 2671, 3144, 3350, 3518, 3652, 3669, 3946, 4305, 4276, 4238, 4975, 5409, 5671,
               6465, 6587, 6250, 6573, 7195, 7405, 7000, 7255, 7664, 8014, 8398, 9251, 10131, 9119, 11115, 13221, 13663)
)

# Crear un objeto de serie de tiempo
ts_produccion <- ts(produccion$Unidades, start = 1947, frequency = 1)

# Graficar la serie de tiempo
library(ggplot2)
ggplot(produccion, aes(x = Año, y = Unidades)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Producción Anual de Unidades de ferraris.",
       x = "Año",
       y = "Unidades Producidas") +
  theme_minimal()

# Cargar y filtrar los datos de manchas solares
library(readr)
manchas_solars <- read_csv("daily_sunspots_time_series_1850-01_2024-05.csv")
manchas_solars <- manchas_solars %>% select(date, counts) %>% filter(date >= "2000-01-01")

# Convertir a serie de tiempo
ts_manchas <- ts(manchas_solars$counts, start = c(2000, 1), frequency = 365)

# Graficar la serie de tiempo de manchas solares
ggplot(manchas_solars, aes(x = date, y = counts)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 0.5) +
  labs(title = "Manchas Solares Diarias",
       x = "Fecha",
       y = "Conteo de Manchas Solares") +
  theme_minimal()


###promedios moviles

# Calcular promedios móviles
manchas_solars <- manchas_solars %>%
  mutate(
    MA5 = rollmean(counts, k = 30, fill = NA, align = "right"),
    MA10 = rollmean(counts, k = 180, fill = NA, align = "right"),
    MA15 = rollmean(counts, k = 365, fill = NA, align = "right")
  )
produccion <- produccion %>%
  mutate(
    MA3 = rollmean(Unidades, k = 3, fill = NA, align = "right"),
    MA4 = rollmean(Unidades, k = 5, fill = NA, align = "right"),
    MA5 = rollmean(Unidades, k = 10, fill = NA, align = "right")
  )

# Graficar la serie de tiempo de manchas solares con promedios móviles
ggplot(manchas_solars, aes(x = date)) +
  geom_line(aes(y = counts, color = "Original")) +
  geom_line(aes(y = MA5, color = "MA 30 días")) +
  geom_line(aes(y = MA10, color = "MA 180 días")) +
  geom_line(aes(y = MA15, color = "MA 365 días")) +
  labs(title = "Manchas Solares Diarias con Promedios Móviles",
       x = "Fecha",
       y = "Conteo de Manchas Solares") +
  theme_minimal() +
  scale_color_manual(values = c("Original" = "aquamarine", "MA 30 días" = "red", "MA 180 días" = "green", "MA 365 días" = "purple"))

# Graficar la serie de tiempo de producción con promedios móviles
ggplot(produccion, aes(x = Año)) +
  geom_line(aes(y = Unidades, color = "Original")) +
  geom_line(aes(y = MA3, color = "MA 3 años")) +
  geom_line(aes(y = MA4, color = "MA 5 años")) +
  geom_line(aes(y = MA5, color = "MA 10 años")) +
  labs(title = "Producción Anual con Promedios Móviles",
       x = "Año",
       y = "Unidades Producidas") +
  theme_minimal() +
  scale_color_manual(values = c("Original" = "aquamarine", "MA 3 años" = "red", "MA 5 años" = "green", "MA 10 años" = "purple"))

# punto 3 
library(forecast)
library(ggplot2)
library(tseries)
library(gridExtra)

set.seed(123)  # Fijar semilla para reproducibilidad
n <- 500       # Número de observaciones
simulations <- list()
#arma1,1
for (i in 1:3) {
  # Simulación del modelo ARMA(1,1)
  ar1 <- 0.9
  ma1 <- -0.9
  Wt <- rnorm(n, mean = 0, sd = 1)
  xt <- numeric(n)
  xt[1] <- Wt[1]
  
  for (t in 2:n) {
    xt[t] <- ar1 * xt[t - 1] + Wt[t] + ma1 * Wt[t - 1]
  }
  
  # Guardar resultados
  simulations[[i]] <- xt
  
  # Gráfico de la serie simulada
  p1 <- ggplot(data.frame(t = 1:n, x = xt), aes(x = t, y = x)) +
    geom_line(color = "blue") +
    labs(title = paste("Serie Simulada", i,"modelo 1"), x = "Tiempo", y = "x_t") +
    theme_minimal()
  
  # ACF y PACF
  p2 <- ggAcf(xt) + ggtitle(paste("ACF - Simulación", i,"modelo 1"))
  p3 <- ggPacf(xt) + ggtitle(paste("PACF - Simulación", i, "modelo 1"))
  
  # Ajuste del modelo ARMA(1,1)
  modelo_ajustado <- arima(xt, order = c(1, 0, 1))
  print(summary(modelo_ajustado))
  
  # Mostrar gráficos
  grid.arrange(p1, p2, p3, nrow = 3)
}

#ar 2
for (i in 1:3) {
  # Simulación del modelo AR(2)
  ar1 <- 0.2
  ar2 <- 0.55
  Wt <- rnorm(n, mean = 0, sd = sqrt(2.25))
  xt <- numeric(n)
  xt[1] <- Wt[1]  # Corrección del índice inicial
  xt[2] <- ar1 * xt[1] + Wt[2]  # Definir segundo valor
  
  for (t in 3:n) {
    xt[t] <- ar1 * xt[t - 1] + ar2 * xt[t - 2] + Wt[t]
  }
  
  # Guardar resultados
  simulations[[i]] <- xt
  
  # Gráfico de la serie simulada
  p1 <- ggplot(data.frame(t = 1:n, x = xt), aes(x = t, y = x)) +
    geom_line(color = "blue") +
    labs(title = paste("Serie Simulada", i, "modelo AR(2)"), x = "Tiempo", y = "x_t") +
    theme_minimal()
  
  # ACF y PACF
  p2 <- ggAcf(xt) + ggtitle(paste("ACF - Simulación", i, "modelo AR(2)"))
  p3 <- ggPacf(xt) + ggtitle(paste("PACF - Simulación", i, "modelo AR(2)"))
  
  # Ajuste del modelo AR(2)
  modelo_ajustado <- arima(xt, order = c(2, 0, 0))
  print(summary(modelo_ajustado))
  
  # Mostrar gráficos
  grid.arrange(p1, p2, p3, nrow = 3)
}

#ma 3
for (i in 1:3) {
  # Simulación del modelo AR(2)
  ma1 <- 0.9
  ma2 <- -0.8
  ma3 <- -0.8
  Wt <- rnorm(n, mean = 0, sd = sqrt(9))
  xt <- numeric(n)
  xt[1] <- Wt[1]  # Corrección del índice inicial
  xt[2] <- ma1 * Wt[1] + Wt[2]  # Definir segundo valor
  xt[3] <- ma1 * Wt[2] +ma2* Wt[1] +Wt[3]
  
  for (t in 4:n) {
    xt[t] <- ma1 * Wt[t - 1] + ma2 * Wt[t - 2] +ma3* Wt[t-3] + Wt[t]
  }
  
  # Guardar resultados
  simulations[[i]] <- xt
  
  # Gráfico de la serie simulada
  p1 <- ggplot(data.frame(t = 1:n, x = xt), aes(x = t, y = x)) +
    geom_line(color = "blue") +
    labs(title = paste("Serie Simulada", i, "modelo MA(3)"), x = "Tiempo", y = "x_t") +
    theme_minimal()
  
  # ACF y PACF
  p2 <- ggAcf(xt) + ggtitle(paste("ACF - Simulación", i, "modelo MA(3)"))
  p3 <- ggPacf(xt) + ggtitle(paste("PACF - Simulación", i, "modelo MA(3)"))
  
  # Ajuste del modelo AR(2)
  modelo_ajustado <- arima(xt, order = c(0, 0, 3))
  print(summary(modelo_ajustado))
  
  # Mostrar gráficos
  grid.arrange(p1, p2, p3, nrow = 3)
}


#punto 3 
library(forecast)
library(ggplot2)
library(tseries)
library(gridExtra)
library(readxl)
library(TSA)  # Para la función EACF

# Cargar datos desde Excel
Datos <- read_excel("HW02-DatosPunto4.xls")

# Convertir las series en objetos de serie de tiempo
timeseries_list <- list(
  uno = ts(Datos$serie1, frequency = 1),
  dos = ts(Datos$serie2, frequency = 1),
  tres = ts(Datos$serie3, frequency = 1),
  cuatro = ts(Datos$serie4, frequency = 1)
)

plot_series <- function(series, series_name) {
  n <- length(series)
  df <- data.frame(t = 1:n, x = series)
  
  # Gráfico de la serie de tiempo
  p1 <- ggplot(df, aes(x = t, y = x)) +
    geom_line(color = "blue") +
    labs(title = paste("Serie de Tiempo", series_name), x = "Tiempo", y = "x_t") +
    theme_minimal()
  
  # Gráfico de ACF
  p2 <- ggAcf(series) + ggtitle(paste("ACF - Serie", series_name))
  
  # Gráfico de PACF
  p3 <- ggPacf(series) + ggtitle(paste("PACF - Serie", series_name))
  
  # Combinar los gráficos en una sola figura
  grid.arrange(p1, p2, p3, nrow = 3)
}

# Generar los gráficos para cada serie de tiempo
for (i in seq_along(timeseries_list)) {
  series_name <- names(timeseries_list)[i]
  plot_series(timeseries_list[[i]], series_name)
}

##arma
timeseries_results <- list()

for (name in names(timeseries_list)) {
  ts_data <- timeseries_list[[name]]
  
  # Identificación del modelo ARMA con EACF
  print(paste("EACF para la serie:", name))
  eacf_result <- eacf(ts_data)
  print(eacf_result)
  
  # Ajuste del modelo ARMA (sin diferenciación regular)
  modelo_ajustado <- auto.arima(ts_data, d = 0, seasonal = FALSE)
  print(paste("Modelo ajustado para:", name))
  print(summary(modelo_ajustado))
  
  # Evaluación de residuales
  checkresiduals(modelo_ajustado)
  
  # Pronósticos h=6
  forecast_h <- forecast(modelo_ajustado, h = 6)
  print(forecast_h)
  
  # Graficar la serie original y el pronóstico
  p1 <- autoplot(ts_data) +
    autolayer(forecast_h, series = "Pronóstico", PI = TRUE) +
    labs(title = paste("Serie y Pronóstico -", name), x = "Tiempo", y = "Valor") +
    theme_minimal()
  
  # Evaluación de estacionariedad e invertibilidad
  estacionario <- all(Mod(polyroot(c(1, -modelo_ajustado$coef[grepl("ar", names(modelo_ajustado$coef))]))) > 1)
  invertible <- all(Mod(polyroot(c(1, modelo_ajustado$coef[grepl("ma", names(modelo_ajustado$coef))]))) > 1)
  
  print(paste("La serie", name, "es estacionaria:", estacionario))
  print(paste("La serie", name, "es invertible:", invertible))
  
  # Guardar resultados
  timeseries_results[[name]] <- list(
    eacf = eacf_result,
    modelo = modelo_ajustado,
    forecast = forecast_h,
    estacionario = estacionario,
    invertible = invertible,
    plot = p1
  )
  
  print(timeseries_results[[name]]$plot)
}

