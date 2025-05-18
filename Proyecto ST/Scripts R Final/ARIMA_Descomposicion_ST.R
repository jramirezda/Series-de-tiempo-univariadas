library(readxl)
library(shiny)
library(plotly)
library(forecast)
library(tseries)
library(ggplot2)
library(lmtest)

setwd("C:/Users/jrodriguez/Downloads")
volumen <- read_excel("Exportaciones.xlsx", 
                      sheet = "1. Total_Volumen", range = "D7:E811")
valor <- read_excel("Exportaciones.xlsx", 
                    sheet = "2. Total_Valor", range = "C7:D811")

volumen_anual <- aggregate(`Total Exportaciones` ~ format(MES, "%Y"), data = volumen, FUN = sum)

serie_vol<-ts(volumen_anual[,2],start = c(1958),frequency=1)
serie_val<-ts(valor[,2],start = c(1958,1),frequency=12)

plot(serie_vol, main = "Serie de Tiempo Anual", col = "blue", lwd = 2)

autoplot(serie_vol) + ggtitle("Volumen de Café Exportado en Colombia (1958-2024)") +
  ylab("Volumen exportado") + xlab("Año")

# Gráficos ACF y PACF
acf(serie_vol, main = "ACF - Serie Original")
pacf(serie_vol, main = "PACF - Serie Original")

ggAcf(serie_vol) + ggtitle("ACF de la serie original")
ggPacf(serie_vol) + ggtitle("PACF de la serie original")

# Prueba de Dickey-Fuller (ADF)
adf_test <- adf.test(serie_vol)
print(adf_test)
kpss.test(serie_vol)
# Aplicar una diferenciación de primer orden
ts_data_diff <- diff(serie_vol, differences = 1)

# Graficar la serie diferenciada
plot(ts_data_diff, main = "Serie Diferenciada (d=1)", col = "darkgreen", lwd = 2)

# Prueba de estacionariedad (ADF) después de diferenciar
adf_test_diff <- adf.test(ts_data_diff)
print(adf_test_diff)
kpss.test(ts_data_diff)

par(mfrow=c(1,2))
# Gráficos ACF y PACF para identificar orden p y q
acf(ts_data_diff, main = "ACF - Serie Diferenciada")
pacf(ts_data_diff, main = "PACF - Serie Diferenciada")

modelo_auto <- auto.arima(log(serie_vol))
summary(modelo_auto)
coeftest(modelo_auto)

checkresiduals(modelo_auto)
checkresiduals(serie_vol)

shapiro.test(residuals(modelo_auto))
shapiro.test(rnorm(100))
Box.test(residuals(modelo_auto), type = "Ljung-Box", lag = 10)

# Pronóstico para los próximos 5 años
forecast_arima <- forecast(modelo_auto, h = 5)

par(mfrow=c(1,1))
# Graficar el pronóstico
plot(forecast_arima, main = "Pronóstico con ARIMA(0,1,0)", col = "blue", lwd = 2)

BoxCox.lambda((serie_vol), method = "guerrero", lower = 0, upper = 2)  

#######################
par(mfrow=c(3,1))
plot(serie_val, main = "Serie de Tiempo Anual", col = "blue", lwd = 2)
BoxCox.lambda(serie_val, method = "guerrero", lower = 0, upper = 2)  
b <- BoxCox(serie_val,0.1786157)
plot(b, main = "Serie de Valores Anuales BoxCox", ylab = "Residuos")
l <- log(serie_val)
plot(l, main = "Serie de Valores Anuales Logaritmo", ylab = "Residuos")

descom_add <- decompose(serie_val, type = "additive")
descom_mult <- decompose(serie_val, type = "multiplicative")

descom_add.b <- decompose(b, type = "additive")
descom_mult.b <- decompose(b, type = "multiplicative")

descom_add.l <- decompose(l, type = "additive")
descom_mult.l <- decompose(l, type = "multiplicative")

residuos_add <- descom_add$random
residuos_mult <- descom_mult$random
residuos_add.b <- descom_add.b$random
residuos_mult.b <- descom_mult.b$random
residuos_add.l <- descom_add.l$random
residuos_mult.l <- descom_mult.l$random

par(mfrow=c(3,2))
plot(residuos_add, main = "Residuos de la Descomposición Aditiva", ylab = "Residuos")
abline(h = 0, col = "red", lwd = 2)
plot(residuos_mult, main = "Residuos de la Descomposición Multiplicativa", ylab = "Residuos")
abline(h = 1, col = "red", lwd = 2)
plot(residuos_add.b, main = "Residuos de la Descomposición Aditiva BoxCox", ylab = "Residuos")
abline(h = 0, col = "red", lwd = 2)
plot(residuos_mult.b, main = "Residuos de la Descomposición Multiplicativa BoxCox", ylab = "Residuos")
abline(h = 1, col = "red", lwd = 2)
plot(residuos_add.l, main = "Residuos de la Descomposición Aditiva Logaritmo", ylab = "Residuos")
abline(h = 0, col = "red", lwd = 2)
plot(residuos_mult.l, main = "Residuos de la Descomposición Multiplicativa Logaritmo", ylab = "Residuos")
abline(h = 1, col = "red", lwd = 2)

par(mfrow = c(2, 2))
plot(descom_add)
plot(descom_mult)
plot(descom_add.b)
plot(descom_mult.b)
par(mfrow = c(1, 1))

tendencia_add <- descom_add.b$trend
tendencia_mult <- descom_mult.b$trend
estacionalidad_add <- descom_add.b$seasonal
estacionalidad_mult <- descom_mult.b$seasonal
residuos_add <- descom_add.b$random
residuos_mult <- descom_mult.b$random

adf.test(na.omit(tendencia_add))  # Tendencia
adf.test(na.omit(estacionalidad_add))  # Estacionalidad
adf.test(na.omit(residuos_add)) 

adf.test(na.omit(tendencia_mult))  
adf.test(na.omit(estacionalidad_mult))  
adf.test(na.omit(residuos_mult)) 

plot(residuos_add, main = "Residuos de la Descomposición Aditiva", ylab = "Residuos")
abline(h = 0, col = "red", lwd = 2)  # Línea de referencia

library(lmtest)
bptest(na.omit(residuos_mult) ~ time(na.omit(residuos_add)))

par(mfrow=c(1,1))
plot(residuos_mult ~ serie_val, main = "Residuos vs. Serie Original",
     xlab = "Valores de la serie", ylab = "Residuos", pch = 20)
abline(h = 0, col = "red", lwd = 2)


par(mfrow = c(1, 2))
hist(na.omit(residuos_add), main = "Histograma de Residuos (Aditivo)", col = "gray", probability = TRUE)
hist(na.omit(residuos_mult), main = "Histograma de Residuos (Multiplicativo)", col = "gray", probability = TRUE)

par(mfrow = c(1, 2))
acf(na.omit(residuos_add), main = "ACF de los Residuos (Aditivo)")
acf(na.omit(residuos_mult), main = "ACF de los Residuos (Multiplicativo)")

shapiro.test(na.omit(residuos_add))
shapiro.test(na.omit(residuos_mult))

Box.test(na.omit(residuos_add), type = "Ljung-Box")
Box.test(na.omit(residuos_mult), type = "Ljung-Box")

par(mfrow = c(1, 1))
