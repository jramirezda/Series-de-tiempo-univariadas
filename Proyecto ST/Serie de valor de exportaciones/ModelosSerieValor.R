
# Librerias ---------------------------------------------------------------

library(readxl)
library(shiny)
library(plotly)
library(urca)
library(forecast)
library(tseries)
library(lmtest)
library(uroot)
library(fUnitRoots)
library(aTSA)
library(car)
library(nortest)
library(rugarch)
library(FinTS)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(tsibble)
library(fable)
library(fabletools)
library(forecast)

# Función cucuq -----------------------------------------------------------
source("C:/Users/jufem/OneDrive/Documentos/Series de tiempo/cucuplots.R")
setwd("C:/Users/jufem/OneDrive/Documentos/Series de tiempo/Proyecto final")



valor <- read_excel("Exportaciones.xlsx", 
                      sheet = "2. Total_Valor", range = "C7:D801")


# Partición serie val train vs test ---------------------------------------


#Conjunto de entrenamiento
T<-nrow(valor)-12#cogemos 12 meses de test


serie_val<-ts(valor[1:T,2],start = c(1958,1),frequency=12)
#Conjunto de test
valor_test<-valor[(T+1):794,2]$`Valor Nominal*`


#Data train vs test plot
autoplot(serie_val,main="Valor de exportaciones de café \n en millones de dolares USD")+autolayer(ts(valor_test,start=c(2023,3),frequency=12),series="Data Test")





# Identificacion ----------------------------------------------------------

  # Para corrobar las diferencias regulares y estacionales
  ndiffs(serie_val)    ####Decreta cuantas diferencias regulares son requeridas
  nsdiffs(serie_val)   ####Decreta cuantas diferencias estacional son requeridas
  


#Primeras diferencias y transformacion log
plot(diff(log(serie_vol)))
plot(diff(log(serie_val)))

#acf
acf(serie_vol)

acf(serie_val)
acf(diff(log(serie_vol)))
acf(diff(log(serie_val)))

#pacf
pacf(serie_vol)
pacf(serie_val)
pacf(diff(log(serie_vol)))
pacf(diff(log(serie_val)))


# Modelo Arima ------------------------------------------------------------
auto.arima(serie_val)
fit1<-Arima(serie_val,order=c(3,1,3),seasonal=c(0,0,2),include.drift = F)
summary(fit1)
tsdiag(fit1)
res<-residuals(fit1)
rest<-(res-mean(res,na.rm = T))/sd(res,na.rm =T )
plot(rest)
rest
acf((rest))
qqnorm(rest)
qqline(rest,col=2)
shapiro.test(rest)
hist(rest)



# MODELOS GARCH -----------------------------------------------------------



# Especificar el modelo GARCH(1,1)
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                         mean.model = list(armaOrder = c(3,3))) # Usamos ARIMA(3,1,1) aquí
garch_fit <- ugarchfit(garch_spec, serie_val)
garch_spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(3,3))) # Usamos ARIMA(3,1,1) aquí
garch_fit2 <- ugarchfit(garch_spec2, (serie_val))

#Exponential garch egarch
garch_spec3 <-  ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(3,3)),
  distribution.model = "std"  # Usar t-distribución o GED
)
garch_fit3 <- ugarchfit(garch_spec3, (serie_val))
#Cambiando por una t
garch_spec4<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
           mean.model = list(armaOrder = c(3,3)),
           distribution.model = "std")  # 'std' for t-distribution
garch_fit4 <- ugarchfit(garch_spec4, (serie_val))


#t asimetrica
garch_spec5<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(3,3)),
                         distribution.model = "sstd")  # t asimetrica
garch_fit5 <- ugarchfit(garch_spec5, (serie_val))
# GED para distribución de error generalizada
garch_spec6<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(3,3)),
                         distribution.model = "ged")  # t asimetrica
garch_fit6 <- ugarchfit(garch_spec6, (serie_val))
# Gjrgarch
garch_spec7<- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(3,3)),
                         distribution.model = "ged")  # t asimetrica
garch_fit7 <- ugarchfit(garch_spec7, (serie_val))

#Cambiando por una t y el modelo arma
garch_spec8<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(2,1)),
                         distribution.model = "std")  # 'std' for t-distribution
garch_fit8 <- ugarchfit(garch_spec8, (serie_val))


#Criterios de informacion.
cbind(c("AIC","BIC","Shibata","Hannan-Quinn"),sapply(list(garch_fit,garch_fit2,garch_fit4,garch_fit5,garch_fit6,garch_fit7,garch_fit8),FUN = function(x) infocriteria(x)))
#Escogemos modelo 6 y lo vamos a comparar con ARIMA Y Red Neuronal



# NNETAR ---------------------------------------------------------------------
serie_val_tsibble <- as_tsibble(serie_val)

# Fit the model using NNETAR
fit <- serie_val_tsibble |>
  model(NNETAR((value)))  #<NNAR(26,1,14)[12]>



# Seleccionando modelo con RMSE ---------------------------------------------------------------

valor_test<-valor[(T+1):794,2]$`Valor Nominal*`
#arima
forecastArima<-forecast::forecast(fit1,12)$mean
RMSE_arima=sqrt(mean(((forecastArima-valor_test)^2) ))

#garch
forecast_g <- ugarchforecast(garch_fit6, n.ahead = 12)
par(mfrow=c(1,1));plot(forecast,which =1)
#RMSE
forecastGarch<-as.numeric(forecast_g@forecast$seriesFor)
pp<-((forecastGarch-valor_test)^2) 
RMSE_garch=sqrt(mean(pp))

#Red Neuronal
forecastNNETNAR<-forecast::forecast(fit,h=12)
forecastNNETNAR_1<-ts(forecastNNETNAR$.mean,start=c(2023,3),frequency=12)
#RMSE
pp<-((forecastNNETNAR$.mean-valor_test)^2) 
RMSE_NNERNAR=sqrt(mean(pp))

#Comparando RMSE
cbind(RMSE_garch,RMSE_arima,RMSE_NNERNAR) #Elegimos modelo garch6



# Validacion Garch 6 --------------------------------------------------------------

res<-garch_fit6@fit$residuals
plot((res-mean(res))/sd(res))
par(mfrow=c(1,1))
#normalidad
qqnorm(res)
qqline(res,col="red")
shapiro.test(res)
#cusum
cucuq(res,0.11848)


#Validación:
plot(garch_fit6, which = "all")
plot(garch_fit6, which = 3)  #Conditional standard derivation
plot(garch_fit6, which = 10)  #ACF of residual


# Pronosticos con todos los modelos ----------------------------------------------------------

#Data train vs test plot vs forecast garc y arima.
autoplot(serie_val,main="Valor de exportaciones de café \n en millones de dolares USD")+
  autolayer(ts(valor_test,start=c(2023,3),frequency=12),series="Data Test")+
  autolayer(forecastArima)+autolayer(ts(forecastGarch,start=c(2023,3),frequency=12),series="forecastgarch")+
  autolayer(forecastNNETNAR_1,series="forecastNNETNAR")






           