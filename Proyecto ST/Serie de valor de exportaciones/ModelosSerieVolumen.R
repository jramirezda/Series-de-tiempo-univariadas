#################### Analisis del precio del Cafe ####################
rm(list=ls(all=TRUE))
##### Libreias #####
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
library(MASS)
library(latex2exp)
library(fpp3)
library(tsoutliers)
library(FinTS)
library(tsibble)

##### Datos #####
volumen = read_excel("Exportaciones.xlsx", 
                      sheet = "1. Total_Volumen", range = "D7:E811")
volumen_anual = aggregate(`Total Exportaciones` ~ format(MES, "%Y"), data = volumen, FUN = sum)
svol = ts(volumen_anual[,2],start = 1958,frequency = 1)

# Grafico de la serie
par(mfrow=c(1,1))
plot(svol,type="l", main= "Serie del Volumen del cafe ",xlab="Tiempo")
# tendencia estocastica, heterocedasticidad, estacionalidad no es clara

# Datos de entrenamiento
svolTr = svol[1:64]
svolTr = ts(svolTr,start = 1958,frequency = 1)
# Datos de testeo
svolTest = svol[65:67]
svolTest = ts(svolTest,start = 2022,frequency = 1)


# Autocorrelograma y Autocorrelograma parcial
par(mfrow=c(1,2))
acf(svolTr)
pacf(svolTr)

##### Identificación #####
# Heterocedasticidad
box_cox_transform <- function(x, lambda) {
  if (lambda == 0) {
    log(x)  # For lambda = 0, use the log transformation
  } else {
    (x^lambda - 1) / lambda
  }
}

lambda.optim = forecast::BoxCox.lambda(svolTr, lower=0);lambda.optim
lambda.optim = forecast::BoxCox.lambda(svolTr, method = "loglik", lower=0);lambda.optim
log.svol = log(svolTr)

par(mfrow=c(1,1))
plot(log.svol,type="l", main= "Log del Volumen de café",xlab="Tiempo")
forecast::BoxCox.lambda(log.svol, method = "loglik", lower=0)

# Para corrobar las diferencias regulares y estacionales
ndiffs(log.svol) # 1 dif regular
nsdiffs(log.svol) #error

# Ho: Raiz unitaria (No estacionaria). vs Ha: Estacionariedad
# prueba con tendencia y drift
tseries::adf.test(log.svol) # p.valor > 0.05 se No rechaza Ho. Raiz unitaria
#Kwiatkowski et al. (1992)°Øs stationarity test
# Ho: Estacionariedad vs Ha: Raiz unitaria (No estacionaria)
tseries::kpss.test(log.svol) # p.valor < 0.05 se rechaza Ho. Raiz unitaria
# Ho: Raiz unitaria (No estacionaria). vs Ha: Estacionariedad
tseries::pp.test(log.svol) # p.valor > 0.05 No se rechaza Ho. Raiz unitaria

# Primera diferencia regular
diff.log.svol = diff(log.svol)

# Ho: Raiz unitaria (No estacionaria). vs Ha: Estacionariedad
# prueba con tendencia y drift
tseries::adf.test(diff.log.svol) # p.valor < 0.05 se rechaza Ho. Estacionariedad
#Kwiatkowski et al. (1992)°Øs stationarity test
# Ho: Estacionariedad vs Ha: Raiz unitaria (No estacionaria)
tseries::kpss.test(diff.log.svol) # p.valor > 0.05 No se rechaza Ho. Estacionariedad
# Ho: Raiz unitaria (No estacionaria). vs Ha: Estacionariedad
tseries::pp.test(diff.log.svol) # p.valor < 0.05 se rechaza Ho. Estacionariedad

aTSA::adf.test(diff.log.svol,nlag = 15)

par(mfrow=c(1,1))
plot(diff.log.svol,type="l", main= "Diferencia regular del log de la serie (d=1)",xlab="Tiempo")
par(mfrow=c(1,2))
acf(diff.log.svol, main = "ACF - Log Serie Diferenciada")
pacf(diff.log.svol, main = "PACF - Log Serie Diferenciada") # serie estacionaria

# Modelo inicial
auto.arima(diff.log.svol, trace = T) # ARMA(0,0,0) X.X

fit = Arima(svolTr, order=c(0,1,0),include.mean = F, lambda = 0)
summary(fit)

##### Análisis de Residuales #####
r.mod = residuals(fit)

par(mfrow=c(1,2))
acf(r.mod)
pacf(r.mod) # serie estacionaria

par(mfrow=c(1,2))
acf(r.mod^2)
pacf(r.mod^2) # serie estacionaria

# Gráficos
tsdiag(fit) # posible valor atipico

par(mfrow=c(1,2))
hist(r.mod, freq = F, ylim = c(0,5), main = "Histograma de los residuales")
lines(density(r.mod))
qqnorm(r.mod); qqline(r.mod,col=2) # Valores atipicos

# Pruebas de normalidad
ad.test(r.mod) # No normalidad
shapiro.test(r.mod) # No Normalidad


at_est = r.mod
# Cartas CUSUM y CUSUMQ
cum=cumsum(at_est)/sd(at_est)
N=length(at_est)
cumq=cumsum(at_est^2)/sum(at_est^2)
Af=0.948 ###Cuantil del 95% para la estad?stica cusum
co=0.14013####Valor del cuantil aproximado para cusumsq para n/2
####Para el caso de la serie de pasajeros es aprox (144-12)/2=66
LS=Af*sqrt(N)+2*Af*c(1:length(at_est))/sqrt(N)
LI=-LS
LQS=co+(1:length(at_est))/N
LQI=-co+(1:length(at_est))/N
par(mfrow=c(1,2))
plot(cum,type="l",ylim=c(min(LI),max(LS)),xlab="t",ylab="",main="CUSUM")
lines(LS,type="S",col="red")
lines(LI,type="S",col="red")
#CUSUMSQ
plot(cumq,type="l",xlab="t",ylab="",main="CUSUMSQ")                      
lines(LQS,type="S",col="red")                                                                           
lines(LQI,type="S",col="red")

# Identificación de datos atipicos
coef= coefs2poly(fit)
coef
outliers= tsoutliers::locate.outliers(r.mod,coef)
outliers###tstat se compara con C=3
?tso####Detección automática de outliers, donde el modelo que se propone es via auto.arima
tso(svol)

xreg = outliers.effects(outliers, 64)
xreg

# Estimación del modelo con datos atipicos
fit2 = Arima(svolTr, order=c(0,1,0),include.mean = F, lambda = 0, xreg = xreg)
summary(fit2)

##### Análisis de Residuales #####
r.mod2 = residuals(fit2)

par(mfrow=c(1,1))
plot(r.mod2, main = "Residuales del modelo ARIMA(0,1,0) con intercenciones y logaritmo natural")

par(mfrow=c(1,2))
acf(r.mod2)
pacf(r.mod2) # serie estacionaria

par(mfrow=c(1,2))
acf(r.mod2^2)
pacf(r.mod2^2) # serie estacionaria

# Gráficos
tsdiag(fit2) # posible valor atipico

# Pruebas de normalidad
ad.test(r.mod2) # No normalidad
shapiro.test(r.mod2) # Normalidad

par(mfrow=c(1,2))
hist(r.mod2, freq = F, ylim = c(0,5.5), main = "Histograma de los residuales")
lines(density(r.mod2))
qqnorm(r.mod2); qqline(r.mod,col=2) # Valores atipicos

at_est = r.mod2
# Cartas CUSUM y CUSUMQ
cum=cumsum(at_est)/sd(at_est)
N=length(at_est)
cumq=cumsum(at_est^2)/sum(at_est^2)
Af=0.948 ###Cuantil del 95% para la estad?stica cusum
co=0.14013####Valor del cuantil aproximado para cusumsq para n/2
####Para el caso de la serie de pasajeros es aprox (144-12)/2=66
LS=Af*sqrt(N)+2*Af*c(1:length(at_est))/sqrt(N)
LI=-LS
LQS=co+(1:length(at_est))/N
LQI=-co+(1:length(at_est))/N
par(mfrow=c(1,2))
plot(cum,type="l",ylim=c(min(LI),max(LS)),xlab="t",ylab="",main="CUSUM")
lines(LS,type="S",col="red")
lines(LI,type="S",col="red")
#CUSUMSQ
plot(cumq,type="l",xlab="t",ylab="",main="CUSUMSQ")                      
lines(LQS,type="S",col="red")                                                                           
lines(LQI,type="S",col="red")

# Capacidad predictiva
mod.evalua0 = Arima(svolTest,model=fit)
accuracy(mod.evalua0)

regresoras=cbind(c(rep(0,3)),c(rep(1,3)))
mod.evalua = Arima(svolTest,model=fit2, xreg = regresoras)
accuracy(mod.evalua) # presice mejor


##### Predicción fuera de muestra #####
xregf = rbind(xreg,regresoras)
fit3 = Arima(svol, order=c(0,1,0),include.mean = F, lambda = 0, xreg = xregf)
summary(fit3)

regresoras2=cbind(c(rep(0,5)),c(rep(1,5)))
colnames(regresoras2) = c("AO20", "LS21")
pronostico_out=forecast(object=fit3,xreg=regresoras2,h=5) 
par(mfrow=c(1,1))
plot(pronostico_out, main = "Pronostico a 5 años")

##### red neuronal #####
# NNETAR ---------------------------------------------------------------------
serie_vol_tsibble = as_tsibble(svolTr)

# Fit the model using NNETAR
fit4 <- serie_vol_tsibble |>
  model(NNETAR((value)))  #<NNAR(26,1,14)[12]>

forecastNNETNAR<-forecast::forecast(fit4,h=3)
forecastNNETNAR_1<-ts(forecastNNETNAR$.mean,start=c(2022),frequency=1)
#RMSE
pp<-((forecastNNETNAR$.mean- svolTest)^2) 
RMSE_NNERNAR=sqrt(mean(pp)) # predice mejor

# predicción fuera de muestra
serie_vol_tsibble = as_tsibble(svol)

# Fit the model using NNETAR
fit5 <- serie_vol_tsibble |>
  model(NNETAR((value)))  #<NNAR(26,1,14)[12]>

forecastNNETNAR<-forecast::forecast(fit5,h=5)

svol%>%
  forecast::forecast(fit5,h=5) %>%
  autoplot

