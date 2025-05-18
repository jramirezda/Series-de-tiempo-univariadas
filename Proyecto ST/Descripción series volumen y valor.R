
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
setwd("C:/Users/jufem/OneDrive/Documentos/Series de tiempo/Proyecto final")



volumen <- read_excel("Exportaciones.xlsx", 
                      sheet = "1. Total_Volumen", range = "D7:E811")
volumen_anual <- aggregate(`Total Exportaciones` ~ format(MES, "%Y"), data = volumen, FUN = sum)

valor <- read_excel("Exportaciones.xlsx", 
                    sheet = "2. Total_Valor", range = "C7:D801")



serie_vol<-ts(volumen_anual[,2],start = c(1958),frequency=1)

serie_val<-ts(valor[,2],start = c(1958,1),frequency=12)



par(mfrow=c(1,2))
plot(serie_vol,main="Volumen de exportaciones de café \n en miles de sacos")
plot(serie_val,main="Valor de exportaciones de café \n en millones de dolares USD")





# Graficos en potly -------------------------------------------------------

fig1 <- plot_ly(volumen_anual, 
                y = ~`Total Exportaciones`, 
                x = ~`format(MES, "%Y")`, 
                type = 'scatter', 
                mode = 'lines', 
                name = "Volumen en miles de \n sacos exportados") %>%
  layout(title = "Exportaciones de café en valor y volumen",
         xaxis = list(title = "Años"),
         yaxis = list(title = "Volumen miles de sacos"))

# Second plot (fig2) - Valor de exportaciones de café
fig2 <- plot_ly(valor, 
                y = ~`Valor Nominal*`, 
                x = ~Mes, 
                type = 'scatter', 
                mode = 'lines', 
                name = "Valor en millones de USD") %>%
  layout(title = "Exportaciones de café en valor y volumen",
         xaxis = list(title = "Mes"),
         yaxis = list(title = "Valor millones USD"))

# Combine both plots into the same panel (side by side)
subplot(fig1, fig2, nrows = 1, shareX = FALSE, shareY = FALSE) %>%
  layout(title = "Exportaciones de café en valor y volumen",
         showlegend = TRUE)




