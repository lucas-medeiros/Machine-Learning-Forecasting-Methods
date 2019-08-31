#bibliotecas
require(graphics)
library(forecast)
library(TTR)

##define diretório correto: (modificar para a pasta que estão os arquivos)
#arquivos .R devem estar na mesma pasta que todos os .txt
setwd("~/Facul/Iniciação Científica/Códigos R")
load("~/Facul/Iniciação Científica/Códigos R/.RData")

## Seasonal Holt-Winters
m <- HoltWinters(co2)
plot(co2)
plot(m)
plot(fitted(m))

plot.ts(AirPassengers)
airPassengersHW <- HoltWinters(AirPassengers, seasonal = "mult")
plot(airPassengersHW)
airPassengersHW_fcst <- forecast:::forecast.HoltWinters(airPassengersHW, h=36)
forecast:::plot.forecast(airPassengersHW_fcst)
plot.ts(airPassengersHW_fcst$residuals) #pra testar qualidade do modelo


## Non-Seasonal Holt-Winters
x <- uspop + rnorm(uspop, sd = 5)
m <- HoltWinters(x, gamma = FALSE)
plot(m)

## Exponential Smoothing
m2 <- HoltWinters(x, gamma = FALSE, beta = FALSE)
lines(fitted(m2)[,1], col = 3)
# }

### exemplo holt-winters

data <- c(77,105,89,135,100,125,115,155,120,145,135,170)

data_ts <- ts(data, start = 1, end = 12, frequency = 1)

plot.ts(data_ts)

data_ts_hw <- HoltWinters(data_ts)
data_ts_hw

data_ts_hw_fcst <- forecast:::forecast(data_ts_hw, h=4)

forecast:::plot.forecast(data_ts_hw_fcst)



## teste de entrada de dados
dataset <- read.table("dataset_exemplo.txt", header = F, sep = "\t", dec = ",")
dataset_ts <- ts(dataset, start = c(1949, 1), end = c(1960, 12), frequency = 12)
plot.ts(dataset_ts)

datasetHW <- HoltWinters(dataset_ts, seasonal = "mult")
plot(datasetHW)
plot(fitted(datasetHW))
datasetHW_forecast <- forecast:::forecast.HoltWinters(datasetHW, h=24)
forecast:::plot.forecast(datasetHW_forecast)
plot.ts(datasetHW_forecast$residuals) #pra testar qualidade do modelo

