#bibliotecas
require(graphics)
library(forecast)
library(TTR)

##define diretório correto: (modificar para a pasta que estão os arquivos)
#arquivos .R devem estar na mesma pasta que todos os .txt
setwd("~/Facul/Iniciação Científica/Códigos R")
load("~/Facul/Iniciação Científica/Códigos R/.RData")


### Script FARINHA

##Quantidade total em sacos:
  #leitura da base de dados: (valores em arquivo txt)
  farinha_total_sacos <- read.csv("farinha_total_qntd_sacos.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  farinha_total_sacos_ts <- ts(farinha_total_sacos, start = c(2012, 6), end = c(2018, 12), frequency = 11)
  #plot da série temporal original
  plot.ts(farinha_total_sacos_ts, ylab = "Quantidade total de farinha em sacos")
  #aplicação do método de Holt-Winters aditivo
  farinha_total_sacos_HW <- HoltWinters(farinha_total_sacos_ts, seasonal = c("additive"))
  plot(farinha_total_sacos_HW)
  #plot de cada componente da série separadamente
  plot(fitted(farinha_total_sacos_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  farinha_total_sacos_forecast <- forecast:::forecast.HoltWinters(farinha_total_sacos_HW, h=11)
  forecast:::plot.forecast(farinha_total_sacos_forecast)
  plot.ts(farinha_total_sacos_forecast$residuals) #para estimar a qualidade do modelo



##Quantidade total em kg:
  farinha_total_kg <- read.csv("farinha_total_kg.txt", header = F, sep = "\t", dec = ",")
  farinha_total_kg_ts <- ts(farinha_total_kg, start = c(2012, 6), end = c(2018, 12), frequency = 11)
  plot.ts(farinha_total_kg_ts)
  farinha_total_kg_HW <- HoltWinters(farinha_total_kg_ts, seasonal = c("additive"))
  plot(farinha_total_kg_HW)
  plot(fitted(farinha_total_kg_HW))
  farinha_total_kg_forecast <- forecast:::forecast.HoltWinters(farinha_total_kg_HW, h=11)
  forecast:::plot.forecast(farinha_total_kg_forecast)
  plot.ts(farinha_total_kg_forecast$residuals) #para estimar a qualidade do modelo
  
  
##Quantidade de farinha premium em kg:
  farinha_premium <- read.csv("farinha_premium_kg.txt", header = F, sep = "\t", dec = ",")
  farinha_premium_ts <- ts(farinha_premium, start = c(2012, 6), end = c(2018, 12), frequency = 11)
  plot.ts(farinha_premium_ts)
  farinha_premium_HW <- HoltWinters(farinha_premium_ts, seasonal = c("additive"))
  plot(farinha_premium_HW)
  plot(fitted(farinha_premium_HW))
  farinha_premium_forecast <- forecast:::forecast.HoltWinters(farinha_premium_HW, h=11)
  forecast:::plot.forecast(farinha_premium_forecast)
  plot.ts(farinha_premium_forecast$residuals) #para estimar a qualidade do modelo
  
  
  
##Quantidade de farinha tipo 1 em kg:
  farinha_tipo1 <- read.csv("farinha_tipo1_kg.txt", header = F, sep = "\t", dec = ",")
  farinha_tipo1_ts <- ts(farinha_tipo1, start = c(2012, 9), end = c(2018, 12), frequency = 11)
  plot.ts(farinha_tipo1_ts)
  farinha_tipo1_HW <- HoltWinters(farinha_tipo1_ts, seasonal = c("additive"))
  plot(farinha_tipo1_HW)
  plot(fitted(farinha_tipo1_HW))
  farinha_tipo1_forecast <- forecast:::forecast.HoltWinters(farinha_tipo1_HW, h=11)
  forecast:::plot.forecast(farinha_tipo1_forecast)
  plot.ts(farinha_tipo1_forecast$residuals) #para estimar a qualidade do modelo
  
  
  
##Quantidade de farinha tipo 1 (considerando apenas a partir de 2016)
  farinha_tipo12016 <- read.csv("farinha_tipo1_kg2016.txt", header = F, sep = "\t", dec = ",")
  farinha_tipo12016_ts <- ts(farinha_tipo12016, start = c(2016, 10), end = c(2018, 12), frequency = 11)
  plot.ts(farinha_tipo12016_ts)
  farinha_tipo12016_HW <- HoltWinters(farinha_tipo12016_ts, seasonal = c("additive"))
  plot(farinha_tipo12016_HW)
  plot(fitted(farinha_tipo12016_HW))
  farinha_tipo12016_forecast <- forecast:::forecast.HoltWinters(farinha_tipo12016_HW, h=11)
  forecast:::plot.forecast(farinha_tipo12016_forecast)
  plot.ts(farinha_tipo12016_forecast$residuals) #para estimar a qualidade do modelo
  
  
  
##Quantidade de integral em kg:
  farinha_integral <- read.csv("farinha_integral_kg.txt", header = F, sep = "\t", dec = ",")
  farinha_integral_ts <- ts(farinha_integral, start = c(2013, 2), end = c(2018, 12), frequency = 11)
  plot.ts(farinha_integral_ts)
  farinha_integral_HW <- HoltWinters(farinha_integral_ts, seasonal = c("additive"))
  plot(farinha_integral_HW)
  plot(fitted(farinha_integral_HW))
  farinha_integral_forecast <- forecast:::forecast.HoltWinters(farinha_integral_HW, h=11)
  forecast:::plot.forecast(farinha_integral_forecast)
  plot.ts(farinha_integral_forecast$residuals) #para estimar a qualidade do modelo

  
  