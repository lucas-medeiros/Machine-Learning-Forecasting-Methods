#bibliotecas
require(graphics)
library(forecast)
library(TTR)

##define diret�rio correto: (modificar para a pasta que est�o os arquivos)
#arquivos .R devem estar na mesma pasta que todos os .txt
setwd("~/Facul/Inicia��o Cient�fica/C�digos R")
load("~/Facul/Inicia��o Cient�fica/C�digos R/.RData")


###Script gr�os


##Modelo gen�rico
  #leitura da base de dados: (valores em arquivo txt)
  dados <- read.csv("graos_castanha.txt", header = F, sep = "\t", dec = ",")
  dados <- read.csv("graos_nozes.txt", header = F, sep = "\t", dec = ",")
  dados <- read.csv("graos_reforcador.txt", header = F, sep = "\t", dec = ",")
  dados <- read.csv("graos_fuba.txt", header = F, sep = "\t", dec = ",")
  dados <- read.csv("graos_acucar.txt", header = F, sep = "\t", dec = ",")
  
  
  #formata��o da s�rie temporal, f = 11, pois dezembro + janeiro
  dados_ts <- ts(dados, start = c(2017, 1), end = c(2018, 11), frequency = 11)
  
  ##utilizar para refor�ador e a�ucar mascavo (possuem dados para dez 2018):
  dados_ts <- ts(dados, start = c(2017, 1), end = c(2018, 12), frequency = 11)
  
  #plot da s�rie temporal original
  plot.ts(dados_ts, ylab = "Valor")
  #aplica��o do m�todo de Holt-Winters aditivo
  dados_HW <- HoltWinters(dados_ts, seasonal = c("additive"))
  plot(dados_HW)
  #plot de cada componente da s�rie separadamente
  plot(fitted(dados_HW))
  #biblioteca utiliza para previs�o com base no m�todo de Holt-Winters
  dados_forecast <- forecast:::forecast.HoltWinters(dados_HW, h=11)
  forecast:::plot.forecast(dados_forecast)
  plot.ts(dados_forecast$residuals) #para estimar a qualidade do modelo
  