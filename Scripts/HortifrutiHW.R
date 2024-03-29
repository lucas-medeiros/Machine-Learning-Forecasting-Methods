#bibliotecas
require(graphics)
library(forecast)
library(TTR)

##define diret�rio correto: (modificar para a pasta que est�o os arquivos)
#arquivos .R devem estar na mesma pasta que todos os .txt
setwd("~/Facul/Inicia��o Cient�fica/C�digos R")
load("~/Facul/Inicia��o Cient�fica/C�digos R/.RData")


###Script Hortifruti


##Modelo gen�rico
#leitura da base de dados: (valores em arquivo txt)
dados <- read.csv("horti_total.txt", header = F, sep = "\t", dec = ",")
dados <- read.csv("horti_laranja_qnt.txt", header = F, sep = "\t", dec = ",")
dados <- read.csv("horti_banana_qnt.txt", header = F, sep = "\t", dec = ",")
dados <- read.csv("horti_cenoura_qnt.txt", header = F, sep = "\t", dec = ",")
dados <- read.csv("horti_maca_qnt.txt", header = F, sep = "\t", dec = ",")
dados <- read.csv("horti_tomatesalada_qnt.txt", header = F, sep = "\t", dec = ",")
dados <- read.csv("horti_cogumelo_preco.txt", header = F, sep = "\t", dec = ",")
dados <- read.csv("horti_tomatecereja_preco.txt", header = F, sep = "\t", dec = ",")
dados <- read.csv("horti_tomatesalada_preco.txt", header = F, sep = "\t", dec = ",")
dados <- read.csv("horti_maca_preco.txt", header = F, sep = "\t", dec = ",")
dados <- read.csv("horti_maracuja_preco.txt", header = F, sep = "\t", dec = ",")



#formata��o da s�rie temporal, f = 11, pois dezembro + janeiro
dados_ts <- ts(dados, start = c(2016, 1), end = c(2018, 11), frequency = 11)

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





##utilizar para morango, apenas dados de 2018 dispon�veis
dados <- read.csv("horti_morango_qnt.txt", header = F, sep = "\t", dec = ",")
dados <- read.csv("horti_morango_preco.txt", header = F, sep = "\t", dec = ",")

dados_ts <- ts(dados, start = c(2018, 2), end = c(2018, 11), frequency = 11)

#plot da s�rie temporal original
plot.ts(dados_ts, ylab = "Valor")
#aplica��o do m�todo de Holt-Winters aditivo
dados_HW <- HoltWinters(dados_ts, seasonal = c("additive"), gamma = F)

#como a s�rie temporal s� tem 10 observa��es � necess�rio descartar a constante de sazonalidade
#para evitar o erro:
# Error in decompose(ts(x[1L:wind], start = start(x), frequency = f), seasonal) : 
#  s�rie temporal n�o tem per�odo, ou tem menos de 2

plot(dados_HW)
#plot de cada componente da s�rie separadamente
plot(fitted(dados_HW))
#biblioteca utiliza para previs�o com base no m�todo de Holt-Winters
dados_forecast <- forecast:::forecast.HoltWinters(dados_HW, h=5)
forecast:::plot.forecast(dados_forecast)
plot.ts(dados_forecast$residuals) #para estimar a qualidade do modelo
