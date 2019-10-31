#bibliotecas
require(graphics)
library(forecast)
library(TTR)

##define diretório correto: (modificar para a pasta que estão os arquivos)
#arquivos .R devem estar na mesma pasta que todos os .txt
setwd("~/Facul/Iniciação Científica/Códigos R")
load("~/Facul/Iniciação Científica/Códigos R/.RData")


###Script frios

##Valor total de frios:
  #leitura da base de dados: (valores em arquivo txt)
  frios_total <- read.csv("frios_total.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_total_ts <- ts(frios_total, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_total_ts, ylab = "Valor total")
  #aplicação do método de Holt-Winters aditivo
  frios_total_HW <- HoltWinters(frios_total_ts, seasonal = c("additive"))
  plot(frios_total_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_total_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_total_forecast <- forecast:::forecast.HoltWinters(frios_total_HW, h=11)
  forecast:::plot.forecast(frios_total_forecast)
  plot.ts(frios_total_forecast$residuals) #para estimar a qualidade do modelo
  
  
##Queijo mussarela quantidade (kg)
  #leitura da base de dados: (valores em arquivo txt)
  frios_mussarela_qnt <- read.csv("frios_mussarela_qnt.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_mussarela_qnt_ts <- ts(frios_mussarela_qnt, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_mussarela_qnt_ts, ylab = "Quantidade de queijo mussarela (kg)")
  #aplicação do método de Holt-Winters aditivo
  frios_mussarela_qnt_HW <- HoltWinters(frios_mussarela_qnt_ts, seasonal = c("additive"))
  plot(frios_mussarela_qnt_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_mussarela_qnt_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_mussarela_qnt_forecast <- forecast:::forecast.HoltWinters(frios_mussarela_qnt_HW, h=11)
  forecast:::plot.forecast(frios_mussarela_qnt_forecast)
  plot.ts(frios_mussarela_qnt_forecast$residuals) #para estimar a qualidade do modelo
  

##Queijo mussarela preço (R$)
  #leitura da base de dados: (valores em arquivo txt)
  frios_mussarela_preco <- read.csv("frios_mussarela_preco.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_mussarela_preco_ts <- ts(frios_mussarela_preco, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_mussarela_preco_ts, ylab = "Preço de queijo mussarela (R$)")
  #aplicação do método de Holt-Winters aditivo
  frios_mussarela_preco_HW <- HoltWinters(frios_mussarela_preco_ts, seasonal = c("additive"))
  plot(frios_mussarela_preco_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_mussarela_preco_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_mussarela_preco_forecast <- forecast:::forecast.HoltWinters(frios_mussarela_preco_HW, h=11)
  forecast:::plot.forecast(frios_mussarela_preco_forecast)
  plot.ts(frios_mussarela_preco_forecast$residuals) #para estimar a qualidade do modelo

  
##Peito de peru quantidade (kg)
  #leitura da base de dados: (valores em arquivo txt)
  frios_peru_qnt <- read.csv("frios_peru_qnt.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_peru_qnt_ts <- ts(frios_peru_qnt, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_peru_qnt_ts, ylab = "Quantidade de peito de peru (kg)")
  #aplicação do método de Holt-Winters aditivo
  frios_peru_qnt_HW <- HoltWinters(frios_peru_qnt_ts, seasonal = c("additive"))
  plot(frios_peru_qnt_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_peru_qnt_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_peru_qnt_forecast <- forecast:::forecast.HoltWinters(frios_peru_qnt_HW, h=11)
  forecast:::plot.forecast(frios_peru_qnt_forecast)
  plot.ts(frios_peru_qnt_forecast$residuals) #para estimar a qualidade do modelo

  
##Peito de peru preço (R$)
  #leitura da base de dados: (valores em arquivo txt)
  frios_peru_preco <- read.csv("frios_peru_preco.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_peru_preco_ts <- ts(frios_peru_preco, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_peru_preco_ts, ylab = "Preço do peito de peru (R$)")
  #aplicação do método de Holt-Winters aditivo
  frios_peru_preco_HW <- HoltWinters(frios_peru_preco_ts, seasonal = c("additive"))
  plot(frios_peru_preco_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_peru_preco_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_peru_preco_forecast <- forecast:::forecast.HoltWinters(frios_peru_preco_HW, h=11)
  forecast:::plot.forecast(frios_peru_preco_forecast)
  plot.ts(frios_peru_preco_forecast$residuals) #para estimar a qualidade do modelo

  
##Presunto quantidade (kg)
  #leitura da base de dados: (valores em arquivo txt)
  frios_presunto_qnt <- read.csv("frios_presunto_qnt.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_presunto_qnt_ts <- ts(frios_presunto_qnt, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_presunto_qnt_ts, ylab = "Quantidade de presunto (kg)")
  #aplicação do método de Holt-Winters aditivo
  frios_presunto_qnt_HW <- HoltWinters(frios_presunto_qnt_ts, seasonal = c("additive"))
  plot(frios_presunto_qnt_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_presunto_qnt_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_presunto_qnt_forecast <- forecast:::forecast.HoltWinters(frios_presunto_qnt_HW, h=11)
  forecast:::plot.forecast(frios_presunto_qnt_forecast)
  plot.ts(frios_presunto_qnt_forecast$residuals) #para estimar a qualidade do modelo
  
  
##Presunto preço (R$)
  #leitura da base de dados: (valores em arquivo txt)
  frios_presunto_preco <- read.csv("frios_presunto_preco.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_presunto_preco_ts <- ts(frios_presunto_preco, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_presunto_preco_ts, ylab = "Preço do presunto (R$)")
  #aplicação do método de Holt-Winters aditivo
  frios_presunto_preco_HW <- HoltWinters(frios_presunto_preco_ts, seasonal = c("additive"))
  plot(frios_presunto_preco_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_presunto_preco_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_presunto_preco_forecast <- forecast:::forecast.HoltWinters(frios_presunto_preco_HW, h=11)
  forecast:::plot.forecast(frios_presunto_preco_forecast)
  plot.ts(frios_presunto_preco_forecast$residuals) #para estimar a qualidade do modelo
  
  
##Mortadela quantidade (kg)
  #leitura da base de dados: (valores em arquivo txt)
  frios_mortadela_qnt <- read.csv("frios_mortadela_qnt.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_mortadela_qnt_ts <- ts(frios_mortadela_qnt, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_mortadela_qnt_ts, ylab = "Quantidade de mortadela (kg)")
  #aplicação do método de Holt-Winters aditivo
  frios_mortadela_qnt_HW <- HoltWinters(frios_mortadela_qnt_ts, seasonal = c("additive"))
  plot(frios_mortadela_qnt_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_mortadela_qnt_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_mortadela_qnt_forecast <- forecast:::forecast.HoltWinters(frios_mortadela_qnt_HW, h=11)
  forecast:::plot.forecast(frios_mortadela_qnt_forecast)
  plot.ts(frios_mortadela_qnt_forecast$residuals) #para estimar a qualidade do modelo

  
##Mortadela preço (R$)
  #leitura da base de dados: (valores em arquivo txt)
  frios_mortadela_preco <- read.csv("frios_mortadela_preco.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_mortadela_preco_ts <- ts(frios_mortadela_preco, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_mortadela_preco_ts, ylab = "Preço da presunto (R$)")
  #aplicação do método de Holt-Winters aditivo
  frios_mortadela_preco_HW <- HoltWinters(frios_mortadela_preco_ts, seasonal = c("additive"))
  plot(frios_mortadela_preco_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_mortadela_qnt_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_mortadela_preco_forecast <- forecast:::forecast.HoltWinters(frios_mortadela_preco_HW, h=11)
  forecast:::plot.forecast(frios_mortadela_preco_forecast)
  plot.ts(frios_mortadela_preco_forecast$residuals) #para estimar a qualidade do modelo
  
  
##Salsicha quantidade (kg)
  #leitura da base de dados: (valores em arquivo txt)
  frios_salsicha_qnt <- read.csv("frios_salsicha_qnt.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_salsicha_qnt_ts <- ts(frios_salsicha_qnt, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_salsicha_qnt_ts, ylab = "Quantidade de salsicha (kg)")
  #aplicação do método de Holt-Winters aditivo
  frios_salsicha_qnt_HW <- HoltWinters(frios_salsicha_qnt_ts, seasonal = c("additive"))
  plot(frios_salsicha_qnt_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_salsicha_qnt_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_salsicha_qnt_forecast <- forecast:::forecast.HoltWinters(frios_salsicha_qnt_HW, h=11)
  forecast:::plot.forecast(frios_salsicha_qnt_forecast)
  plot.ts(frios_salsicha_qnt_forecast$residuals) #para estimar a qualidade do modelo
  
  
##Salsicha preço (R$)
  #leitura da base de dados: (valores em arquivo txt)
  frios_salsicha_preco <- read.csv("frios_salsicha_preco.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_salsicha_preco_ts <- ts(frios_salsicha_preco, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_prato_preco_ts, ylab = "Preço da salsicha (R$)")
  #aplicação do método de Holt-Winters aditivo
  frios_salsicha_preco_HW <- HoltWinters(frios_salsicha_preco_ts, seasonal = c("additive"))
  plot(frios_salsicha_preco_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_salsicha_preco_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_salsicha_preco_forecast <- forecast:::forecast.HoltWinters(frios_salsicha_preco_HW, h=11)
  forecast:::plot.forecast(frios_salsicha_preco_forecast)
  plot.ts(frios_salsicha_preco_forecast$residuals) #para estimar a qualidade do modelo
  
  
##Manteiga quantidade (kg)
  #leitura da base de dados: (valores em arquivo txt)
  frios_manteiga_qnt <- read.csv("frios_manteiga_qnt.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_manteiga_qnt_ts <- ts(frios_manteiga_qnt, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_manteiga_qnt_ts, ylab = "Quantidade de manteiga (kg)")
  #aplicação do método de Holt-Winters aditivo
  frios_manteiga_qnt_HW <- HoltWinters(frios_manteiga_qnt_ts, seasonal = c("additive"))
  plot(frios_manteiga_qnt_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_manteiga_qnt_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_manteiga_qnt_forecast <- forecast:::forecast.HoltWinters(frios_manteiga_qnt_HW, h=11)
  forecast:::plot.forecast(frios_manteiga_qnt_forecast)
  plot.ts(frios_manteiga_qnt_forecast$residuals) #para estimar a qualidade do modelo
  
  
##Queijo prato
  #leitura da base de dados: (valores em arquivo txt)
  frios_prato_preco <- read.csv("frios_prato_preco.txt", header = F, sep = "\t", dec = ",")
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  frios_prato_preco_ts <- ts(frios_prato_preco, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(frios_prato_preco_ts, ylab = "Preço do queijo prato (R$)")
  #aplicação do método de Holt-Winters aditivo
  frios_prato_preco_HW <- HoltWinters(frios_prato_preco_ts, seasonal = c("additive"))
  plot(frios_prato_preco_HW)
  #plot de cada componente da série separadamente
  plot(fitted(frios_prato_preco_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  frios_prato_preco_forecast <- forecast:::forecast.HoltWinters(frios_prato_preco_HW, h=11)
  forecast:::plot.forecast(frios_prato_preco_forecast)
  plot.ts(frios_prato_preco_forecast$residuals) #para estimar a qualidade do modelo
  
  
  
##Modelo genérico
  #leitura da base de dados: (valores em arquivo txt)
  dados <- read.csv("", header = F, sep = "\t", dec = ",") #inserir nome do arquivo desejado
  
  #formatação da série temporal, f = 11, pois dezembro + janeiro
  dados_ts <- ts(dados, start = c(2015, 1), end = c(2018, 11), frequency = 11)
  #plot da série temporal original
  plot.ts(dados_ts, ylab = "Valor")
  #aplicação do método de Holt-Winters aditivo
  dados_HW <- HoltWinters(dados_ts, seasonal = c("additive"))
  plot(dados_HW)
  #plot de cada componente da série separadamente
  plot(fitted(dados_HW))
  #biblioteca utiliza para previsão com base no método de Holt-Winters
  dados_forecast <- forecast:::forecast.HoltWinters(dados_HW, h=11)
  forecast:::plot.forecast(dados_forecast)
  plot.ts(dados_forecast$residuals) #para estimar a qualidade do modelo
  
  