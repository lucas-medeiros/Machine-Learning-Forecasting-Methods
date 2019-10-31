#bibliotecas
require(graphics)
library(forecast)
library(TTR)
library(glmnet) 
library(dplyr)   
library(psych)   
library(mvtnorm)
library(excelR)
library(elmNNRcpp)
library(readxl)


##define diretório correto: (modificar para a pasta que estão os arquivos)
#arquivos .R devem estar na mesma pasta que todos os .txt
setwd("~/Facul/Iniciação Científica/Códigos R")
#load("~/Facul/Iniciação Científica/Códigos R/.RData")

#-----------------------------------------------------------------------------------

# Método de HoltWinters

##Modelo genérico
#leitura da base de dados: (valores em arquivo txt)
#N/A data 
dados <- read.csv("dataset_solar_plant.txt", header = T, sep = "\t", dec = ",")

main_meter <- dados$Meter
temperature <- dados$Temperature
radiation <- dados$Radiation

#formatação da série temporal
#frequencia = 10 min ?

main_meter_ts <- ts(main_meter, frequency = 10)
temperature_ts <- ts(temperature, frequency = 10)
radiation_ts <- ts(radiation, frequency = 10)

dados_ts <- main_meter_ts
dados_ts <- temperature_ts
dados_ts <- radiation_ts

#plot da série temporal original
plot.ts(main_meter_ts, ylab = "Medidor principal (total de energia produzida em kWh)")
plot.ts(temperature_ts, ylab = "Temperatura ambiente (ºC)")
plot.ts(radiation_ts, ylab = "Radiação solar em plano inclinado (W/m2)")

#aplicação do método de Holt-Winters aditivo
dados_HW <- HoltWinters(dados_ts, seasonal = c("additive"))
plot(dados_HW)
#plot de cada componente da série separadamente
plot(fitted(dados_HW))
#biblioteca utiliza para previsão com base no método de Holt-Winters
dados_forecast <- forecast:::forecast.HoltWinters(dados_HW, h=1500)
forecast:::plot.forecast(dados_forecast)
plot.ts(dados_forecast$residuals) #para estimar a qualidade do modelo



#--------------------------------------------------------------------------------------

# LASSO

#seleção dos dados: seleciona uma coluna em y e as demais para x

y <- dados %>% select(Meter) %>% as.matrix()
X <- dados %>% select(-Meter) %>% as.matrix()

y <- dados %>% select(Temperature) %>% as.matrix()
X <- dados %>% select(-Temperature) %>% as.matrix()

y <- dados %>% select(Radiation) %>% as.matrix()
X <- dados %>% select(-Radiation) %>% as.matrix()



set.seed(123)
# Validação cruzada de 10 etapas para selecionar o lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
# alpha = 1 para -> lasso regression


## erro na função cv.glmnet de missing value, por ter poucas colunas
lasso_cv <- cv.glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot do resultado da validação cruzada, destacando o valor mínimo do erro médio quadrático
plot(lasso_cv)

# seleciona o melhor lambda (menor)
lambda_cv <- lasso_cv$lambda.min
# Realiza o modelo final utilizando o melhor lambda
model_cv <- glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, X) #previsão com base no modelo final gerado
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv) #resíduos (para avaliar a qualidade do modelo)
rsq_lasso_cv <- cor(y, y_hat_cv)^2

##warning: correção entre y e y_hat_cv da 0 em alguns casos
## e o vetor de previsão y_hat_cv mantém um valor constante


#Realiza o plot dos coeficientes para cada variável, com legenda
#O coeficientes dimunuem a medida que o lambda aumenta
res <- glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")

legend("bottomright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7)
legend("topright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7) #usar se a legenda ficar na frente
legend("topleft", lwd = 1, col = 1:6, legend = colnames(X), cex = .7) #usar se a legenda ficar na frente


#--------------------------------------------------------------------------------------

#ELM

#leitura da base de dados: (valores em arquivo txt)
dados <- read.csv("dataset_solar_plant.txt", header = T, sep = "\t", dec = ",")

#-----------
# Regression
#-----------

y <- dados %>% select(Meter) %>% as.matrix()
X <- dados %>% select(-Meter) %>% as.matrix()

y <- dados %>% select(Temperature) %>% as.matrix()
X <- dados %>% select(-Temperature) %>% as.matrix()

y <- dados %>% select(Radiation) %>% as.matrix()
X <- dados %>% select(-Radiation) %>% as.matrix()

#N/A data error expected double
out_regr = elm_train(X, y, nhid = 20, actfun = 'purelin', init_weights = 'uniform_negative')

plot(out_regr$fitted_values)
plot(out_regr$predictions)
plot(out_regr$residuals)



