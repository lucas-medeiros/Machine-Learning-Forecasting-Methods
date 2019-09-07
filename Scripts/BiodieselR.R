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


##define diret�rio correto: (modificar para a pasta que est�o os arquivos)
#arquivos .R devem estar na mesma pasta que todos os .txt
setwd("~/Facul/Inicia��o Cient�fica/C�digos R")
#load("~/Facul/Inicia��o Cient�fica/C�digos R/.RData")

#-----------------------------------------------------------------------------------

# M�todo de HoltWinters

##Modelo gen�rico
#leitura da base de dados: (valores em arquivo txt)
dados <- read.csv("dataset_biodiesel.txt", header = F, sep = "\t", dec = ",")

dados_ts <- ts(dados, start = c(2005, 1), end = c(2018, 12), frequency = 12)


#plot da s�rie temporal original
plot.ts(dados_ts, ylab = "Produ��o de Biodiesel", xlab = "Ano")
#aplica��o do m�todo de Holt-Winters aditivo
dados_HW <- HoltWinters(dados_ts, seasonal = c("additive"))
plot(dados_HW)
#plot de cada componente da s�rie separadamente
plot(fitted(dados_HW))
#biblioteca utiliza para previs�o com base no m�todo de Holt-Winters
dados_forecast <- forecast:::forecast.HoltWinters(dados_HW, h=12)
forecast:::plot.forecast(dados_forecast)
plot.ts(dados_forecast$residuals) #para estimar a qualidade do modelo



#--------------------------------------------------------------------------------------

# LASSO

##Modelo gen�rico
#leitura da base de dados importada de um arquivo xlsx para manter o formato de tabela
#alterar o caminho do arquivo para o diret�rio atual
#USAR A OP��O do RStudio: 'Import Dataset" -> 'From Excel" -> escolher arquivo
dataset_biodiesel_MATRIX <- read_excel("~/Facul/Inicia��o Cient�fica/C�digos R/dataset_biodiesel_MATRIX.xlsx")

#descarta a primeira coluna (nomes)
dados <- dataset_biodiesel_MATRIX %>% select(-1)

#ALTERAR VALOR DE ANO (INSERIR VALOR ENTRE 5 E 18)
ano <- 5
num <- 2000 + ano
ano_string <- as.character(num)

#sele��o dos dados: seleciona uma coluna em y e as demais para x
y <- dados %>% select(ano_string) %>% as.matrix()
X <- dados %>% select(-ano_string) %>% as.matrix()


set.seed(123)
# Valida��o cruzada de 10 etapas para selecionar o lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
# alpha = 1 para -> lasso regression
lasso_cv <- cv.glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot do resultado da valida��o cruzada, destacando o valor m�nimo do erro m�dio quadr�tico
plot(lasso_cv)

# seleciona o melhor lambda (menor)
lambda_cv <- lasso_cv$lambda.min
# Realiza o modelo final utilizando o melhor lambda
model_cv <- glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, X) #previs�o com base no modelo final gerado
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv) #res�duos (para avaliar a qualidade do modelo)
rsq_lasso_cv <- cor(y, y_hat_cv)^2

##warning: corre��o entre y e y_hat_cv da 0 em alguns casos
## e o vetor de previs�o y_hat_cv mant�m um valor constante


#Realiza o plot dos coeficientes para cada vari�vel, com legenda
#O coeficientes dimunuem a medida que o lambda aumenta
res <- glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")

legend("bottomright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7)
legend("topright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7) #usar se a legenda ficar na frente
legend("topleft", lwd = 1, col = 1:6, legend = colnames(X), cex = .7) #usar se a legenda ficar na frente


#--------------------------------------------------------------------------------------

#ELM

#leitura da base de dados importada de um arquivo xlsx para manter o formato de tabela
#alterar o caminho do arquivo para o diret�rio atual
#USAR A OP��O do RStudio: 'Import Dataset" -> 'From Excel" -> escolher arquivo
dataset_biodiesel_MATRIX <- read_excel("~/Facul/Inicia��o Cient�fica/C�digos R/dataset_biodiesel_MATRIX.xlsx")

#descarta a primeira coluna (nomes)
dados <- dataset_biodiesel_MATRIX %>% select(-1)

#ALTERAR VALOR DE ANO (INSERIR VALOR ENTRE 5 E 18)
ano <- 18
num <- 2000 + ano
ano_string <- as.character(num)

#sele��o dos dados: seleciona uma coluna em y e as demais para x
y <- dados %>% select(ano_string) %>% as.matrix()
X <- dados %>% select(-ano_string) %>% as.matrix()


out_regr = elm_train(X, y, nhid = 20, actfun = 'purelin', init_weights = 'uniform_negative')

plot(out_regr$fitted_values)
plot(out_regr$predictions)
plot(out_regr$residuals)
