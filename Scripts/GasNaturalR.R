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


##define diret�rio correto: (modificar para a pasta que est�o os arquivos)
#arquivos .R devem estar na mesma pasta que todos os .txt
setwd("~/Facul/Inicia��o Cient�fica/C�digos R")
#load("~/Facul/Inicia��o Cient�fica/C�digos R/.RData")

#-----------------------------------------------------------------------------------

# M�todo de HoltWinters

##Modelo gen�rico
#leitura da base de dados: (valores em arquivo txt)
dados <- read.csv("dataset_gas_natural.txt", header = F, sep = "\t", dec = ",")

dados_ts <- ts(dados, start = c(2000, 1), end = c(2019, 7), frequency = 12)


#plot da s�rie temporal original
plot.ts(dados_ts, ylab = "Produ��o de g�s natural (10^3 m3)", xlab = "Ano")
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
#SE DER ERRO USAR A OP��O do RStudio: 'Import Dataset" -> 'From Excel" -> escolher arquivo
dataset_gas_natural_MATRIX <- read_excel("~/Facul/Inicia��o Cient�fica/C�digos R/dataset_gas_natural_MATRIX.xlsx")
#valores de 2019 s�o descartados, pois o m�todo falha quando h� espa�os da matriz marcados como NA
dados <- dataset_gas_natural_MATRIX %>% select(-"2019")

#ALTERAR VALOR DE ANO (INSERIR VALOR ENTRE 0 E 18)
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
#SE DER ERRO USAR A OP��O do RStudio: 'Import Dataset" -> 'From Excel" -> escolher arquivo
dataset_gas_natural_MATRIX <- read_excel("~/Facul/Inicia��o Cient�fica/C�digos R/dataset_gas_natural_MATRIX.xlsx")
#valores de 2019 s�o descartados, pois o m�todo falha quando h� espa�os da matriz marcados como NA
dados <- dataset_gas_natural_MATRIX %>% select(-"2019")

#ALTERAR VALOR DE ANO (INSERIR VALOR ENTRE 0 E 18)
ano <- 18
num <- 2000 + ano
ano_string <- as.character(num)

#sele��o dos dados: seleciona uma coluna em y e as demais para x
y <- dados %>% select(ano_string) %>% as.matrix()
x <- dados %>% select(-ano_string) %>% as.matrix()


# Divide dataset em duas partes - 70% dos dados <- treinamento e 30% <- teste
#----------------------------------------

dados07 = round(nrow(y)*0.7)                          #retorna n�mero inteiro que representa 70% dos valores dispon�veis

xtr = x[1:dados07, ]                                  #seleciona parte de treinamento
xte = x[(dados07 + 1):nrow(x), ]                      #seleciona parte de teste

ytr = matrix(y[1:dados07, ], ncol = 1)                #seleciona parte de treinamento de y
yte = matrix(y[(dados07 + 1):nrow(y), ], ncol = 1)    #seleciona parte de teste de y


## perform a fit and predict [ elmNNRcpp ]
#----------------------------------------
#fun��o de ativa��o <- sigmoid
#k-folds <- 5 folds

fit_elm = elm_train(xtr, ytr, nhid = 5, actfun = 'sig', init_weights = "uniform_negative", bias = TRUE, verbose = T)

pr_te_elm = elm_predict(fit_elm, xte)


# evaluation metric
#----------------------------------------
rmse = function (y_true, y_pred) {                  #para o c�lculo dos res�duos <- erro m�dio quadr�tico
  out = sqrt(mean((y_true - y_pred)^2))
  out
}


# mean-squared-error for 'elm'
#--------------------------------------
cat('Erro m�dio quadr�tico para Extreme Machine Learning �: ', rmse(yte, pr_te_elm[, 1]), '\n')


