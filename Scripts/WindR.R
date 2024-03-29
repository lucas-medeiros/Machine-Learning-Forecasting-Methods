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

#N�o � necess�rio carregar dados do .RData
#load("~/Facul/Inicia��o Cient�fica/C�digos R/.RData")

#-----------------------------------------------------------------------------------

# M�todo de HoltWinters

##Modelo gen�rico
#leitura da base de dados: (valores em arquivo txt)
dados <- read.csv("dataset_wind_.txt", header = T, sep = "\t", dec = ",")

#operador $ � utilizado para acessar colunas espec�ficas de uma matrix

power <- dados$Power	
gen_temp1 <- dados$Generator_Temp1	
gen_temp2 <- dados$Generator_Temp2	
gen_speed <- dados$Generator_Speed	
wind_speed <- dados$Wind_Speed	
windir <- dados$WindDir	
nacelle <- dados$Nacelle	
temperature <- dados$Temperature

#formata��o da s�rie temporal
#frequencia = 10 (observa��es a cada 10 segundos) ?

power_ts <- ts(power, frequency = 10)
gen_temp1_ts <- ts(power, frequency = 10)
gen_temp2_ts <- ts(gen_temp2, frequency = 10)
gen_speed_ts <- ts(gen_speed, frequency = 10)
wind_speed_ts <- ts(wind_speed, frequency = 10)
windir_ts <- ts(windir, frequency = 10)
nacelle_ts <- ts(nacelle, frequency = 10)
temperature_ts <- ts(temperature, frequency = 10)

#para facilitar a generaliza��o de c�digo
dados_ts <- power_ts
dados_ts <- gen_temp1_ts
dados_ts <- gen_temp2_ts
dados_ts <- gen_speed_ts
dados_ts <- wind_speed_ts
dados_ts <- windir_ts
dados_ts <- nacelle_ts
dados_ts <- temperature_ts


#plot da s�rie temporal original
plot.ts(power_ts, ylab = "Energia (kW)", xlab = "Tempo (10s)")
plot.ts(gen_temp1_ts, ylab = "Temperatura do rolamento do gerador 1 (�C)", xlab = "Tempo (10s)")
plot.ts(gen_temp2_ts, ylab = "Temperatura do rolamento do gerador 2 (�C)", xlab = "Tempo (10s)")
plot.ts(gen_speed_ts, ylab = "Velocidade de giro do gerador (RPM)", xlab = "Tempo (10s)")
plot.ts(wind_speed_ts, ylab = "Velocidade do vento (m/s)", xlab = "Tempo (10s)")
plot.ts(windir_ts, ylab = "DIre��o do vento (�)", xlab = "Tempo (10s)")
plot.ts(nacelle_ts, ylab = "Nacelle dire��o (�)", xlab = "Tempo (10s)")
plot.ts(temperature_ts, ylab = "Temperatura ambiente (�C)", xlab = "Tempo (10s)")


#aplica��o do m�todo de Holt-Winters aditivo
dados_HW <- HoltWinters(dados_ts, seasonal = c("additive"))
plot(dados_HW)
#plot de cada componente da s�rie separadamente
plot(fitted(dados_HW))
#biblioteca utiliza para previs�o com base no m�todo de Holt-Winters
dados_forecast <- forecast:::forecast.HoltWinters(dados_HW, h=100)
forecast:::plot.forecast(dados_forecast)
plot.ts(dados_forecast$residuals) #para estimar a qualidade do modelo



#--------------------------------------------------------------------------------------

# LASSO

#sele��o dos dados: seleciona uma coluna em y e as demais para x
y <- dados %>% select(Power) %>% as.matrix()
X <- dados %>% select(-Power) %>% as.matrix()

y <- dados %>% select(Generator_Temp1) %>% as.matrix()
X <- dados %>% select(-Generator_Temp1) %>% as.matrix()

y <- dados %>% select(Generator_Temp2) %>% as.matrix()
X <- dados %>% select(-Generator_Temp2) %>% as.matrix()

y <- dados %>% select(Generator_Speed) %>% as.matrix()
X <- dados %>% select(-Generator_Speed) %>% as.matrix()

y <- dados %>% select(Wind_Speed) %>% as.matrix()
X <- dados %>% select(-Wind_Speed) %>% as.matrix()

y <- dados %>% select(WindDir) %>% as.matrix()
X <- dados %>% select(-WindDir) %>% as.matrix()

y <- dados %>% select(Nacelle) %>% as.matrix()
X <- dados %>% select(-Nacelle) %>% as.matrix()

y <- dados %>% select(Temperature) %>% as.matrix()
X <- dados %>% select(-Temperature) %>% as.matrix()



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

#leitura da base de dados: (valores em arquivo txt)
dados <- read.csv("dataset_wind_.txt", header = T, sep = "\t", dec = ",")

#-----------
# Regression
#-----------


y <- dados %>% select(Power) %>% as.matrix()
x <- dados %>% select(-Power) %>% as.matrix()

y <- dados %>% select(Generator_Temp1) %>% as.matrix()
x <- dados %>% select(-Generator_Temp1) %>% as.matrix()

y <- dados %>% select(Generator_Temp2) %>% as.matrix()
x <- dados %>% select(-Generator_Temp2) %>% as.matrix()

y <- dados %>% select(Generator_Speed) %>% as.matrix()
x <- dados %>% select(-Generator_Speed) %>% as.matrix()

y <- dados %>% select(Wind_Speed) %>% as.matrix()
x <- dados %>% select(-Wind_Speed) %>% as.matrix()

y <- dados %>% select(WindDir) %>% as.matrix()
x <- dados %>% select(-WindDir) %>% as.matrix()

y <- dados %>% select(Nacelle) %>% as.matrix()
x <- dados %>% select(-Nacelle) %>% as.matrix()

y <- dados %>% select(Temperature) %>% as.matrix()
x <- dados %>% select(-Temperature) %>% as.matrix()


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


