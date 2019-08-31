#bibliotecas
require(graphics)
library(forecast)
library(TTR)
library(glmnet) 
library(dplyr)   
library(psych)   
library(mvtnorm)


##define diretório correto: (modificar para a pasta que estão os arquivos)
#arquivos .R devem estar na mesma pasta que todos os .txt
setwd("~/Facul/Iniciação Científica/Códigos R")

#Não é necessário carregar dados do .RData
#load("~/Facul/Iniciação Científica/Códigos R/.RData")

#-----------------------------------------------------------------------------------

# Método de HoltWinters

##Modelo genérico
#leitura da base de dados: (valores em arquivo txt)
dados <- read.csv("dataset_wind_.txt", header = T, sep = "\t", dec = ",")

#operador $ é utilizado para acessar colunas específicas de uma matrix

power <- dados$Power	
gen_temp1 <- dados$Generator_Temp1	
gen_temp2 <- dados$Generator_Temp2	
gen_speed <- dados$Generator_Speed	
wind_speed <- dados$Wind_Speed	
windir <- dados$WindDir	
nacelle <- dados$Nacelle	
temperature <- dados$Temperature

#formatação da série temporal
#frequencia = 10 (observações a cada 10 segundos) ?

power_ts <- ts(power, frequency = 10)
gen_temp1_ts <- ts(power, frequency = 10)
gen_temp2_ts <- ts(gen_temp2, frequency = 10)
gen_speed_ts <- ts(gen_speed, frequency = 10)
wind_speed_ts <- ts(wind_speed, frequency = 10)
windir_ts <- ts(windir, frequency = 10)
nacelle_ts <- ts(nacelle, frequency = 10)
temperature_ts <- ts(temperature, frequency = 10)

#para facilitar a generalização de código
dados_ts <- power_ts
dados_ts <- gen_temp1_ts
dados_ts <- gen_temp2_ts
dados_ts <- gen_speed_ts
dados_ts <- wind_speed_ts
dados_ts <- windir_ts
dados_ts <- nacelle_ts
dados_ts <- temperature_ts


#plot da série temporal original
plot.ts(power_ts, ylab = "Energia (kW)", xlab = "Tempo (10s)")
plot.ts(gen_temp1_ts, ylab = "Temperatura do rolamento do gerador 1 (ºC)", xlab = "Tempo (10s)")
plot.ts(gen_temp2_ts, ylab = "Temperatura do rolamento do gerador 2 (ºC)", xlab = "Tempo (10s)")
plot.ts(gen_speed_ts, ylab = "Velocidade de giro do gerador (RPM)", xlab = "Tempo (10s)")
plot.ts(wind_speed_ts, ylab = "Velocidade do vento (m/s)", xlab = "Tempo (10s)")
plot.ts(windir_ts, ylab = "DIreção do vento (º)", xlab = "Tempo (10s)")
plot.ts(nacelle_ts, ylab = "Nacelle direção (º)", xlab = "Tempo (10s)")
plot.ts(temperature_ts, ylab = "Temperatura ambiente (ºC)", xlab = "Tempo (10s)")


#aplicação do método de Holt-Winters aditivo
dados_HW <- HoltWinters(dados_ts, seasonal = c("additive"))
plot(dados_HW)
#plot de cada componente da série separadamente
plot(fitted(dados_HW))
#biblioteca utiliza para previsão com base no método de Holt-Winters
dados_forecast <- forecast:::forecast.HoltWinters(dados_HW, h=100)
forecast:::plot.forecast(dados_forecast)
plot.ts(dados_forecast$residuals) #para estimar a qualidade do modelo



#--------------------------------------------------------------------------------------

# LASSO

#seleção dos dados: seleciona uma coluna em y e as demais para x
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
# Validação cruzada de 10 etapas para selecionar o lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
# alpha = 1 para -> lasso regression
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