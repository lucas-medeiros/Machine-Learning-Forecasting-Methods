library(glmnet) 
library(dplyr)   
library(psych)   
library(mvtnorm)

#Leitura dos dados referentes aos hortifruti (quantidade)
dados_qnt <- read.csv("horti_qnt_dataset.txt", header = T, sep = "\t", dec = ",")

#Usar para aplicar a regress�o sobre os dados referentes a quantidade de Laranja
y <- dados_qnt %>% select(Laranja) %>% as.matrix()
X <- dados_qnt %>% select(-Laranja) %>% as.matrix()

#Usar para aplicar a regress�o sobre os dados referentes a quantidade de Banana
y <- dados_qnt %>% select(Banana) %>% as.matrix()
X <- dados_qnt %>% select(-Banana) %>% as.matrix()

#Usar para aplicar a regress�o sobre os dados referentes a quantidade de Cenoura
y <- dados_qnt %>% select(Cenoura) %>% as.matrix()
X <- dados_qnt %>% select(-Cenoura) %>% as.matrix()

#Usar para aplicar a regress�o sobre os dados referentes a quantidade de ma��
y <- dados_qnt %>% select(ma��) %>% as.matrix()
X <- dados_qnt %>% select(-ma��) %>% as.matrix()

#Usar para aplicar a regress�o sobre os dados referentes a quantidade de Tomate Salada
y <- dados_qnt %>% select(tomate.salada) %>% as.matrix()
X <- dados_qnt %>% select(-tomate.salada) %>% as.matrix()

#--------------------------------------------------------------------------------------

#Leitura dos dados referentes aos hortifruti (pre�o)
dados_preco <- read.csv("horti_preco_dataset.txt", header = T, sep = "\t", dec = ",")

#Usar para aplicar a regress�o sobre os dados referentes a quantidade de Cogumelo Paris
y <- dados_preco %>% select(cogumelo.paris) %>% as.matrix()
X <- dados_preco %>% select(-cogumelo.paris) %>% as.matrix()

#Usar para aplicar a regress�o sobre os dados referentes a quantidade de Tomate Cereja
y <- dados_preco %>% select(tomate.cereja) %>% as.matrix()
X <- dados_preco %>% select(-tomate.cereja) %>% as.matrix()

#Usar para aplicar a regress�o sobre os dados referentes a quantidade de Tomate Salada
y <- dados_preco %>% select(tomate.salada) %>% as.matrix()
X <- dados_preco %>% select(-tomate.salada) %>% as.matrix()

#Usar para aplicar a regress�o sobre os dados referentes a quantidade de Ma�� nacional
y <- dados_preco %>% select(ma��.nacional) %>% as.matrix()
X <- dados_preco %>% select(-ma��.nacional) %>% as.matrix()

#Usar para aplicar a regress�o sobre os dados referentes a quantidade de Tomate Salada
y <- dados_preco %>% select(maracuj�) %>% as.matrix()
X <- dados_preco %>% select(-maracuj�) %>% as.matrix()


#--------------------------------------------------------------------------------------

set.seed(123)
# Valida��o cruzada de 10 etapas para selecionar o lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
# alpha = 1 para -> lasso regression
lasso_cv <- cv.glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot di resultado da valida��o cruzada, destacando o valor m�nimo do erro m�dio quadr�tico
plot(lasso_cv)

# seleciona o melhor lambda (menor)
lambda_cv <- lasso_cv$lambda.min
# Realiza o modelo final utilizando o melhor lambda
model_cv <- glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, X) #previs�o com base no modelo final gerado
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv) #res�duos (para avaliar a qualidade do modelo)
rsq_lasso_cv <- cor(y, y_hat_cv)^2


#Realiza o plot dos coeficientes para cada vari�vel, com legenda
#O coeficientes dimunuem a medida que o lambda aumenta
res <- glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7)

legend("topright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7) #usar se a legenda ficar na frente
