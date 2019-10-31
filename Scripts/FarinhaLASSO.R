library(glmnet) 
library(dplyr)   
library(psych)   
library(mvtnorm)

#Leitura dos dados referentes a Farinha (todos juntos no mesmo dataset)
dados <- read.csv("farinha_dataset.txt", header = T, sep = "\t", dec = ",")

#Usar para aplicar a regressão sobre os dados referentes a quantidade total em sacos
y <- dados %>% select(total.sacos) %>% as.matrix()
X <- dados %>% select(-total.sacos) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade total em kg
y <- dados %>% select(total.kg) %>% as.matrix()
X <- dados %>% select(-total.kg) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de farinha premium em kg
y <- dados %>% select(premium.kg) %>% as.matrix()
X <- dados %>% select(-premium.kg) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de farinha integral em kg
y <- dados %>% select(integral.kg) %>% as.matrix()
X <- dados %>% select(-integral.kg) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de farinha tipo 1 em kg 
y <- dados %>% select(tipo.1.kg) %>% as.matrix()
X <- dados %>% select(-tipo.1.kg) %>% as.matrix()


#--------------------------------------------------------------------------------------

set.seed(123)
# Validação cruzada de 10 etapas para selecionar o lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
# alpha = 1 para -> lasso regression
lasso_cv <- cv.glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot di resultado da validação cruzada, destacando o valor mínimo do erro médio quadrático
plot(lasso_cv)

## em alguns casos o primeiro valor de lambda ja é o q fornece o menor erro

# seleciona o melhor lambda (menor)
lambda_cv <- lasso_cv$lambda.min
# Realiza o modelo final utilizando o melhor lambda
model_cv <- glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, X) #previsão com base no modelo final gerado
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv) #resíduos (para avaliar a qualidade do modelo)
rsq_lasso_cv <- cor(y, y_hat_cv)^2


#Realiza o plot dos coeficientes para cada variável, com legenda
#O coeficientes dimunuem a medida que o lambda aumenta
res <- glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7)

legend("topright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7) #usar se a legenda ficar na frente
