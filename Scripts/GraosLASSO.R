library(glmnet) 
library(dplyr)   
library(psych)   
library(mvtnorm)

#Leitura dos dados referentes aos grãos (todos juntos no mesmo dataset)
dados <- read.csv("graos_dataset.txt", header = T, sep = "\t", dec = ",")

#Usar para aplicar a regressão sobre os dados referentes a Castanha do Pará
y <- dados %>% select(castanha.do.pará.164) %>% as.matrix()
X <- dados %>% select(-castanha.do.pará.164) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a Nozes
y <- dados %>% select(nozes.526) %>% as.matrix()
X <- dados %>% select(-nozes.526) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a Reforçador
y <- dados %>% select(reforçador.155) %>% as.matrix()
X <- dados %>% select(-reforçador.155) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a Fubá
y <- dados %>% select(fubá.581) %>% as.matrix()
X <- dados %>% select(-fubá.581) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a Açucar Mascavo
y <- dados %>% select(açucar.375) %>% as.matrix()
X <- dados %>% select(-açucar.375) %>% as.matrix()


#--------------------------------------------------------------------------------------

set.seed(123)
# Validação cruzada de 10 etapas para selecionar o lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
# alpha = 1 para -> lasso regression
lasso_cv <- cv.glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot di resultado da validação cruzada, destacando o valor mínimo do erro médio quadrático
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
