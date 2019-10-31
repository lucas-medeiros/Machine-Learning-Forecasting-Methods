library(glmnet) 
library(dplyr)   
library(psych)   
library(mvtnorm)

#Leitura dos dados referentes aos frios (quantidade)
dados_qnt <- read.csv("frios_qnt_dataset.txt", header = T, sep = "\t", dec = ",")

#Usar para aplicar a regressão sobre os dados referentes a quantidade de Queijo mussarela
y <- dados_qnt %>% select(mussarela.129) %>% as.matrix()
X <- dados_qnt %>% select(-mussarela.129) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de Peito de peru defumado
y <- dados_qnt %>% select(peito.de.peru) %>% as.matrix()
X <- dados_qnt %>% select(-peito.de.peru) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de presunto
y <- dados_qnt %>% select(presunto) %>% as.matrix()
X <- dados_qnt %>% select(-presunto) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de mortadela
y <- dados_qnt %>% select(mortadela) %>% as.matrix()
X <- dados_qnt %>% select(-mortadela) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de salsicha
y <- dados_qnt %>% select(salsicha) %>% as.matrix()
X <- dados_qnt %>% select(-salsicha) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de manteiga
y <- dados_qnt %>% select(manteiga) %>% as.matrix()
X <- dados_qnt %>% select(-manteiga) %>% as.matrix()

#--------------------------------------------------------------------------------------

#Leitura dos dados referentes aos frios (preço)
dados_preco <- read.csv("frios_preco_dataset.txt", header = T, sep = "\t", dec = ",")

#Usar para aplicar a regressão sobre os dados referentes a quantidade de Queijo mussarela
y <- dados_preco %>% select(mussarela.129) %>% as.matrix()
X <- dados_preco %>% select(-mussarela.129) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de Peito de peru defumado
y <- dados_preco %>% select(peito.de.peru) %>% as.matrix()
X <- dados_preco %>% select(-peito.de.peru) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de presunto
y <- dados_preco %>% select(presunto) %>% as.matrix()
X <- dados_preco %>% select(-presunto) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de mortadela
y <- dados_preco %>% select(mortadela) %>% as.matrix()
X <- dados_preco %>% select(-mortadela) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de salsicha
y <- dados_preco %>% select(salsicha) %>% as.matrix()
X <- dados_preco %>% select(-salsicha) %>% as.matrix()

#Usar para aplicar a regressão sobre os dados referentes a quantidade de Queijo prato
y <- dados_preco %>% select(queijo.prato) %>% as.matrix()
X <- dados_preco %>% select(-queijo.prato) %>% as.matrix()


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


#Realiza o plot dos coeficientes para cada variável, com legenda
#O coeficientes dimunuem a medida que o lambda aumenta
res <- glmnet(X, y, family = "gaussian", alpha = 1, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7)

legend("topright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7) #usar se a legenda ficar na frente
