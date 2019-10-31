library(elmNNRcpp)


##define diretório correto: (modificar para a pasta que estão os arquivos)
#arquivos .R devem estar na mesma pasta que todos os .txt
setwd("~/Facul/Iniciação Científica/Códigos R")

#leitura da base de dados: (valores em arquivo txt)
dados <- read.csv("dataset_wind_.txt", header = T, sep = "\t", dec = ",")

#-----------
# Regression
#-----------


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


out_regr = elm_train(X, y, nhid = 20, actfun = 'purelin', init_weights = 'uniform_negative')

plot(out_regr$fitted_values)
plot(out_regr$predictions)
plot(out_regr$residuals)


