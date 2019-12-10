# ELM com otimizaçao multiobjetivo
# Projetos de PIBIC 

# (usar JADE E SADE no lugar das tecnicas de otimizaçao DE, PSO, COA, ...)

# Orientadora: Viviana Cocco Mariani

#comando pra limpar a memoria
rm(list=ls(all=TRUE)) 
require(graphics)


#pasta de trabalho IMPORTANTE ALTERAR DE ACORDO ONDE ESTÁ os codigos que serao usados

setwd("~/Facul/Iniciação Científica/Códigos R")

#suprime warning mensages
options(warn=-1)


#lendo funções auxiliares
source("COA.R")

#verifica se bibliotecas estão instaladas
requiredPackages<-c("tcltk","graphics","forecast","TTR","graphics","ggplot2",
            "dplyr","psych","metaheuristicOpt","pso","psoptim","tidyr",
            "mvtnorm","excelR","elmNNRcpp","stringr","DEoptim","kernlab")

#carrega os pacotes
for (p in requiredPackages){
  if(!require(p,character.only = TRUE)) 
    install.packages(p)
  library(p,character.only = TRUE)
}

#leitura da base de dados: (valores em arquivo txt)
dados <- read.csv("dataset_wind_.txt", header = T, sep = "\t", dec = ",")


#IMPORTANTE escolher os dados de acordo com o que será entrada e saída no arquivo dataset_wind.txt
input  <- dados %>% select(-Power) %>% as.matrix()
output <- dados %>% select(Power) %>% as.matrix()

##########

#dataset_gas_natural_MATRIX <- read_excel("~/Facul/Iniciação Científica/Códigos R/dataset_gas_natural_MATRIX.xlsx")
#valores de 2019 são descartados, pois o método falha quando há espaços da matriz marcados como NA
#dados <- dataset_gas_natural_MATRIX %>% select(-"2019")

#input  <- dados %>% select(-"2018") %>% as.matrix()
#output <- dados %>% select("2018") %>% as.matrix()



#dados <- read.csv("dataset_energia eletrica_brasil.txt", header = T, sep = "\t", dec = ",")

#output <- dados %>% select(Total) %>% as.matrix()
#input  <- dados %>% select(-Total) %>% as.matrix()


typeof(input)

problem <- 1

#divide dataset em duas partes - p% dos dados <- treinamento e (1-p)% <- teste
p       <- 0.7
tam     <- round(nrow(output)*p)                      #retorna número inteiro que representa 70% dos valores disponíveis

if (problem == 1) {   # regressão
  cat("Problema de regressão\n\n")
  input_tr <- input[1:tam, ]                            #seleciona parte de treinamento
  input_te <- input[(tam + 1):nrow(input), ]            #seleciona parte de teste

  output_tr <- matrix(output[1:tam, ], ncol = 1)               #seleciona parte de treinamento de y
  output_te <- matrix(output[(tam + 1):nrow(output), ], ncol = 1)    #seleciona parte de teste de y
}  
  

if (problem == 2) {   #série temporal
  
  
  cat("Problema de previsão de séries temporais\n\n")
  
  atraso       <- 1   # atraso no sinal de entrada do modelo
  
  # define o tipo e tamanho das matrizes
  input_tr   <- matrix(nrow=tam-atraso                         , ncol=dim(input)[2])
  output_tr  <- matrix(nrow=tam-atraso                         , ncol=ncol(output)) 
  
  input_te   <- matrix(nrow=dim(input)[1]-atraso-tam           , ncol=dim(input)[2])
  output_te  <- matrix(nrow=dim(output)[1]-atraso-tam          , ncol=ncol(output)) 
  
  #atribui os valores
  input_tr   <- matrix( input [(atraso+1):tam, ]               , ncol = ncol(input) )
  output_tr  <- matrix( output[(atraso+1):tam, ]               , ncol = ncol(output) )
  
  input_te   <- matrix( input [(atraso+tam):nrow(input)-1, ]   , ncol = ncol(input) )
  output_te  <- matrix( output[(atraso+tam):nrow(input), ]     , ncol = ncol(output) )
  
  cat("\nDimensão do sinal de entrada (treinamento) : ", dim(input_tr))
  cat("\nDimensão do sinal de saída  (teste)        : ", dim(input_te))
  

}  

# Definicoes para os otimizadores
# lb - (l - lower) limite inferior das variaveis de projeto
# ub - (u - upper) limite superior das variaveis de projeto
# [no. de neuronios na camada oculta (de 10 a 50), tipo da funcao de ativacao, tipo de inicialização, bias (polarizacao)]

lb        <- c(10,1,1,1)
ub        <- c(100,10,3,2)
numVar    <- length(lb)   # numero de variaveis de projeto da rede neural, que serão otimizadas

rangeVar  <- matrix(c(lb,ub), byrow=TRUE,nrow=2)
Iter      <- 250 # numero maximo de iteracoes do metodo de otimizacao
Pop       <- length(lb)*10  # numero da população do metodo de otimizacao - 4*10


#RESULTADOS DA OTIMIZAÇÃO
# Params armazena as variáveis de projeto de otimização, 4 variáveis que deram o menor
# valor para a funcao objetivo

Params           <- matrix(nrow=6,ncol=length(lb),byrow=TRUE)
colnames(Params) <- paste("Params_",1:length(lb),sep="")
rownames(Params) <- c("DE","GWO","PSO","DA","MFO","COA")

#------------------------------------------------------------------------
# DE - DIFFERENTIAL EVOLUTION
#------------------------------------------------------------------------
cat("DE optimization starts! \n")
source('elm_obj.R')

set.seed(1234)
elm_de_opt       <- DEoptim(fn = elm_obj, lower = lb, upper = ub,
                            control = DEoptim.control(reltol=1e-3,steptol =10,itermax = Iter))

fitted_params_DE <- elm_de_opt$optim$bestmem
best_obj_DE      <- elm_de_opt$optim$bestval
Params[1,]       <- floor(fitted_params_DE)

cat('Parameters for DE         :',Params[1,],'\n')
cat('Objective function for DE :',best_obj_DE,'\n')

source('elm_graphics.R')

aux  <- elm_graphics(Params[1,],1)    # train
aux  <- elm_graphics(Params[1,],2)    # test



#------------ ATÉ AQUI OK


#------------------------------------------------------------------------
# GWO - GREY WOLF OPTIMIZER
#------------------------------------------------------------------------
# param 1 FUN: an objective function or cost function,
# param 2 optimType: "MIN" and "MAX". The default value is "MIN"
# param 3 numVar: a positive integer to determine the number variables.
# param 4 numPopulation: a positive integer to determine the number populations. The default value is 40.
# param 5 maxIter: a positive integer to determine the maximum number of iterations. The default value is 500.
# param 6 rangeVar: a matrix (2 Ã— n) containing the range of variables

cat("GWO optimization starts! \n")
Params[2,]<-GWO(elm_obj, optimType = "MIN", numVar, numPopulation = Pop, 
                 maxIter = Iter,  rangeVar)
floor(Params[2,])
elm_obj(Params[2,])
cat('Parameters for GWO:',Params[2,],'\n')


#----------------------------------------------------------------
# PSO - PARTICLE SWARM OPTIMIZATION
#----------------------------------------------------------------
Vmax <- 2;  ci <- 1.5;  cg <- 1.5;  w <- 0.9   #parâmetros do PSO
cat("PSO optimization starts! \n")
Params[3,]<-PSO(elm_obj, optimType="MIN", numVar, numPopulation=Pop,
                 maxIter=Iter, rangeVar, Vmax, ci, cg, w)
floor(Params[3,])
elm_obj(Params[3,])
cat('Parameters for PSO:',Params[3,],'\n')


#-----------------------------------------------------------------
# DA - DRAGONFLY ALGORITHM
#-----------------------------------------------------------------
cat("DA optimization starts! \n")
Params[4,] <- DA(elm_obj, optimType = "MIN", numVar, numPopulation = Pop, 
                maxIter = Iter,  rangeVar)
floor(Params[4,])
elm_obj(Params[4,])
cat('Parameters for DA:',Params[4,],'\n')


#------------------------------------------------------------------
# MFO - MOTH-FLAME OPTIMIZATION
#------------------------------------------------------------------
cat("MFO optimization starts! \n")
Params[5,] <- MFO(elm_obj, optimType = "MIN", numVar, numPopulation = Pop, 
                  maxIter = Iter,  rangeVar)
floor(Params[5,])
elm_obj(Params[5,])
cat('Parameters for MFO:',Params[5,],'\n')


#------------------------------------------------------------------
# COA - COYOTE OPTIMIZATION ALGORITHM
#------------------------------------------------------------------
cat("COA optimization starts! \n")

# Objective function parameters
D     <- length(ub)
lu    <- matrix(lb,2,D,byrow=TRUE)
lu[2,]<- ub

# -- COA parameters
n_packs = 5
n_coy = 10
nfevalMAX = n_packs*n_coy*3

# -- Run the experiments
n_exper = 1; 	 # Number of experiments
y = matrix(0,1,n_exper)
t <- Sys.time()
for (i in 1:n_exper){
  otm_COA = COA(elm_obj,lu,nfevalMAX,n_packs,n_coy)
  print(Sys.time() - t)
  y[1,i] = otm_COA$globalMin
  t <- Sys.time()
}
Params[6,]<-otm_COA$globalParams
cat('Parameters for COA:',Params[6,],'\n')
# -- Show the statistics
v = c(min(y), mean(y), median(y), max(y), sd(y))
print('Statistics (min., avg., median, max., std.)')
print(v)
cat('Optimization process done! \n')


###############################Save Results###################################
form2   <-paste("Results_Parameters_wind1_",Sys.Date(),".RData", sep='')
save(Params,file=form2)

############################################################################
Results<-Params


