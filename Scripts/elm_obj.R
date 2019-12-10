elm_obj<-function(x)
{
  
  #funcoes de ativação da rede neural; sigmoide, senoide, função de base radial, degrau ...
  
  funcoes           <-c('sig','sin','radbas','hardlim','hardlims','satlins',
                        'tansig','tribas','relu','purelin')
  tipo_inicializacao<-c('normal_gaussian','uniform_positive','uniform_negative')
  valor_bias        <-c(TRUE, FALSE)
  
  nhid<-ifelse(floor(x[1])<=0,1,floor(x[1]))
  
  actfun<-ifelse(floor(x[2]) >= 10, funcoes[10],
                 ifelse(floor(x[2]) >= 9 ,funcoes[9],
                        ifelse(floor(x[2])>= 8 ,funcoes[8],
                               ifelse(floor(x[2]) >= 7 ,funcoes[7],
                                      ifelse(floor(x[2]) >= 6 ,funcoes[6],
                                             ifelse(floor(x[2]) >= 5 ,funcoes[5],
                                                    ifelse(floor(x[2]) >= 4 ,funcoes[4],
                                                           ifelse(floor(x[2]) >= 3 ,funcoes[3],
                                                                  ifelse(floor(x[2])>= 2 ,funcoes[2],funcoes[1]))))))))) 
  
  
  init_weights<-ifelse(floor(x[3])>=3,tipo_inicializacao[1],
                       ifelse(floor(x[3])>= 2 ,tipo_inicializacao[2],tipo_inicializacao[3])) 
  
  bias        <-ifelse(floor(x[4])>=2 ,valor_bias[2],valor_bias[1])
  
  #cat(nhid,actfun,init_weights,bias,'\n')
  out_regr <-elm_train(input_tr, output_tr, nhid, actfun, init_weights , bias, verbose = FALSE)
  #cat("ELM")
  pr_regr <-elm_predict(out_regr, input_tr)
  
  #RMSE(pr_regr,output)
  RMSE <- sqrt(mean((pr_regr-output_tr)^2))
}