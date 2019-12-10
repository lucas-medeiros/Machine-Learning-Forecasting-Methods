elm_graphics<-function(x,flag)
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
  
  # train
  if (flag==1) {
    #cat(nhid,actfun,init_weights,bias,'\n')
    out_regr <-elm_train(input_tr, output_tr, nhid, actfun, init_weights , bias, verbose = FALSE)
    #cat("ELM")
    pr_regr <-elm_predict(out_regr, input_tr)
    
    #plot(pr_regr,output_tr, xlab="predicted",ylab="actual")
    #abline(a=0,b=1)
    

    # http://www.sthda.com/english/wiki/add-legends-to-plots-in-r-software-the-easiest-way
    d <- dim(output_tr)[1]
    t <- seq(1,d)
    
    plot(t,output_tr, xlab="time",ylab="output",type="l", col="red")
    par(mar = rep(2, 4))   # https://stackoverflow.com/questions/12766166/error-in-plot-new-figure-margins-too-large-in-r
    lines(t,pr_regr, xlab="time",ylab="output",type="l", col="blue", add=TRUE)
    
    
    # http://www.countbio.com/web_pages/left_object/R_for_biology/R_fundamentals/multiple_curves_R.html
    legend(1,500,legend=c("measured output","predicted output"),col=c("blue","red"),pch=c("-","-"),
           lty=c(1,2,3))
           
    #--------- options to test
    # https://stackoverflow.com/questions/2564258/plot-two-graphs-in-same-plot-in-r
    # df <- data.frame(t,output_tr,pr_regr)
    # ggplot(df, aes(t)) +                    # basic graphical object
    #   geom_line(aes(y=output_tr), colour="red") +  # first layer
    #   geom_line(aes(y=pr_regr), colour="green")  # second layer
    
    # df1 <- data.frame(x=t,y=output_tr,type="l")
    # df2 <- data.frame(x=t,y=pr_regr,type="l")
    # df <- rbind(df1,df2)
    # library(ggplot2)
    # ggplot(df)+geom_line(aes(x,y,colour=type))
    
    

    RMSE <- sqrt(mean((pr_regr-output_tr)^2))
    
    tss         <-  sum((pr_regr-output_tr)^2 )
    y_all_mean  <-  mean(output_tr)
    rss         <-  sum((output_tr-y_all_mean)^2)
    rsq         <-  1 - (tss/rss)
  }
  
  # test
  if (flag==2) {
    #cat(nhid,actfun,init_weights,bias,'\n')
    out_regr <-elm_train(input_te, output_te, nhid, actfun, init_weights , bias, verbose = FALSE)
    #cat("ELM")
    pr_regr <-elm_predict(out_regr, input_te)
    #plot(pr_regr,output_te, xlab="predicted",ylab="actual")
    
    d <- dim(output_te)[1]
    t <- seq(1,d)
    
    plot(t,output_te, xlab="time",ylab="output",type="l", col="red")
    par(mar = rep(2, 4))   # https://stackoverflow.com/questions/12766166/error-in-plot-new-figure-margins-too-large-in-r
    lines(t,pr_regr, xlab="time",ylab="output",type="l", col="blue")
    
    legend(1,500,legend=c("measured output","predicted output"),col=c("blue","red"),pch=c("-","-"),
           lty=c(1,2,3))
    
    RMSE <- sqrt(mean((pr_regr-output_te)^2))
    
    tss         <-  sum((pr_regr-output_te)^2 )
    y_all_mean  <-  mean(output_te)
    rss         <-  sum((output_te-y_all_mean)^2)
    rsq         <-  1 - (tss/rss)
  }
  cat('The R-square of the test data is ', rsq, '\n')
  cat('Objective function (RMSE) is ',RMSE,'\n')
  

}