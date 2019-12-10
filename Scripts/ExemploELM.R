##ELM test

library(elmNNRcpp)

#-----------
# Regression
#-----------

data(Boston, package = 'KernelKnn')

Boston = as.matrix(Boston)
dimnames(Boston) = NULL

x = Boston[, -ncol(Boston)]
y = matrix(Boston[, ncol(Boston)], nrow = length(Boston[, ncol(Boston)]), ncol = 1)

out_regr = elm_train(x, y, nhid = 20, actfun = 'purelin', init_weights = 'uniform_negative')

plot(out_regr$fitted_values)
plot(out_regr$predictions)
plot(out_regr$residuals)

#---------------
# Classification
#---------------

data(ionosphere, package = 'KernelKnn')

x_class = ionosphere[, -c(2, ncol(ionosphere))]
x_class = as.matrix(x_class)
dimnames(x_class) = NULL

y_class = as.numeric(ionosphere[, ncol(ionosphere)])

y_class_onehot = onehot_encode(y_class - 1)     # class labels should begin from 0

out_class = elm_train(x_class, y_class_onehot, nhid = 20, actfun = 'relu')

plot(out_class$fitted_values)
plot(out_class$predictions)
plot(out_class$residuals)


##EXEMPLO 2

# load the data and split it in two parts
#----------------------------------------

data(Boston, package = 'KernelKnn')

library(elmNNRcpp)
## Loading required package: KernelKnn
Boston = as.matrix(Boston)
dimnames(Boston) = NULL

X = Boston[, -dim(Boston)[2]]
xtr = X[1:350, ]
xte = X[351:nrow(X), ]


# prepare / convert the train-data-response to a one-column matrix
#-----------------------------------------------------------------

ytr = matrix(Boston[1:350, dim(Boston)[2]], nrow = length(Boston[1:350, dim(Boston)[2]]), ncol = 1)


# perform a fit and predict [ elmNNRcpp ]
#----------------------------------------

fit_elm = elm_train(xtr, ytr, nhid = 5, actfun = 'sig', init_weights = "uniform_negative", bias = TRUE, verbose = T)


pr_te_elm = elm_predict(fit_elm, xte)


# evaluation metric
#------------------

rmse = function (y_true, y_pred) {
  
  out = sqrt(mean((y_true - y_pred)^2))
  
  out
}


# test data response variable
#----------------------------

yte = Boston[351:nrow(X), dim(Boston)[2]]


# mean-squared-error for 'elm'
#--------------------------------------

cat('the rmse error for extreme-learning-machine is :', rmse(yte, pr_te_elm[, 1]), '\n')

