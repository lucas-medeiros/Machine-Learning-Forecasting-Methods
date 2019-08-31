library(MASS)
library(GGally)
library(corrplot)
library(glmnet)
library(dplyr)
library(leaps)

#Initial Data Exloration
data(Boston)
dim(Boston)
cormat=cor(Boston)
corrplot(cormat, type = "upper", method="number")
Boston$chas<-as.factor(Boston$chas)
#EDA
plot(medv~lstat, data=Boston)
plot(medv~rm, data=Boston)
counts <- table(Boston$chas)
barplot(counts, main="", xlab="chas")

#Test train split
set.seed(123)
index <- sample(nrow(Boston),nrow(Boston)*0.80) #80-20 split
Boston.train <- Boston[index,]
Boston.test <- Boston[-index,]

#Building Linear Regression Model
model0<- lm(medv~lstat, data = Boston.train)
model1<- lm(medv~., data=Boston.train)
model2<- lm(medv~. -indus -age, data=Boston.train)
AIC(model0)

## [1] 2626.408
BIC(model0)
## [1] 2638.412
AIC(model1)
## [1] 2434.433
BIC(model1)
## [1] 2494.454
AIC(model2)
## [1] 2431.336
BIC(model2)
## [1] 2483.354
anova(model2, model1)

summary(model2)

#Best Subset Selection
model.subset<- regsubsets(medv~.,data=Boston.train, nbest=1, nvmax = 13)
subset_fit=summary(model.subset)
subset_fit

#Stepwise selection
null.model<-lm(medv~1, data=Boston)
full.model<-lm(medv~., data=Boston.train)
result<-step(null.model, scope=list(lower=null.model, upper=full.model), k = 2, direction="forward")
result$anova
result<-step(full.model, scope=list(lower=null.model, upper=full.model), k = 2, direction="backward")
result$anova
result<-step(null.model, scope=list(lower=null.model, upper=full.model), k = 2, direction="both")
result$anova

#LASSO Regression
Boston$chas<-as.numeric(Boston$chas)
#Standardize covariates before fitting LASSO
Boston.X.std<- scale(select(Boston,-medv))
X.train<- as.matrix(Boston.X.std)[index,]
X.test<-  as.matrix(Boston.X.std)[-index,]
Y.train<- Boston[index, "medv"]
Y.test<- Boston[-index, "medv"]

lasso.fit<- glmnet(x=X.train, y=Y.train, family = "gaussian", alpha = 1)
plot(lasso.fit, xvar = "lambda", label=TRUE)

#Cross Validation in LASSO
cv.lasso<- cv.glmnet(x=X.train, y=Y.train, family = "gaussian", alpha = 1, nfolds = 10)
plot(cv.lasso)

cv.lasso$lambda.min
## [1] 0.00909601
cv.lasso$lambda.1se
## [1] 0.3424589
pred.lasso.train<- predict(lasso.fit, newx = X.train, s=cv.lasso$lambda.min)
pred.lasso.min<- predict(lasso.fit, newx = X.test, s=cv.lasso$lambda.min)
pred.lasso.1se<- predict(lasso.fit, newx = X.test, s=cv.lasso$lambda.1se)
#Lasso MSE
mean((Y.train-pred.lasso.train)^2)
## [1] 22.51041
#MSPE
mean((Y.test-pred.lasso.min)^2)
## [1] 20.27092
mean((Y.test-pred.lasso.1se)^2)
## [1] 21.25325
