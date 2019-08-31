# Loaging the library
library(glmnet)

# Loading the data
data(swiss)

x_var <- model.matrix(Fertility~. , swiss)[,-1]
y_var <- swiss$Fertility
lambda_seq <- 10^seq(2, -2, by = -.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_var), nrow(x_var)/2)
x_test = (-train)
y_test = y_var[test]

cv_output <- cv.glmnet(x_vars[train,], y_var[train], 
                       alpha = 1, lambda = lambda_seq)

# identifying best lamda
best_lam <- cv_output$lambda.min

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_vars[test,])

final <- cbind(y_var[test], pred)
# Checking the first six obs
head(final)

#The function provided below is just indicative and 
#you must provide the actual and predicted values based upon your dataset.

actual <- test$actual
preds <- test$predicted
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

# Inspecting beta coefficients
coef(lasso_best)
