# Choose grid of values for the turning parameter
grid = 10^seq(5, -3, length = 100)

#Fit a model with LASSO penalty for each value of the turning parameter
lasso_fit_train = glmnet(train[,1:6], train$Class, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid)

# Examine the effect of the tuning parameter on the parameter estimates
plot(lasso_fit_train, xvar="lambda", col=rainbow(p), label=TRUE, print.eval=TRUE)

#Fit a model with LASSO penalty for each value of the turning parameter
#lasso_fit = glmnet(bc_data_red[1:6], Class, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid)

## Examine the effect of the tuning parameter on the parameter estimates
#plot(lasso_fit, xvar="lambda", col=rainbow(p), label=TRUE)


# Extract the coefficients
#(beta_hat = coef(lasso_fit))

## Lots of shrinkage
#grid[1]
#beta_hat[,1]

## Some shrinkage
#grid[75]
#beta_hat[,75]

## Very little shrinkage
#grid[100]
#beta_hat[,100]

# Extract the coefficients
(beta_hat_train = coef(lasso_fit_train))

## Lots of shrinkage
#grid[1]
#beta_hat_train[,1]

## Some shrinkage
#grid[75]
#beta_hat_train[,75]

## Very little shrinkage
#grid[100]
beta_hat_train[,100]

# Plot coefficients
#plot(lasso_fit, xvar="lambda", col=1:7, label=TRUE)

# Plot coefficients
plot(lasso_fit_train, xvar="lambda", col=1:7, label=TRUE)

# Cross-validation
lasso_train_cv_fit = cv.glmnet(train_l, train$Class, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid)
plot(lasso_train_cv_fit)

## Identify the optimal value for the tuning parameter
(lambda_lasso_min = lasso_train_cv_fit$lambda.min)# 0.001
(which_lambda_lasso = which(lasso_train_cv_fit$lambda == lambda_lasso_min)) #100

# Extract correspoding mean MSE
MSE <- lasso_train_cv_fit$cvm[which_lambda_lasso] #0.004222146

## Find the parameter estimates associated with optimal value of the tuning parameter
coef(lasso_fit_train, s=lambda_lasso_min)

#Fit a model with LASSO penalty for each value of the turning parameter
lasso1_fit_train = glmnet(train, train$Class, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid)

# Training error
train_l <- as.matrix(train)
# Calculate fitted values 
phat_l <- predict(lasso1_fit_train, train_l, type = "response") # Compute predicted probabilities
yhat_l <- ifelse(phat_l > 0.5, 1, 0) # Compute predicted values

#Compute the confusion matrix
(confusion_3 <- table(Observed=train$Class, predicted=yhat_l))

# Normalise function
normalise = function(x) {
  return(x / sum(x))
}

# Apply function to the confusion matrix
t(apply(confusion_3, 1, normalise)) # 

# Calculate the training error
1 - sum(diag(confusion_3)) / sum(confusion_3) # 

# Calculate Test error
#Re-add class into LASSO
lasso1_fit_train = glmnet(train, train$Class, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid)

# Compute test error
test_l <- as.matrix(test)
lasso_test = predict(lasso1_fit_train, test_l, type ="response") 
yhat_lasso_test = ifelse(lasso_test > 0.5, 1, 0)

# Compute test error
(1-mean(test$Class == yhat_lasso_test)) #0.2572263 25.72%

# ROC - Not working
lasso_test_roc <- roc(test$Class ~ lasso_test, plot = TRUE, print.auc = TRUE)

# ROC plot Tests
#Re-add class into LASSO
lasso2_fit_train = glmnet(train, train$Class, family = "binomial", alpha = 1, standardize = FALSE, lambda = MSE)

# Compute test error
lasso_test2 = as.vector(predict(lasso2_fit_train, test_l, type ="response")) 
yhat_lasso_test2 = ifelse(lasso_test2 > 0.5, 1, 0)

# Compute test error
(1-mean(test$Class == yhat_lasso_test2))

# ROC
lasso_test_roc <- roc(test$Class ~ lasso_test2, plot = TRUE, print.auc = TRUE)

# Cross-validation
#(lasso_cv_fit = cv.glmnet(xs, Class, family="binomial", alpha=1, standardize=FALSE, lambda=grid, type.measure="class"))
#plot(lasso_cv_fit)


