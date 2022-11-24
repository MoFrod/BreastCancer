#LASSO - Re-written code to find error.
# Choose grid of values for the turning parameter
grid = 10^seq(5, -3, length = 100)

#Fit a model with LASSO penalty for each value of the turning parameter, for all data
lasso_fit = glmnet(xs, bc_data$Class, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid)

# Examine the effect of the tuning parameter on the parameter estimates for all data
plot(lasso_fit, xvar="lambda", col=rainbow(p), label=TRUE) # Doesn't align to LogR selection. Last to drop out = 6, followed  by = 3, 2, 1, 7, 8, 4, 5, first to drop out = 9

# Cross-validate LASSO for all data
lasso_cv_fit = cv.glmnet(xs, bc_data$Class, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid, type.measure = "class")
plot(lasso_cv_fit) # Plot cross-validation

#Identify the optimal value for the turning parameter
(lambda_lasso_min = lasso_cv_fit$lambda.min) #0.007742637
(which_lambda_lasso = which(lasso_cv_fit$lambda == lambda_lasso_min)) #89

#Find the parameter associated with the optimal value of the turning parameter
coef(lasso_fit, s=lambda_lasso_min) #CT, BN, CSh, BC, MA, and NN have the highest values, and are most influential.

#Compute predicted probabilities
lasso_phat = predict(lasso_fit, xs, s=lambda_lasso_min, type = "response")

#Compute fitted (i.e. predicted) values
lasso_yhat = ifelse(lasso_phat > 0.5, 1, 0)

#Calculate confusion matrix
(lasso_confusion = table(Observed=bc_data$Class, Predicted=lasso_yhat))

#Calculate the training error
1-mean(bc_data$Class==lasso_yhat) #0.02928258, 2.93%

#LASSO w. 6 Predictor Variables

# Extract the response variable
yClass <- bc_data_red[,7]

#Extract the predictor variables
x1 <- bc_data_red[,1:6]

#Fit a model with LASSO penalty for each value of the turning parameter, for all data
lasso_fit_red = glmnet(x1, yClass, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid)

# Examine the effect of the tuning parameter on the parameter estimates for all data
plot(lasso_fit_red, xvar="lambda", col=rainbow(p), label=TRUE) 

# Cross-validate LASSO for all data
x1 <- as.matrix(x1)
lasso_cv_fit_red = cv.glmnet(x1, yClass, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid, type.measure = "class")
plot(lasso_cv_fit_red) # Plot cross-validation

#Identify the optimal value for the turning parameter
(lambda_lasso_min_red = lasso_cv_fit_red$lambda.min) #0.002104904
(which_lambda_lasso_red = which(lasso_cv_fit_red$lambda == lambda_lasso_min_red)) #96

#Find the parameter associated with the optimal value of the turning parameter
coef(lasso_fit_red, s=lambda_lasso_min_red) # CT, BN, CB and CS are most incluencial.

#Compute predicted probabilities
lasso_phat_red = predict(lasso_fit_red, x1, s=lambda_lasso_min_red, type = "response")

#Compute fitted (i.e. predicted) values
lasso_yhat_red = ifelse(lasso_phat_red > 0.5, 1, 0)

#Calculate confusion matrix
(lasso_confusion_red = table(Observed=bc_data_red$Class, Predicted=lasso_yhat_red))

#Calculate the training error
1-mean(bc_data_red$Class==lasso_yhat_red) #0.02928258, 2.93%

#LASSO w. SPLIT DATA

# Extract the response variable
tClass <- train[,7]

#Extract the predictor variables
xt <- train[,1:6]

##Cross-validate LASSO for training data
train_l = as.matrix(xt) #Covert to matrix for cv
lasso_cv_fit_train = cv.glmnet(train_l, tClass, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid, type.measure = "class")
plot(lasso_cv_fit_train) # Plot cross-validation

##Identify the optimal value for the turning parameter
(lambda_lasso_min_train = lasso_cv_fit_train$lambda.min) #0.002104904
(which_lambda_lasso_train = which(lasso_cv_fit_train$lambda == lambda_lasso_min_train)) #96

# Extract corresponding mean MSE
lasso_cv_fit_train$cvm[which_lambda_lasso_train] # 0.02380952

##Fit a model with LASSO penalty for each value of the turning parameter to training data
lasso_fit_train = glmnet(xt, tClass, family = "binomial", alpha = 1, standardize = FALSE, lambda = lambda_lasso_min_train)

## Find the parameter estimates associated with optimal value of the tuning parameter
coef(lasso_fit_train, s=lambda_lasso_min_train)

##Compute fitted values for the validation data
xtest <- test[,1:6] ##Extract the predictor variables
test_l <- as.matrix(xtest) #Test data as matrix
lasso_phat_test = predict(lasso_fit_train, test_l, s=lambda_lasso_min_train, type="response")
lasso_yhat_test = ifelse(lasso_phat_test > 0.5, 1, 0)

##Compute test error
1-mean(test$Class == lasso_yhat_test) #0.05839416, 5.84%

## Evaluate the model
lasso_phat_test = predict(lasso_fit_train, test_l, s=lambda_lasso_min_train, type="response") 
pred.lasso <- prediction(lasso_phat_test, test$Class)
roc.perf.lasso = performance(pred.lasso, measure = "tpr", x.measure = "fpr")
auc.train.lasso <- performance(pred.lasso, measure = "auc")
auc.train.lasso <- auc.train.lasso@y.values

## Plot LASSO on ROC
plot(roc.perf.lasso, col="#1f78b4", main="Plot : LASSO Regression ROC", cex.main=0.8)
abline(a=0, b= 1)
text(x = .25, y = .65, paste("AUC = ", round(auc.train.lasso[[1]],3), sep = ""))
