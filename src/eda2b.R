#LASSO - Re-written code to determine error
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

#LASSO w. SPLIT DATA

##Cross-validate LASSO for training data
train_l = as.matrix(train) #Covert to matrix for cv
lasso_cv_fit_train = cv.glmnet(train_l, train$Class, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid, type.measure = "class")
plot(lasso_cv_fit_train) # Plot cross-validation

##Identify the optimal value for the turning parameter
(lambda_lasso_min_train = lasso_cv_fit_train$lambda.min) #0.01963041
(which_lambda_lasso_train = which(lasso_cv_fit_train$lambda == lambda_lasso_min_train)) #84

# Extract correspoding mean MSE
MSE <- lasso_train_cv_fit$cvm[which_lambda_lasso] #0.0336585

##Fit a model with LASSO penalty for each value of the turning parameter to training data
lasso_fit_train = glmnet(train, train$Class, family = "binomial", alpha = 1, standardize = FALSE, lambda = lambda_lasso_min_train)

##Compute fitted values for the validation data
test_l <- as.matrix(test) #Test data as matrix
lasso_phat_test.n = as.numeric(predict(lasso_fit_train, test_l, s=lambda_lasso_min_train, type="response"))
lasso_yhat_test.n = ifelse(lasso_phat_test.n > 0.5, 1, 0)

##Compute test error
1-mean(test$Class == lasso_yhat_test.n) #0%
1-mean(test_l == lasso_yhat_test.n) #0.8571429. If i change to test_l[,7] it goes back to zero.

## Evaluate the model
lasso_phat_test = predict(lasso_fit_train, test_l, s=lambda_lasso_min_train, type="response") #Not as.numeric
pred.lasso <- prediction(lasso_phat_test, test$Class)
roc.perf.lasso = performance(pred.lasso, measure = "tpr", x.measure = "fpr")
auc.train.lasso <- performance(pred.lasso, measure = "auc")
auc.train.lasso <- auc.train.lasso@y.values

# Plot LASSO on ROC
plot(roc.perf.lasso, col="#1f78b4", main="Plot : LASSO Regression ROC", cex.main=0.8)
abline(a=0, b= 1)
text(x = .25, y = .65, paste("AUC = ", round(auc.train.lasso[[1]],3), sep = ""))

# ROC - NOT WORKING
lasso_test_roc <- roc(test_l[,7] ~ lasso_phat_test.n, plot = TRUE, print.auc = TRUE, main="Plot : LASSO Regression ROC", cex.main=0.8, col="#1f78b4")
lasso_test_roc <- roc(train$Class ~ lasso_phat_test.n, plot = TRUE, print.auc = TRUE, main="Plot : LASSO Regression ROC", cex.main=0.8, col="#1f78b4")
