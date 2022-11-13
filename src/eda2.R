# Choose grid of values for the turning parameter
grid = 10^seq(5, -3, length = 100)

#Fit a model with LASSO penalty for each value of the turning parameter
lasso_fit = glmnet(xs, Class, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid)

li## Examine the effect of the tuning parameter on the parameter estimates
plot(lasso_fit, xvar="lambda", col=rainbow(p), label=TRUE)

# Extract the coefficients
beta_hat = coef(lasso_fit)

## Lots of shrinkage
grid[1]
beta_hat[,1]

## Some shrinkage
grid[75]
beta_hat[,75]

## Very little shrinkage
grid[100]
beta_hat[,100]

# Plot coefficients
plot(lasso_fit, xvar="lambda", col=1:8, label=TRUE)

# Cross-validation
lasso_cv_fit = cv.glmnet(xs, Class, family="binomial", alpha=1, standardize=FALSE, lambda=grid, type.measure="class")
plot(lasso_cv_fit)

## Identify the optimal value for the tuning parameter
(lambda_lasso_min = lasso_cv_fit$lambda.min)
# 0.009326033
(which_lambda_lasso = which(lasso_cv_fit$lambda == lambda_lasso_min))
#88

# Extract correspoding mean MSE
lasso_cv_fit$cvm[which_lambda_lasso]

## Find the parameter estimates associated with optimal value of the tuning parameter
coef(lasso_fit, s=lambda_lasso_min)
#0.03074671
