# Cross-validation based on test error.
# Extract the response variable
yClass <- bc_data_red[,7]

#Extract the predictor variables
x1 <- bc_data_red[,1:6]

## LOG

#Set seed to make the analysis reproducible
set.seed(1)

# 10-fold cross validation
nfolds <- 10

# Sample fold-assignment index
fold_index <- sample(nfolds, n, replace=TRUE)

# Function to estimate the mean squared error (MSE) by k-fold cross validation for logistic regression
reg_cv = function(X1, y, fold_ind) {
  Xy = data.frame(X1, y=y)
  nfolds = max(fold_ind)
  if(!all.equal(sort(unique(fold_ind)), 1:nfolds)) stop("Invalid fold partition.")
  cv_errors = numeric(nfolds)
  for(fold in 1:nfolds) {
    tmp_fit = glm(y ~ ., data=Xy[fold_ind!=fold,])
    yhat = predict(tmp_fit, Xy[fold_ind==fold,])
    yobs = y[fold_ind==fold]
    cv_errors[fold] = mean((yobs - yhat)^2)
  }
  fold_sizes = numeric(nfolds)
  for(fold in 1:nfolds) fold_sizes[fold] = length(which(fold_ind==fold))
  test_error = weighted.mean(cv_errors, w=fold_sizes)
  return(test_error)
}

# applying to data
(logr_final_mse = reg_cv(bc_data_red[,1:6], bc_data_red[,7], fold_index)) #0.03806376

#LASSO

#x as matrix
m.x1 <- as.matrix(x1)

# Cross validation for lasso using nfolds for consistency
lasso_cv = cv.glmnet(m.x1, yClass, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid, type.measure = "class", foldid = fold_index)

#Identify the optimal value for the turning parameter
(lambda_min = lasso_cv$lambda.min) #0.003053856
(i = which(lasso_cv$lambda == lambda_min)) #93

#Extract mean MSE
lasso_final_mse <- lasso_cv$cvm[i] #0.03221083

## LDA
# Perform cv LDA
(lda_cv <- lda(Class~., data=bc_data_red, cv=TRUE, subset = fold_index))



