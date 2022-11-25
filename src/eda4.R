# Cross-validation based on test error.

## LOG

#Set seed to make the analysis reproducible
set.seed(1)
x1 <- bc_data_red[,1:6] #Extract response variable
yClass <- bc_data_red[,7] #Extract the predictor variables

# 10-fold cross validation
nfolds <- 10

# Sample fold-assignment index
fold_index <- sample(nfolds, nrow(x1), replace=TRUE)

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
m.y <- as.matrix(yClass)
m_red <- as.matrix(bc_data_red) #doesn't work aS replaced x/y

# NOT-WORKING - Function to estimate the mean squared error (MSE) by k-fold cross validation for lasso regression
lasso_cv = function(m.x1, y, fold_ind) { #If m.y is used - Error in data.frame(m.x1, m.y = y) : arguments imply differing number of rows: 683, 546
  Xy = data.frame(m.x1, y=y)
  nfolds = max(fold_ind)
  if(!all.equal(sort(unique(fold_ind)), 1:nfolds)) stop("Invalid fold partition.")
  cv_errors = numeric(nfolds)
  for(fold in 1:nfolds) {
    tmp_fit = glmnet(y ~ ., data=Xy[fold_ind!=fold,], family = "binomial", alpha = 1, standardize = FALSE, lambda = lambda__min)
    yhat = predict(tmp_fit, Xy[fold_ind==fold,], s=lamda_min, type= "response")
    yobs = y[fold_ind==fold]
    cv_errors[fold] = mean((yobs - yhat)^2) #1 - mean(Xy$y[fold_ind==fold]==yhat) doesn't work either
  }
  fold_sizes = numeric(nfolds)
  for(fold in 1:nfolds) fold_sizes[fold] = length(which(fold_ind==fold))
  test_error = weighted.mean(cv_errors, w=fold_sizes)
  return(test_error)
} 

# NOT-WORKING applying to data
(lasso_cv = lasso_cv(m.x1, m.y, fold_ind=fold_index)) #Error in if (is.null(np) | (np[2] <= 1)) stop("x should be a matrix with 2 or more columns") : argument is of length zero

# Attempt 2: Cross validation for lasso using nfolds for consistency
lasso_cv = cv.glmnet(m.x1, yClass, family = "binomial", alpha = 1, standardize = FALSE, lambda = grid, type.measure = "class", foldid = fold_index)

#Identify the optimal value for the turning parameter
(lambda_min = lasso_cv$lambda.min) #0.003053856
(i = which(lasso_cv$lambda == lambda_min)) #93

#Extract mean MSE
lasso_final_mse <- lasso_cv$cvm[i] #0.03221083

## NOT-WORKING LDA

# Attempt 1: Perform cv LDA
(lda_cv <- lda(Class~., data=bc_data_red, cv=TRUE, subset = fold_index))

# Attempt 2: Function to estimate the mean squared error (MSE) by k-fold cross validation for LDA
lda_cv = function(m.x1, y, fold_ind) { 
  Xy = data.frame(m.x1, y=y)
  nfolds = max(fold_ind)
  if(!all.equal(sort(unique(fold_ind)), 1:nfolds)) stop("Invalid fold partition.")
  cv_errors = numeric(nfolds)
  for(fold in 1:nfolds) {
    tmp_fit = lda(y ~ ., data=Xy[fold_ind!=fold,])
    yhat = predict(tmp_fit, Xy[fold_ind==fold,])
    yobs = y[fold_ind==fold]
    cv_errors[fold] = 1 - mean(Xy$y[fold_ind==fold]==yhat) #mean((yobs - yhat)^2) doesn't work either
  }
  fold_sizes = numeric(nfolds)
  for(fold in 1:nfolds) fold_sizes[fold] = length(which(fold_ind==fold))
  test_error = weighted.mean(cv_errors, w=fold_sizes)
  return(test_error)
}

# applying to data
(lda_final_mse = lda_cv(m.x1, m.y, fold_index))  #Error in h(simpleError(msg, call)) : error in evaluating the argument 'x' in selecting a method for function 'mean': 'list' object cannot be coerced to type 'double'

## Haven't applied cv to QDA as I can't resolve the errors in LDA