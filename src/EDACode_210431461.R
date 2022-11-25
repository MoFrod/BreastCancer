# Check size of data
dim(BC1)

# Summarise data
summary(BC1) # total summaries
describe(BC1)

## Boxplot
# Plot spread of characteristics by class
ggplot(BC1, aes(x = Class, y = Cl.thickness)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Clump Thickness (scale)", title = "Clump Thickness by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Cell.size)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Cell Size (scale)", title = "Cell Size by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Cell.shape)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Cell Shape (scale)", title = "Cell Shape by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Marg.adhesion)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Marginal Adhesion (scale)", title = "Marginal Adhesion  by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Epith.c.size)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Single Epitheial Cell Size (scale)", title = "Single Epitheial Cell Size by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Bare.nuclei)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Bare Nuclei (scale)", title = "Bare Nuclei by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Bl.cromatin)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Bland Cromatin (scale)", title = "Bland Cromatin by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Normal.nucleoli)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Normal Nucleoli (scale)", title = "Normal Nucleoli by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Mitoses)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Mitoses (scale)", title = "Mitoses by Class") + theme(legend.position = "none") + expand_limits(y = 0)

# Quick plot of data
pairs(BC1[2:11]) # Remove ID column as it represents meta-information and the response variable (M/B)
# Malignant only for cell thickness 10, cell size 5, 6 and 10, cell shape 10, marg.adhesion 7, 8, 9, epith.c.size 9, bare.nuclei 6 and 9, BI.cromatin 8, 9, 10, normal.nuclei 9 and 10, mitoses 4, 6, and 9
# Potentially linear relationship between cell size and shape
# Mitoses more consistent at lower levels for all 9 categories

# Percentage of Malignant per Benign
(239/444 * 100) #53.83%

# Filter for class type
M <- filter(BC1, Class == "malignant")
summary(M)
describe(M)
B <- filter(BC1, Class == "benign")
summary(B) # Mean/Median are generally smaller (only marg.adhesion, epith.c.size and bare.nuclei have max of 10)
describe(B)

# Understand the variance of the categories
apply(BC1[,2:10], 2, var) # Remove ID and Class, Bare.nuclei has largest var and epith.c.size has lowest.
apply(M[,2:10], 2, var) 
apply(B[,2:10], 2, var) # Very low variances in comparison to M

# Observe the correlations
(cor_bc <- cor(BC1[,2:10]))

# Plot correlations
corrplot(cor_bc, order = "hclust", tl.cex = 0.7) # Shows bivariate relationship among the characteristics. Highly correlated features are likely to provide redundant information.

# Quickplot heatmap
BC2 <- data.matrix(BC1) # Convert into numeric matrix
heatmap(BC2[,-1]) # This doesn't tell us much more.

# Sample variance of data
s <- var(BC2[,2:10])

# Calculate total variation
s_sq = diag(s) # Extract diagonal elements

(total_variation = sum(s_sq))
#70.7

# Generalised variance 
det(s) #47432

# GG density plots of variables
BC1 %>%
  ggplot(aes(x = Cl.thickness, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Clump Thickness", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Cell.size, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Cell Size", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Cell.shape, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Cell shape", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Marg.adhesion, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Marginal Adhesion", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Epith.c.size, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Single Epithelial Cell Size", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Bare.nuclei, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Bare Nuclei", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Bl.cromatin, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Bland Chromatin", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Normal.nucleoli, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Normal Nucleoli", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Mitoses, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Mitoses", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

# Extract the response variable
Class <- BC2[,11]-1

#Extract the predictor variables
x <- BC2[,2:10] 

# Quick pairs plot
pairs(x, col=c("black", "#1f78b4")[Class+1])

# Standardise the predictor variables 
xs <- scale(x)

# Sample means
center <- attr(xs, "scaled:center")

# Sample standard deviations
scale <- attr(xs, "scaled:scale")

# Reform the data frame
bc_data <- data.frame(xs, Class)

# Conduct PCA
pca <- prcomp(x=xs) # Already scaled, and excluding Class
print(pca) # Print results
summary(pca) # We need 6 PCAs to explain 0.93 of the variance, and 3 to explain 0.80.

pca$sdev
pca$rotation

# Quickplot
plot(pca, type = "lines", main="")

# Plot PC1 and PC2
pca_df <- as_tibble(pca$x)
ggplot(pca_df, aes(x=PC1, y=PC2, col=bc_data$Class)) + geom_point(alpha=0.5) 

# Best subset selection
bss <- regsubsets(Class ~., data=bc_data, method="exhaustive", nvmax=9)
(bss_summary <- summary(bss)) # summarise best subset selection

# Adjusted Rsq
bss_summary$adjr2
(best_adjr2 = which.max(bss_summary$adjr2)) #Optimal number of predictor values (k)

# Mallows's Cp statistic
bss_summary$cp
(best_cp = which.min(bss_summary$cp)) #Optimal number of predictor values (k)

#BIC
bss_summary$bic
(best_bic = which.min(bss_summary$bic)) #Optimal number of predictor values (k)


# Store n and p
n <- nrow(bc_data)
p <- ncol(bc_data)- 1

# Fit logistic regression model
(logr_fit = glm(Class ~ ., data = bc_data, family="binomial"))

# Summarise the model fit
summary(logr_fit) # Cell thickness is most associated with malignant, followed by bare.nuclei and bl cromatin
# A number of variables have very large p-values meaning that, individually, they contribute very little to a model which contains all other predictors. 
# Only cl.thickness, marg.adhesion, bare.nuclei and bl.cromatin have coefficients significantly different from zero.

# Set up and scale new data
xs_1 <- data.frame(Cl.thickness=9, Cell.size=7, Cell.shape=6, Marg.adhesion=2, Epith.c.size = 3, Bare.nuclei=4, Bl.cromatin=4, Normal.nucleoli=4, Mitoses=2) %>%
  scale(center=center, scale=scale) %>%
  as.data.frame()

# Perform prediction 1
(p1 = predict(logr_fit, xs_1, type="response")) 
(y = as.numeric(ifelse(p1 > 0.5, 1, 0))) # Would be likely to have malignant 

# Calculate fitted values 
phat <- predict(logr_fit, bc_data, type = "response") # Compute predicted probabilities
yhat = ifelse(phat > 0.5, 1, 0) # Compute predicted values

#Compute the confusion matrix
(confusion <- table(Observed=bc_data$Class, predicted=yhat))

# Normalise function
normalise = function(x) {
  return(x / sum(x))
}

# Apply function to the confusion matrix
t(apply(confusion, 1, normalise)) # Performance is okay.

# Calculate the training error
1 - sum(diag(confusion)) / sum(confusion) # 0.03074671 (3.08%)

# Apply best subset selection
fit_AIC <- bestglm(bc_data, family = binomial, IC="AIC")
fit_BIC <- bestglm(bc_data, family = binomial, IC="BIC")

# Examine subset selection results
fit_AIC$Subsets
fit_BIC$Subsets

# Identify best-fitting models
(best_AIC = fit_AIC$ModelReport$Bestk)

(best_BIC = fit_BIC$ModelReport$Bestk)

# Create a multi-panel plotting device
par(mfrow=c(1,2))

# Produce plots, highlighting optimal value of 'k'
plot(0:p, fit_AIC$Subsets$AIC, xlab = "Number of Predictors", ylab = "AIC", type = "b") # 7
points(best_AIC, fit_AIC$Subsets$AIC[best_AIC+1], col="#1f78b4", pch=16)

plot(0:p, fit_BIC$Subsets$BIC, xlab = "Number of Predictors", ylab = "BIC", type = "b") # 5
points(best_BIC, fit_BIC$Subsets$BIC[best_BIC+1], col="#1f78b4", pch=16)

# It seems like the model with 6 predictors is a good compromise between 5 and 7 
pstar = 6

# Check which predictors are in the 6-predictor model
fit_AIC$Subsets[pstar+1,]

# Construct a reduced data set containing only the selected predictors
(indices <- as.logical(fit_AIC$Subsets[pstar+1, 2:(p+1)]))
bc_data_red = data.frame(bc_data[,indices])

# Create test and train data sets
set.seed(683) 
train_index <- sample(nrow(bc_data_red), size = round(0.75 * nrow(bc_data_red)), replace = FALSE)
train <- bc_data_red[train_index,]
test <- bc_data_red[-train_index,]

# Obtain regression coefficients for this model
logr1_fit = glm(Class ~ ., data = train, family="binomial")
summary(logr1_fit)  # Estimate std. are the maximum liklihood estimates of the regression coefficients. Because Cl. thickness and bare.nuclei are the largest positive values, this indicates that leisons with higher numbers in theres are more likely to have malignant cancer?
summ(logr1_fit, scale = TRUE) # Presents details of model fit
summ(logr1_fit, confint = TRUE, digits = 3) # Presents confidence intervals

# Training error
predict(logr1_fit, train, type="response") 
(y = as.numeric(ifelse(p1 > 0.5, 1, 0))) # Would be likely to have malignant 

# Calculate fitted values 
phat <- predict(logr1_fit, train, type = "response") # Compute predicted probabilities
yhat <- ifelse(phat > 0.5, 1, 0) # Compute predicted values

#Compute the confusion matrix
(confusion <- table(Observed=train$Class, predicted=yhat))

# Normalise function
normalise = function(x) {
  return(x / sum(x))
}

# Apply function to the confusion matrix
t(apply(confusion, 1, normalise)) # Performance is okay.# Perform prediction 2

# Calculate the training error
1 - sum(diag(confusion)) / sum(confusion) # 0.02014652 (2.02%)

# Calculate Test error
logr_test = predict(logr1_fit, test, type ="response") 
yhat_logr_test = ifelse(logr_test > 0.5, 1, 0)

#Compute the confusion matrix
(confusion_2 <- table(Observed=test$Class, predicted=yhat_logr_test))

# Apply function to the confusion matrix
t(apply(confusion_2, 1, normalise)) # Performance is okay.

# Compute test error
(1-mean(test$Class == yhat_logr_test)) #0.05847953 (5.84%)

#Compute the confusion matrix
(confusion_2 <- table(Observed=test$Class, predicted=yhat_logr_test))

# Apply function to the confusion matrix
t(apply(confusion_2, 1, normalise)) # Performance is okay.

# ROC plot
logr_test_roc <- roc(test$Class ~ logr_test, plot = TRUE, print.auc = TRUE)

# Evaluate the model
pred.logr <- prediction(logr_test, test$Class)
roc.perf.logr = performance(pred.logr, measure = "tpr", x.measure = "fpr")
auc.train.logr <- performance(pred.logr, measure = "auc")
auc.train.logr <- auc.train.logr@y.values

# Plot LOGR on ROC
plot(roc.perf.logr, col="#1f78b4", main="Plot : LOGISTIC REGRESSION ROC", cex.main=0.8)
abline(a=0, b= 1)
text(x = .25, y = .65, paste("AUC = ", round(auc.train.logr[[1]],3), sep = ""))

#LASSO
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

#Perform LDA with linDa
linDA(variables=bc_data_red[,1:6], group=bc_data_red$Class)

x2 = seq(-3, 5, 0.001)
Q1 = -1.824+5.857*x2

# Perform LDA on train and test data
(lda_train <- lda(Class~., data=train, type = "response"))
#Cl.thickness Cell.shape Marg.adhesion Bare.nuclei Bl.cromatin Normal.nucleoli
#0   -0.5570767 -0.6143429    -0.5280895  -0.6066062  -0.5692678      -0.5342636
#1    0.9497031  1.1325687     0.9614207   1.1461472   1.0257595       0.9892274

## Compute fitted values for the lda validation data
lda_test = predict(lda_train, test)
lda_yhat_test = lda_test$class 

## Compute test error
1 - mean(test$Class == lda_yhat_test) # 0.0729927, 7.30%

# Get the posteriors as a dataframe.
lda.predict.posteriors <- as.data.frame(lda_test$posterior)

# Evaluate the model
pred.lda <- prediction(lda.predict.posteriors[,2], test$Class)
roc.perf.lda = performance(pred.lda, measure = "tpr", x.measure = "fpr")
auc.train.lda <- performance(pred.lda, measure = "auc")
auc.train.lda <- auc.train.lda@y.values
# Plot
plot(roc.perf.lda, col="#1f78b4", main="Plot : LDA ROC", cex.main=0.8)
abline(a=0, b= 1)
text(x = .25, y = .65, paste("AUC = ", round(auc.train.lda[[1]],3), sep = ""))


#Perform QDA with quaDa
quaDA(variables=bc_data_red[,1:6], group=bc_data_red$Class, functions=TRUE)

# Perform QDA
(qda_train <- qda(Class~., data=train))

#Group means:
#  Cl.thickness Cell.shape Marg.adhesion Bare.nuclei Bl.cromatin Normal.nucleoli
#0   -0.5570767 -0.6143429    -0.5280895  -0.6066062  -0.5692678      -0.5342636
#1    0.9497031  1.1325687     0.9614207   1.1461472   1.0257595       0.9892274

## Compute fitted values for the qda validation data
qda_test = predict(qda_train, test, type = "response")
yhat_test = qda_test$class

## Compute test error
1 - mean(test$Class == yhat_test) # 0.06569343, 6.57%

## Get the posteriors as a dataframe
qda.predict.posteriors <- as.data.frame(qda_test$posterior)

## Evaluate the model
pred.qda <- prediction(qda.predict.posteriors[,2], test$Class)
roc.perf.qda = performance(pred.qda, measure = "tpr", x.measure = "fpr")
auc.train.qda <- performance(pred.qda, measure = "auc")
auc.train.qda <- auc.train.qda@y.values

# Plot QDA on ROC
plot(roc.perf.qda, col="#1f78b4", main="Plot : QDA ROC", cex.main=0.8)
abline(a=0, b= 1)
text(x = .25, y = .65, paste("AUC = ", round(auc.train.qda[[1]],3), sep = ""))

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
