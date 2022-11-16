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
pca <- prcomp(x=bc_data, scale=FALSE) # Already scaled
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

# Quick plot of effect
effect_plot(logr1_fit, pred = Cl.thickness, interval = TRUE, plot.points = TRUE, jitter = 0.25) # Why are there points below zero for class?
