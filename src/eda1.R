# Extract the response variable
Class <- BC2[,11]-1

#Extract the predictor variables
x <- BC2[,2:10] 

# Standardise the predictor variables 
xs <- scale(x)

# Reform the data frame
bc_data <- data.frame(xs, Class)

# Store n and p
n <- nrow(bc_data)
p <- ncol(bc_data)- 1

# Fit logistic regression model
logr_fit = glm(Class ~ ., data = bc_data, family="binomial")

# Summarise the model fit
summary(logr_fit) # Cell thickness is most associated with malignant, followed by bare.nuclei and bl cromatin
# A number of variables have very large p-values meaning that, individually, they contribute very little to a model which contains all other predictors. 
# Only cl.thickness, marg.adhesion, bare.nuclei and bl.cromatin have coefficients significantly different from zero.

# Apply best subset selection
fit_AIC <- bestglm(bc_data, family = binomial, IC="AIC")
fit_BIC <- bestglm(bc_data, family = binomial, IC="BIC")

# Examine subset selection results
fit_AIC$Subsets
fit_BIC$Subsets

# Create a multi-panel plotting device
par(mfrow=c(1,2))

# Produce plots, highlighting optimal value of 'k'
plot(0:p, fit_AIC$Subsets$AIC, xlab = "Number of Predictors", ylab = "AIC", type = "b") # 5
plot(0:p, fit_BIC$Subsets$BIC, xlab = "Number of Predictors", ylab = "BIC", type = "b") # 7

# It seems like the model with 6 predictors is a good compromise between 5 and 7 
pstar = 6

# Check which predictors are in the 6-predictor model
fit_AIC$Subsets[pstar+1,]

# Construct a reduced data set containing only the selected predictors
(indices <- as.logical(fit_AIC$Subsets[pstar+1, 2:(p+1)]))
bc_data_red = data.frame(bc_data[,indices])

# Obtain regression coefficients for this model
logr1_fit = glm(Class ~ ., data = bc_data_red, family="binomial")
summary(logr1_fit)  # Estimate std. are the maximum liklihood estimates of the regression coefficients. Because Cl. thickness and bare.nuclei are the largest positive values, this indicates that leisons with higher numbers in theres are more likely to have malignant cancer?
summ(logr1_fit, scale = TRUE) # Presents details of model fit
summ(logr1_fit, confint = TRUE, digits = 3) # Presents confidence intervals

# Quick plot of effect
effect_plot(logr1_fit, pred = Cl.thickness, interval = TRUE, plot.points = TRUE, jitter = 0.25) # Why are there points below zero for class?
