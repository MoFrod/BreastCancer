# Extract the response variable
Class <- BC2[,11]-1

#Extract the predictor variables
x <- BC2[,2:10] 

pairs(x, col=Class)

# Standardise the predictor variables 
xs <- scale(x)

# Reform the data frame
bc_data <- data.frame(Class, xs)

# Fit logistic regression model
lr_fit = glm(Class ~ ., data = bc_data, family="binomial")

# Summarise the model fit
summary(lr_fit) # Cell thickness is most associated with malignant, followed by bare.nuclei and bl cromatin
# A numbr of variables have very large p-values meaning that, individually, they contribute very little to a model which contains all other predictors. 
