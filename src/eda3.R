# Test for multinormal data
mult.norm(xs) # Data is not multinormal

# Common covariance matrix
cov(xs)

# Perform LDA
(lda_fit <- linDA(variables = bc_data_red[,1:6], group = bc_data_red$Class)) # error rate 0.03953148

# Extract discriminant functions
lda_fit$functions

# Normalise LDA confusion matrix
t(apply(lda_fit$confusion, 1, normalise)) # Performance okay 98% in observed vs 92% in predicted

## Sample indices of training data:
train_set = sample(c(TRUE, FALSE), nrow(bc_data_red), replace=TRUE)
training_indices = which(train_set)
head(training_indices)

## Deduce indices of validation data:
validation_indices = which(!train_set)
head(validation_indices)

#Perform validation LDA
(lda_fit_vsa <- linDA(variables=bc_data_red[,1:6], group = bc_data_red$Class, validation="learntest", learn=training_indices, test=validation_indices)) # error rate 0.03550296
lda_train <- lda(Class~., data=bc_data_red[train_set, ])
#Group means:
#Cl.thickness Cell.shape Marg.adhesion Bare.nuclei Bl.cromatin Normal.nucleoli
#0   -0.5251410 -0.6001135    -0.5114023  -0.6148178  -0.5384374      -0.5099309
#1    0.9222019  1.1209302     0.7787258   1.0866759   0.9258078       0.9086436

## Compute fitted values for the lda validation data
lda_test = predict(lda_train, bc_data_red[!train_set,])
lda_yhat_test = lda_test$class # 0.04854369

## Compute test error
1 - mean(bc_data_red$Class[!train_set] == lda_yhat_test)

# Perform QDA
qda_fit <- quaDA(variables = bc_data_red[,1:6], group = bc_data_red$Class, functions = TRUE) # error rate 0.04538799

# Extract discriminant functions and print to screen
qda_fit$functions

# Perform validation QDA
(qda_fit_vsa <- quaDA(variables=bc_data_red[,1:6], group = bc_data_red$Class, validation="learntest", learn=training_indices, test=validation_indices)) # 0.07119741
qda_train <- qda(Class~., data=bc_data_red[train_set, ])
# Group means:
#Cl.thickness Cell.shape Marg.adhesion Bare.nuclei Bl.cromatin Normal.nucleoli
#0   -0.5708387 -0.6260378    -0.5172869  -0.6151112  -0.5447366      -0.5278978
#1    0.9258774  1.1737511     0.9937861   1.1340365   1.0743485       1.0430729

## Compute fitted values for the qda validation data
qda_test = predict(qda_train, bc_data_red[!train_set,])
yhat_test = qda_test$class

## Compute test error
1 - mean(bc_data_red$Class[!train_set] == yhat_test) # 0.07119741
