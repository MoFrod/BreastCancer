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
