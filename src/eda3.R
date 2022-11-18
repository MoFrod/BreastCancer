# Perform LDA 
lda_train <- lda(Class~., data=train, type = "response")
#Group means:
#  Cl.thickness Cell.shape Marg.adhesion Bare.nuclei Bl.cromatin Normal.nucleoli
#0   -0.5570767 -0.6143429    -0.5280895  -0.6066062  -0.5692678      -0.5342636
#1    0.9497031  1.1325687     0.9614207   1.1461472   1.0257595       0.9892274

## Compute fitted values for the lda validation data
lda_test = predict(lda_train, test)
lda_yhat_test = lda_test$class 

## Compute test error
1 - mean(test$Class == lda_yhat_test) # 0.0729927, 7.30%

# Perform QDA
qda_train <- qda(Class~., data=train)
#Group means:
#  Cl.thickness Cell.shape Marg.adhesion Bare.nuclei Bl.cromatin Normal.nucleoli
#0   -0.5570767 -0.6143429    -0.5280895  -0.6066062  -0.5692678      -0.5342636
#1    0.9497031  1.1325687     0.9614207   1.1461472   1.0257595       0.9892274

## Compute fitted values for the qda validation data
qda_test = predict(qda_train, test, type = "response")
yhat_test = qda_test$class

## Compute test error
1 - mean(test$Class == yhat_test) # 0.06569343, 6.57%
