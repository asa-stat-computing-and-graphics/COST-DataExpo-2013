# This script is tuning the parameter(s) for random forests, SVM, and RPART and
# calculating the misclassification error rates.

library(caret)
library(e1071)

data1 <- read.csv("FinalData08.csv")
data2 <- read.csv("FinalData09.csv")
data3 <- read.csv("FinalData10.csv")

data1 <- na.omit(data1)
data2 <- na.omit(data2)
data3 <- na.omit(data3)

data1 <- data1[order(data1$ccegrp), ]
data2 <- data2[order(data2$ccegrp), ]
data3 <- data3[order(data3$ccegrp), ]

yPos <- which(colnames(data1) == "ccegrp")

# Rearrange data and rename the dependent variable for convenience
data1 <- cbind(y = data1[, yPos], data1[, -yPos])
data2 <- cbind(y = data2[, yPos], data2[, -yPos])
data3 <- cbind(y = data3[, yPos], data3[, -yPos])

# Randomly select 3000 values (cases to be part of the training set)
set.seed(111)
inTrain08 <- sample(1:nrow(data1), 3000)
set.seed(111)
inTrain09 <- sample(1:nrow(data2), 3000)
set.seed(111)
inTrain10 <- sample(1:nrow(data3), 3000)

# Subset the data sets into training and test set so that we can use the train 
# function from the caret package to do cross validation and to calculate
# the misclassification error rates
train08 <- data1[inTrain08, ]
train09 <- data2[inTrain09, ]
train10 <- data3[inTrain10, ]

test08 <- data1[-inTrain08, ]
test09 <- data2[-inTrain09, ]
test10 <- data3[-inTrain10, ]

trainX08 <- train08[, -1]
trainX09 <- train09[, -1]
trainX10 <- train10[, -1]

trainY08 <- as.factor(train08[, 1])
trainY09 <- as.factor(train09[, 1])
trainY10 <- as.factor(train10[, 1])

testX08 <- test08[, -1]
testX09 <- test09[, -1]
testX10 <- test10[, -1]

# Do 10-fold crossvalidataion
fitControl08 <- trainControl(method = "cv", number = 10)
fitControl09 <- trainControl(method = "cv", number = 10)
fitControl10 <- trainControl(method = "cv", number = 10)

########################################################################
# rpart
########################################################################
# I looked at a wide range of values for cp, but I'm only looking
# at the best couple of values to make the code run faster for other 
# users
rpartGrid08 <- expand.grid(.cp = seq(0, .03, by = .01)) 
rpartFit08 <- train(trainX08, 
                    trainY08, 
                    "rpart", 
                    tuneGrid = rpartGrid08, 
                    trControl = fitControl08)
rpartFit08
yhat08 <- predict(rpartFit08, newdata = testX08)
rpartMse <- mean(yhat08 != test08$y)
rpartMse

rpartGrid09 <- expand.grid(.cp = seq(0, .03, by = .01)) 
rpartFit09 <- train(trainX09, 
                    trainY09, 
                    "rpart", 
                    tuneGrid = rpartGrid09, 
                    trControl = fitControl09)
rpartFit09
yhat09 <- predict(rpartFit09, newdata = testX09)
rpartMse09 <- mean(yhat09 != test09$y)
rpartMse09

rpartGrid10 <- expand.grid(.cp = seq(0, .03, by = .01)) 
rpartFit10 <- train(trainX10, 
                    trainY10, 
                    "rpart", 
                    tuneGrid = rpartGrid10, 
                    trControl = fitControl10)
rpartFit10
yhat10 <- predict(rpartFit10, newdata = testX10)
rpartMse10 <- mean(yhat10 != test10$y)
rpartMse10

########################################################################
# Random Forest (RF)
########################################################################
# I looked at a wide range of values for mtry, but I'm only looking
# at the best couple of values to make the code run faster for other 
# users
rfGrid08 <- expand.grid(.mtry = 5:7)
rfFit08 <- train(trainX08, 
                 trainY08, 
                 "rf", 
                 tuneGrid = rfGrid08, 
                 trControl = fitControl08, 
                 verbose = FALSE)
rfFit08
yhat08 <- predict(rfFit08, newdata = testX08)
rfMse08 <- mean(yhat08 != test08$y)
rfMse08

rfGrid09 <- expand.grid(.mtry = 6:8)
rfFit09 <- train(trainX09, 
                 trainY09, 
                 "rf", 
                 tuneGrid = rfGrid09, 
                 trControl = fitControl09, 
                 verbose = FALSE)
rfFit09
yhat09 <- predict(rfFit09, newdata = testX09)
rfMse09 <- mean(yhat09 != test09$y)
rfMse09


rfGrid10 <- expand.grid(.mtry = 3:5)
rfFit10 <- train(trainX10, 
                 trainY10, 
                 "rf", 
                 tuneGrid = rfGrid10, 
                 trControl = fitControl10, 
                 verbose = FALSE)
rfFit10
yhat10 <- predict(rfFit10, newdata = testX10)
rfMse10 <- mean(yhat10 != test10$y)
rfMse10

########################################################################
# Linear Discriminant Analysis (LDA)
########################################################################
# There's no parameter that we need to tune here but we do need to 
# check how well LDA performs 
ldaFit08 <- train(trainX08, 
                  trainY08, 
                  "lda", 
                  trControl = fitControl08, 
                  verbose = FALSE)
ldaFit08
yhat08 <- predict(ldaFit08, newdata = testX08)
ldaErr08 <- mean(yhat08 != test08$y)
ldaErr08


ldaFit09 <- train(trainX09, 
                  trainY09, 
                  "lda", 
                  trControl = fitControl09, 
                  verbose = FALSE)
ldaFit09
yhat09 <- predict(ldaFit09, newdata = testX09)
ldaErr09 <- mean(yhat09 != test09$y)
ldaErr09


ldaFit10 <- train(trainX10, 
                  trainY10, 
                  "lda", 
                  trControl = fitControl10, 
                  verbose = FALSE)
ldaFit10
yhat10 <- predict(ldaFit10, newdata = testX10)
ldaErr10 <- mean(yhat10 != test10$y)
ldaErr10

########################################################################
# Support Vector Machines (SVM)
########################################################################
# I've already chosen the optimal tuning parameters just so that the 
# code runs faster.
svmGrid08 <- expand.grid(.C = 2, .sigma = .001)
svmFit08 <- train(trainX08, 
                  trainY08, 
                  "svmRadial", 
                  tuneGrid = svmGrid08, 
  		            trControl = fitControl08)
svmFit08

yhat08 <- predict(svmFit08, newdata = testX08)
svmErr08 <- mean(yhat08 != test08$y)
svmErr08


svmGrid09 <- expand.grid(.C = 1, .sigma = 10^(-3))
svmFit09 <- train(trainX09, 
                  trainY09, 
                  "svmRadial", 
                  tuneGrid = svmGrid09, 
    	            trControl = fitControl09)
svmFit09

yhat09 <- predict(svmFit09, newdata = testX09)
svmErr09 <- mean(yhat09 != test09$y)
svmErr09


svmGrid10 <- expand.grid(.C = .5, .sigma = 10^(-2))
svmFit10 <- train(trainX10, 
                  trainY10, 
                  "svmRadial", 
                  tuneGrid = svmGrid10, 
                  trControl = fitControl10)
svmFit10

yhat10 <- predict(svmFit10, newdata = testX10)
svmErr10 <- mean(yhat10 != test10$y)
svmErr10
