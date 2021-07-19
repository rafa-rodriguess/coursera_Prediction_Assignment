library(e1071)
library(caret)
library(doParallel)

#Download Training Set
pml_training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
dest <- "pml-training.csv"
if (!file.exists(dest))download.file(pml_training,dest)
training_set <- read.csv(dest, row.names = 1)


#Download Final Testing Set
pml_testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
dest <- "pml_testing.csv"
if (!file.exists(dest))download.file(pml_testing,dest)
testing_set <- read.csv(dest, row.names = 1)


#cleanning data
#Remove columns with more than 95% of NA or "" values
treshold <- dim(training_set)[1] * 0.95
goodColumns <- !apply(training_set, 2, function(x) sum(is.na(x)) > treshold  || sum(x=="") > treshold)
training_set <- training_set[, goodColumns]
badColumns <- nearZeroVar(training_set, saveMetrics = TRUE)
training_set <- training_set[, badColumns$nzv==FALSE]
training_set$classe = factor(training_set$classe)

testing_set <- testing_set[, goodColumns]
testing_set <- testing_set[, badColumns$nzv==FALSE]
testing_set$classe <- NA


#Making the trainning and crossvalidation dataset
inTrain <- createDataPartition(training_set$classe, p = 0.7, list=FALSE)
crossv <- training_set[-inTrain,]
training <- training_set[ inTrain,]


#Model 01 - trees
mod1 <- train(classe ~., method="rpart", data=training)
pred1 <- predict(mod1, crossv)
confusionMatrix(pred1, crossv$classe)
finalpred <- predict(mod1, testing_set)
finalpred

#Model 02 - Random Florest
mod2 <- train(classe ~ ., method="rf", data=training)
pred2 <- predict(mod2, crossv)
confusionMatrix(pred2, crossv$classe)
finalpred <- predict(mod2, testing_set)
finalpred

#Model 03 - GBM
mod3 <- train(classe ~ ., method="gbm", data=training)
pred3 <- predict(mod3, crossv)
confusionMatrix(pred3, crossv$classe)
finalpred <- predict(mod3, testing_set)
finalpred

#Model 04 - LDA
mod4 <- train(classe ~ ., data=training, method="lda")
pred4 <- predict(mod4, crossv)
confusionMatrix(pred4, crossv$classe)
finalpred <- predict(mod4, testing_set)
finalpred





