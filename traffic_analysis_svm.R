library(kernlab)
library(caret)
library(dplyr)
library("doParallel")

#---------- END CREATE TEST AND TRAINING ----------#

data_classifier <- ksvm(Citation ~ Race + Gender, data = training_data,
                          kernel = "vanilladot")

data_predictions <- predict(data_classifier, test_data)

#head(data_predictions)
#table(data_predictions, test_data$Citation)

#agreement <- data_predictions == test_data$Citation
#table(agreement)
#prop.table(table(agreement))

#use caret confusion matrix
confusionMatrix(data_predictions,test_data$Citation) #83.9% AVG Accuracy

#Register core backend, using 4 cores
cl <- makeCluster(4)
registerDoParallel(cl)

#list number of workers
getDoParWorkers()

#purposely using too small number of folds to reduce computation time, use 10 fold 
#and repeats
ctrl <- trainControl(method="cv",number=2,
                     classProbs=TRUE,
                     #function used to measure performance
                     summaryFunction = multiClassSummary, 
                     allowParallel = TRUE) #default looks for parallel backend

##svm with radial kernel
modelLookup("svmRadial")
m.svm <- train(Citation ~ Race + Gender, 
               trControl = ctrl,
               metric = "Accuracy", #using AUC to find best performing parameters
               preProc = c("range"), #scale from 0 to 1
               data = training_data, 
               method = "svmRadial")

#save train model to file to avoid very lengthy train time
saveRDS(m.svm, "letermodel-svmRadialCaretTrain.rds")

#read train model from file
m.svm<-readRDS("letermodel-svmRadialCaretTrain.rds")

plot(m.svm)


# Random
modelLookup("rf")

randomForest <- train(Citation ~ Race + Gender, 
                      # trControl = ctrl,
                      metric = "Accuracy", #using AUC to find best performing parameters
                      # preProc = c("range"), #scale from 0 to 1
                      data = training_data, 
                      method = "rf")

stopCluster(cl)



# 0.3: 70,72%
