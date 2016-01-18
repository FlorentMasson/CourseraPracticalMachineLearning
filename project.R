library(caret)

#Import CSV files
trainingImport <- read.csv(file='D:/PraticalMachineLearning/pml-training.csv')
testingImport <- read.csv(file='D:/PraticalMachineLearning/pml-testing.csv')

#Convert predictor variable to factor
trainingImport$classe <- as.factor(trainingImport$classe)

#Remove columns with NA
t1 <- trainingImport[,colSums(is.na(trainingImport)) == 0]
#Remove columns with empty fields
t2 <- t1[,colSums(t1 == "") == 0]
#remove unrelated columns & keep only accelerometers data
t3 <- t2[, ! names(t2) %in% c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window") ]
# We now have 52 variables and 1 predictor

#Split into train and CV set
inTrain <- createDataPartition(t3$classe, p=0.70, list=FALSE)
training <- t3[inTrain,]
validation <- t3[-inTrain,]

#build the model using random forest
model <- train(classe~., data=training, method="rf")

predcv <- predict(model, validation)
confusionMatrix(predcv, validation$classe)


#Clean Testing set
#Remove columns with NA
t1 <- testingImport[,colSums(is.na(testingImport)) == 0]
#Remove columns with empty fields
t2 <- t1[,colSums(t1 == "") == 0]
#remove unrelated columns & keep only accelerometers data
t3 <- t2[, ! names(t2) %in% c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window", "prediction_id") ]
# We now have 52 variables
testing <- t3
#Makes predictions
predictions <- predict(model, testing)