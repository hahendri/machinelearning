# Set working directory
setwd("C:/Users/harla/Desktop/Coursera/PracticalMachineLearning/machinelearning")
# Add libraries
library(dplyr)
library(ggplot2)
library(lattice)
library(lubridate)
library(caret)
# Data for this project comes from http://groupware.les.inf.puc-rio.br/har and
# this dataset is licensed under the Creative Commons license (CC BY-SA). 
# The CC BY-SA license means you can remix, tweak, and build upon this work even for commercial purposes, 
# as long as you credit the authors of the original work and you license your new creations under 
# the identical terms we are licensing to you. This license is often compared to "copyleft" 
# free and open source software licenses. All new works based on this dataset will carry the same 
#license, so any derivatives will also allow commercial use. Read more: 
# http://groupware.les.inf.puc-rio.br/har#ixzz5OgHPS02v
# Citation:
# Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. 
# Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th 
# International Conference in Cooperation with SIGCHI (Augmented Human '13) . 
# Stuttgart, Germany: ACM SIGCHI, 2013. 
# Read more: http://groupware.les.inf.puc-rio.br/har#ixzz5OgIPQbIt

# Download and read the train and test datasets:
if (!file.exists("data")){
      dir.create("data")  
}      
trainfileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testfileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        download.file(trainfileURL, destfile = "./data/hartraindata.csv", method="curl")
        download.file(testfileURL, destfile = "./data/hartestdata.csv", method = "curl")
traindata <- read.csv("./data/hartraindata.csv")
testdata <- read.csv("./data/hartestdata.csv")
# Set Seed and create train and validation set from train data
set.seed(1234)
intrain <- createDataPartition(y = traindata$classe, p = 0.90, list = FALSE)
training <- traindata[intrain,]
validation <- traindata[-intrain,]
#Set cross validation method for 10-Fold
controlcv <- trainControl(method = "cv", number = 10, savePredictions = "all", classProbs = TRUE)
#Out of 159 predictor variables, I choose predictors that most likely would predict form of movement
#and choose a manageable number of predictors to keep the model simple for processing.
#Train Tree Model
modeltree <- train(classe ~ roll_belt + pitch_belt + yaw_belt + total_accel_belt + roll_arm + 
                  pitch_arm + yaw_arm + total_accel_arm + roll_dumbbell + pitch_dumbbell + 
                  yaw_dumbbell + total_accel_dumbbell + roll_forearm + pitch_forearm + 
                  yaw_forearm + total_accel_forearm, data = training, method = "rpart",
                  trControl = controlcv)
#Train Random Forest Model
modelrf <- train(classe ~ roll_belt + pitch_belt + yaw_belt + total_accel_belt + roll_arm + 
                  pitch_arm + yaw_arm + total_accel_arm + roll_dumbbell + pitch_dumbbell + 
                  yaw_dumbbell + total_accel_dumbbell + roll_forearm + pitch_forearm + 
                  yaw_forearm + total_accel_forearm, data = training, method = "rf",
                  trControl = controlcv)
#Train Bosting with Trees Model
modelgbm <- train(classe ~ roll_belt + pitch_belt + yaw_belt + total_accel_belt + roll_arm + 
                  pitch_arm + yaw_arm + total_accel_arm + roll_dumbbell + pitch_dumbbell + 
                  yaw_dumbbell + total_accel_dumbbell + roll_forearm + pitch_forearm + 
                  yaw_forearm + total_accel_forearm, data = training, method = "gbm",
                  trControl = controlcv)
#Train Naive Bayes Model
modelnb <- train(classe ~ roll_belt + pitch_belt + yaw_belt + total_accel_belt + roll_arm + 
                  pitch_arm + yaw_arm + total_accel_arm + roll_dumbbell + pitch_dumbbell + 
                  yaw_dumbbell + total_accel_dumbbell + roll_forearm + pitch_forearm + 
                  yaw_forearm + total_accel_forearm, data = training, method = "nb",
                  trControl = controlcv)
#Predictions on validation set
predtree <- predict(modeltree, validation)
predrf <- predict(modelrf, validation)
predgbm <- predict(modelgbm, validation)
prednb <- predict(modelnb, validation)
#Out of sample error rates with caret confusionMatrix
validation$predtreeRight <- predtree==validation$classe
cmtree <- table(predtree, validation$classe)
validation$predrfRight <- predrf==validation$classe
cmrf <- table(predrf, validation$classe)
validation$predgbmRight <- predgbm==validation$classe
cmgbm <- table(predgbm, validation$classe)
validation$predtnbRight <- prednb==validation$classe
cmnb <- table(prednb, validation$classe)
confusionMatrix(cmtree)
confusionMatrix(cmrf)
confusionMatrix(cmgbm)
confusionMatrix(cmnb)
