# Infra and Data Setup
###################################################################
  rm(list=ls())
  # data_train = read.csv("../input/train.csv")
  # data_test = read.csv("../input/test.csv")
  setwd("F:/Ram/Data Science/GitHub/Data-Science/R Programming/Kaggle/Titanic")
  data_train = read.csv("train1.csv")
  data_test = read.csv("test1.csv")
  library(caret)
    

# PreProcessing
###################################################################
  summary(data_train)
  str(data_train)

  # Removing Unnecessary variables
    
    #Training Data set    
      temp_train_PassengerId = data_train$PassengerId
      data_train$Survived = as.factor(data_train$Survived)
      data_train$Pclass = as.factor(data_train$Pclass)
      data_train$PassengerId = NULL
      data_train$Name = NULL
      data_train$Ticket = NULL
      data_train$Cabin = NULL
    
    #Test Data set  
      temp_test_PassengerId = data_test$PassengerId
      data_test$Survived = as.factor(data_test$Survived)
      data_test$Pclass = as.factor(data_test$Pclass)
      data_test$PassengerId = NULL
      data_test$Name = NULL
      data_test$Ticket = NULL
      data_test$Cabin = NULL
  
    #Impute Center and Scale
      model_impute = preProcess(data_train, method = c("knnImpute", "center", "scale"))

     #install.packages("RANN")
      library(RANN)
      data_train_pp = predict(model_impute, newdata = data_train)
      
      sum(is.na(data_train))
      sum(is.na(data_train_pp))

      summary(data_train_pp)
      str(data_train)
      str(data_train_pp)

  # Test Dataset
      
      #Impute Center and Scale
      model_impute_test = preProcess(data_test, method = c("knnImpute", "center", "scale"))
      
      #install.packages("RANN")
      library(RANN)
      data_test_pp = predict(model_impute_test, newdata = data_test)
      
      sum(is.na(data_test))
      sum(is.na(data_test_pp))
      
      summary(data_test_pp)
      str(data_test)
      str(data_test_pp)
      
      
              
# Split Data
###################################################################
  
  index = createDataPartition(data_train_pp$Survived, p = 0.8, list = FALSE)
  
  data_train_pp_train = data_train_pp[index,]
  data_train_pp_test = data_train_pp[-index,]

  str(data_train_pp_train)
  str(data_train_pp_test)
  
# Feature Selection
###################################################################
  outcome = "Survived"
  predictors = names(data_train_pp_train)[!names(data_train_pp_train) %in% outcome]
  
  control_fs = rfeControl(functions = rfFuncs, method = "repeatedCv", repeats = 5, verbose = FALSE)
  
  # install.packages("e1071")
  library(e1071)  
  # install.packages("randomForest")
  library(randomForest)
  
  model_fs = rfe(data_train_pp_train[,predictors], data_train_pp_train[,outcome], rfeControl = control_fs)
  print(model_fs)
  
  predictors = c("Sex","Pclass","Fare","Age")
  
# Train Model & Parameter tuning
###################################################################
  model_rf = train(data_train_pp_train[,predictors], data_train_pp_train[,outcome],method = 'rf', tuneLength = 10)
  summary(model_rf)
  print(model_rf)
  
  # Variable Importance
  ###################################################################
    varImp(model_rf)
    
  # Prediction
  ###################################################################
    prediction = predict.train(object = model_rf, newdata = data_train_pp_test, type = "raw")  
    confusionMatrix(prediction,data_train_pp_test$Survived)


# Train Model with Full Data
###################################################################
  model_rf_full = train(data_train_pp[,predictors], data_train_pp[,outcome],method = 'rf', tuneLength = 10)
  summary(model_rf_full)
  print(model_rf_full)

  # Variable Importance
  ###################################################################
    varImp(model_rf_full)
  
  # Prediction
  ###################################################################
    prediction = predict.train(object = model_rf_full, newdata = data_test_pp, type = "raw")  
    PassengerID = temp_test_PassengerId
    Survived = prediction
    output_file = data.frame(PassengerID,Survived)
    write.csv(output_file,"titanic_caret.csv",row.names = FALSE)
  