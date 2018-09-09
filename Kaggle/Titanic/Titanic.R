# Infra and Data Setup
###################################################################
    rm(list=ls())
    # data_train = read.csv("../input/train.csv")
    setwd("F:/Ram/Data Science/GitHub/Data-Science/R Programming/Kaggle/Titanic")
    data_train = read.csv("train.csv")
    data_test = read.csv("test.csv")
    data_train$Survived = as.factor(as.character(data_train$Survived))


    library(mice)
      # md.pattern(data_train)
      # summary(data_train)
      # data_train_subset = data_train[,c(2,3,5,6,7,9,11,12)]
    library(caTools)
      # install.packages("randomForest")
    library(randomForest)
      # install.packages("caret")
    library(caret)
      # install.packages("e1071")
    library(e1071)

    # colnames(data_train)
    # View(data_train)

    
# Removing some of the unnecessary text fields in train and test data
###################################################################
    
    set.seed(1)
    
    data_train_subset = data_train[,c("Survived",
                                      "Pclass",
                                      "Sex",
                                      "SibSp",
                                      "Parch",
                                      "Age",
                                      "Fare",
                                      # "Cabin",
                                      "Embarked"
                                      
    )]
    
    data_test_subset = data_test[,c("Pclass",
                                    "Sex",
                                    "SibSp",
                                    "Parch",
                                    "Age",
                                    "Fare",
                                    # "Cabin",
                                    "Embarked"
    )]
    

# Impute Train data and split it for internal testing
######################################################

    library(DMwR)
    data_train_imputed = centralImputation(data_train_subset)
    
    summary(data_train_subset)
    summary(data_train_imputed)
    
    split_data = sample.split(data_train_imputed,SplitRatio = 0.7)
    data_train1 = subset(data_train_imputed, split_data == TRUE)
    data_train2 = subset(data_train_imputed, split_data == FALSE)
    summary(data_train1)


# Random Forest modelling
########################################################

    model_rf1 = randomForest(Survived~., 
                             data = data_train1, 
                             # mtry=3, 
                             ntree = 500,
                             importance = TRUE
                             )

# Predict for internal data and measure accuracy
########################################################

    predict_model_rf1 = predict(model_rf1, newdata = data_train2, type = "class")
    table(data_train2$Survived,predict_model_rf1)
    confusionMatrix2 = confusionMatrix(predict_model_rf1,data_train2$Survived)
    confusionMatrix2

# Create Random forest model with full training data
##########################################################

    model_rf1 = randomForest(Survived~., 
                             data = data_train_imputed, 
                             # mtry=3, 
                             ntree = 55,
                             importance = TRUE
                              )

# Predict for testing data and write into file
##########################################################

    data_test_imputed = centralImputation(data_test_subset)
    
    str(data_train_imputed)
    str(data_test_imputed)
    
    levels(data_test_imputed$Embarked) = levels(data_train_imputed$Embarked)
    
    predict_model_rf1 = predict(model_rf1, newdata = data_test_imputed, type = "class")
    
    Survived = predict_model_rf1
    attach(data_test)
    outputfile_Imputed_Data = data.frame(PassengerId,Survived)
    detach(data_test)
    
    write.csv(outputfile_Imputed_Data,"outputfile_Imputed_Data.csv",row.names = FALSE)

###################################################################