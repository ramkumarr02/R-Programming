# Infra and Data Setup
###################################################################
    rm(list=ls())
    # data_train = read.csv("../input/train.csv")
    # data_test = read.csv("../input/test.csv")
    setwd("F:/Ram/Data Science/GitHub/Data-Science/R Programming/Kaggle/Titanic")
    data_train = read.csv("train1.csv")
    data_test = read.csv("test1.csv")
    data_train$Survived = as.factor(as.character(data_train$Survived))
    data_train$Cabin.Count = as.numeric(as.character(data_train$Cabin.Count))
    data_test$Cabin.Count = as.numeric(as.character(data_test$Cabin.Count))
    
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

    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    
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
                                      # "Cabin.Category",
                                      # "Cabin.Count",
                                      # "Cabin",
                                      "Embarked"
                                      
    )]
    
    data_test_subset = data_test[,c("Pclass",
                                    "Sex",
                                    "SibSp",
                                    "Parch",
                                    "Age",
                                    "Fare",
                                    # "Cabin.Category",
                                    # "Cabin.Count",
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
        
        model_rf1$importance
    
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
            ensemble_data = data.frame(PassengerId,predict_model_rf1)
        detach(data_test)
        
        write.csv(outputfile_Imputed_Data,"rf_outputfile_Imputed_Data.csv",row.names = FALSE)

###########################################################################
        
        
# Logistic modelling
########################################################
        
        model_log1 = glm(Survived ~ .,
                         data = data_train1,
                         family = binomial()
                         )

        
        # Predict for internal data and measure accuracy
        ########################################################
        
        predict_model_log1 = predict(model_log1, newdata = data_train2, type = "response")
        
        predict_model_log1 = ifelse(predict_model_log1>0.6,1,0)
        
        table(data_train2$Survived,predict_model_log1)
        
        confusionMatrix2 = confusionMatrix(predict_model_log1,data_train2$Survived)
        confusionMatrix2
        
        model_rf1$importance
        
        # Create logt model with full training data
        ##########################################################
        
        model_log1 = glm(Survived~., 
                         data = data_train_imputed, 
                         family = binomial()
        )
        
        # Predict for testing data and write into file
        ##########################################################
        
        data_test_imputed = centralImputation(data_test_subset)
        
        str(data_train_imputed)
        str(data_test_imputed)
        
        levels(data_test_imputed$Embarked) = levels(data_train_imputed$Embarked)
        
        predict_model_log1 = predict(model_log1, newdata = data_test_imputed, type = "response")
        
        predict_model_log1 = ifelse(predict_model_log1>0.6,1,0)
        
        Survived = predict_model_log1
        attach(data_test)
            outputfile_Imputed_Data = data.frame(PassengerId,Survived)
            ensemble_data$predict_model_log1 = predict_model_log1
        detach(data_test)
        
        write.csv(outputfile_Imputed_Data,"log_outputfile_Imputed_Data.csv",row.names = FALSE)
        
        write.csv(ensemble_data,"Ensemble_Data.csv",row.names = FALSE)
        
###########################################################################
        
        
# Rpart modelling
########################################################

        # install.packages("rpart")
        library(rpart)
                        
        model_rpart1 = rpart(Survived ~ . ,
                            data = data_train1,
                            method = "class"
                            )
        

# Predict for internal data and measure accuracy
########################################################

        predict_model_rpart1 = predict(model_rpart1, newdata = data_train2, type = "class")
        table(data_train2$Survived,predict_model_rpart1)
        
        # predict_model_log1 = ifelse(predict_model_log1>0.6,1,0)
        
        confusionMatrix2 = confusionMatrix(predict_model_rpart1,data_train2$Survived)
        confusionMatrix2

# Create logt model with full training data
##########################################################

        model_rpart1 = rpart(Survived~., 
                         data = data_train_imputed, 
                         method = "class"
        )

# Predict for testing data and write into file
##########################################################
        
        data_test_imputed = centralImputation(data_test_subset)
        
        str(data_train_imputed)
        str(data_test_imputed)
        
        levels(data_test_imputed$Embarked) = levels(data_train_imputed$Embarked)
        
        predict_model_rpart1 = predict(model_rpart1, newdata = data_test_imputed, type = "class")
        
        # predict_model_log1 = ifelse(predict_model_log1>0.6,1,0)
        
        Survived = predict_model_rpart1
        attach(data_test)
        outputfile_Imputed_Data = data.frame(PassengerId,Survived)
        ensemble_data$predict_model_rpart1 = predict_model_rpart1
        detach(data_test)
        
        write.csv(outputfile_Imputed_Data,"rpart_outputfile_Imputed_Data.csv",row.names = FALSE)


###########################################################################


# Ensemble #####################
        
        i = 0
        for (i in 1:nrow(ensemble_data)) 
          {
          
              x = c(predict_model_rf1[i],predict_model_log1[i],predict_model_rpart1[i])
              ensemble_data$Ensemble_value[i] = Mode(x)
              
              i = i +1
          }

        ensemble_data$Ensemble_value = ifelse(ensemble_data$Ensemble_value==1,0,1)
        ensemble_data$Survived = ensemble_data$Ensemble_value
        ensemble_data = ensemble_data[,c("PassengerId","Survived")]
        write.csv(ensemble_data,"Ensemble_Data.csv",row.names = FALSE)
###########################################################################