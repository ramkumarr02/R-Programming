# Env Prep #############
    rm(list = ls())
    setwd("F:/Ram/Data Science/GitHub/Data-Science/R Programming/Kaggle/Caret cross validation")
########################

      
      
#Data prep #############

    # install.packages("C50")
    library(C50)
    library(caret)
    
    data(churn)
    
    train_predictors = colnames(churnTrain)[colnames(churnTrain) != "churn"]
    
    set.seed(1)
    data_split = createDataPartition(allData$churn,p = 0.75,list = FALSE)
    
    
    data_source = read.csv("train.csv")
    data_target = read.csv("test.csv")
##########################

    
    
#Remove text columns that aren't useful for the model#############
    colnames(data_source)
    data_source_sub = data_source[,c(
                                   # "PassengerId"
                                   "Survived"
                                   ,"Pclass"
                                   # ,"Name"
                                   ,"Sex"
                                   ,"Age"
                                   ,"SibSp"
                                   ,"Parch"
                                   # ,"Ticket"
                                   ,"Fare"
                                   # ,"Cabin"
                                   ,"Embarked"
                                   )]

    str(data_source_sub)
#################################################################
  
    
    
#Split Data source into training and testing sets using caret packaged##########################
    library(caret)
    caret_data_split = createDataPartition(data_source_sub$Survived,
                                           p = 0.75,
                                           list = FALSE)

    str(caret_data_split)

    data_train = data_source_sub[caret_data_split,]
    data_test = data_source_sub[-caret_data_split,]
##############################################################################


    
# Impute ##############################

    library(DMwR)
    data_train_imp = centralImputation(data_train)
    data_test_imp = centralImputation(data_test)
    
################################################


    
# Caret Arguments ##############################
    ctrl = trainControl(method = "boot"
                        ,repeats = 2
                        ,predictionBounds = c(TRUE,FALSE)
                        )      
    summary(ctrl)
################################################
    
    
    
# Caret Model ##############################

    # getModelInfo("rf")
    
    # model_log1= glm(Survived ~ .,data = data_train_imp, family = binomial)
    # summary(model_log1)

    caret_fit = train(Survived ~ .,
                      data = data_train_imp
                      ,method = "glm"
                      # ,family = binomial
                      ,tunelength = 1
                      ,trcontrol = ctrl()
                      # ,metric = "ROC"
                      # ,preProcess = c("center","scale")
                      )

      
################################################