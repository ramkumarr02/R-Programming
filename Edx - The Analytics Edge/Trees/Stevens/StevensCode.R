#########################################################################
#Stevens US Court - Trees....................................
#########################################################################


#-------------------------------------------------------------------------
#Env setup....................................

    rm(list = ls())
    setwd("F:/Ram/Data Science/GitHub/Data-Science/R Programming/Edx - The Analytics Edge/Trees/Stevens")
#-------------------------------------------------------------------------

  
#-------------------------------------------------------------------------
#Data Setup....................................
  
    data_source = read.csv("stevens.csv")
    library(caTools)
    set.seed(3000)      
    split = sample.split(data_source$Reverse,SplitRatio = 0.7)
    data_train = subset(data_source,split == TRUE)
    data_test = subset(data_source,split == FALSE)
    
    View(data_train)    
#-------------------------------------------------------------------------
  
    
#-------------------------------------------------------------------------
#Modelling....................................
    # install.packages("rpart")
    # install.packages("rpart.plot")
    library(rpart)
    library(rpart.plot)
    
    summary(data_train)
    
    model_rpart1 = rpart(Reverse ~ Circuit 
                              +Issue 
                              +Petitioner 
                              +Respondent 
                              +LowerCourt 
                              +Unconst, 
                         data = data_train, 
                         method = "class", 
                         minbucket = 100)
    prp(model_rpart1)
#-------------------------------------------------------------------------

    
#-------------------------------------------------------------------------
#Predict....................................

    pred_rpart1 = predict(model_rpart1,
                          newdata = data_test,
                          type = "class")
    
    table(data_test$Reverse,pred_rpart1)
#-------------------------------------------------------------------------

    
#-------------------------------------------------------------------------    
#ROC Curve....................................
    
    library(ROCR)
    pred_rpart1_reg = predict(model_rpart1,newdata = data_test)

    rocr_pred = prediction(pred_rpart1_reg[,2],data_test$Reverse)
    rocr_perf = performance(rocr_pred,"tpr","fpr")    
  
    plot(rocr_perf)  
   
    performance(rocr_pred,"auc")@y.values
    
    
#-------------------------------------------------------------------------    
    
    
    