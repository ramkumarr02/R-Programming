rm(list=ls())

setwd("F:/Ram/Data Science/GitHub/Learning-DataScience/R Programming/Edx - The Analytics Edge/Logistic Regression/The Framingham Heart study")

data_source = read.csv("framingham.csv")

library("caTools")

set.seed(1000)

split = sample.split(data_source$TenYearCHD,0.65)

data_train = subset(data_source,split == TRUE)
data_test = subset(data_source,split == FALSE)

attach(data_train)

  model_log1 = glm(TenYearCHD ~ .,data = data_train)

detach(data_train)
  
summary(model_log1)  

prediction_test = predict(model_log1,newdata = data_test)

table(data_test$TenYearCHD, prediction_test > 0.5)

# install.packages("ROCR")
library(ROCR)

rocr_pred =  prediction(prediction_test,data_test$TenYearCHD)
rocr_perf = performance(rocr_pred,"auc")
as.numeric(rocr_perf@y.values)
