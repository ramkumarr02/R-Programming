rm(list=ls())
setwd("F:/Ram/Data Science/GitHub/Learning-DataScience/R Programming/Edx - The Analytics Edge/Logistic Regression/US Elections")

data_source = read.csv("PollingData.csv")

head(data_source)
summary(data_source)

data_small = data_source[c("Rasmussen","SurveyUSA","DiffCount","PropR")]
summary(data_small)

# install.packages("mice")
library("mice")

data_imputed = complete(mice(data_small))
summary(data_imputed)

data_source$Rasmussen = data_imputed$Rasmussen
data_source$SurveyUSA = data_imputed$SurveyUSA

library(caTools)
data_train = subset(data_source,data_source$Year !=2012)
data_test = subset(data_source,data_source$Year ==2012)

attach(data_source)
model_log1 = glm(Republican ~ . -State -Year, data = data_source, family = binomial)
summary(model_log1)

cor(data_train[c("Rasmussen","SurveyUSA","DiffCount","PropR","Republican")])

attach(data_train)
model_log1 = glm(Republican ~ PropR, data = data_train, family = binomial)
summary(model_log1)

pred_data_train = predict(model_log1,type = "response")
table(data_train$Republican,pred_data_train > 0.5)
