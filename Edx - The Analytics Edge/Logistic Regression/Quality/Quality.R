rm(list = ls())
setwd("F:/Ram/Data Science/GitHub/Learning-DataScience/R/Edx - The Analytics Edge/Logistic Regression/Quality")

data_source = read.csv("quality.csv")
View(data_source)

# install.packages("caTools")

library("caTools")

set.seed(1)
split = sample.split(data_source$PoorCare, SplitRatio = 3/4)

data_train = subset(data_source,split == TRUE)
data_test = subset(data_source,split == FALSE)

attach(data_train)

model_log1 = glm(PoorCare ~ Narcotics + OfficeVisits, data = data_train, family = binomial)
summary(model_log1)

prediction_train = predict(model_log1,type = "response")

tapply(prediction_train,data_train$PoorCare,mean)

##############################################################################

rm(list = ls())
setwd("F:/Ram/Data Science/GitHub/Learning-DataScience/R/Edx - The Analytics Edge/Logistic Regression/Quality")

data_source = read.csv("quality.csv")
View(data_source)

# install.packages("caTools")

library("caTools")

set.seed(88)
split = sample.split(data_source$PoorCare, SplitRatio = 3/4)

data_train = subset(data_source,split == TRUE)
data_test = subset(data_source,split == FALSE)

attach(data_train)
  model_log2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = data_train, family = binomial())
detach(data_train)

summary(model_log2)

prediction_train2 = predict(model_log2, type = "response")

tapply(prediction_train2,StartedOnCombination, mean)

table(data_train$PoorCare,prediction_train2 > 0.5)
