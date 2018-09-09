rm(list=ls())
setwd("F:/Ram/Data Science/GitHub/Learning-DataScience/R Programming/Edx - The Analytics Edge/Logistic Regression/Songs")

data_source = read.csv("songs.csv")
# summary(data_source)
# head(data_source)
# str(data_source)
# View(data_source)

# table(data_source$year == "2010")
# subset(data_source,data_source$artistname == "Michael Jackson")$songtitle
# nrow(subset(data_source,data_source$artistname == "Michael Jackson"))
# subset(data_source,data_source$artistname == "Michael Jackson" & Top10 == 1)$songtitle
# table(data_source$timesignature)
# data_source$songtitle[which.max(data_source$tempo)]
# table(data_source$year)

data_train = subset(data_source, data_source$year != 2010)
data_test = subset(data_source, data_source$year == 2010)

# model_log1 = glm(Top10 ~ . -year -songtitle -songID -artistID -artistname, data = data_train, family = binomial)
# summary(model_log1)
# cor(data_train$loudness,data_train$energy)
# 
# model_log2 = glm(Top10 ~ . -loudness -year -songtitle -songID -artistID -artistname, data = data_train, family = binomial)
# summary(model_log2)

nonvars = c("year","songtitle","artistname","artistID","songID","energy")
data_train_nums = data_train[,!(names(data_train) %in% nonvars)]
data_test_nums = data_test[,!(names(data_test) %in% nonvars)]
str(data_train_nums)
str(data_test_nums)

model_log3 = glm(Top10~., data = data_train_nums, family = binomial)
summary(model_log3)

pred_top10 = predict(model_log3, newdata = data_test_nums, type = "response")

str(pred_top10)

table(data_test_nums$Top10,pred_top10>0.45)
table(pred_top10>0.45)

Accuracy = (309+19)/(309+19+40+5)
Accuracy

sensitivity = 19/(19+40)
sensitivity

specificity = 309/(309+5)
specificity


table(data_test$Top10)
AccuracyB = (314)/(314+59)
AccuracyB
