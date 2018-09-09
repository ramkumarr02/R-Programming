# Infra and Data Setup
###################################################################
setwd("F:/Ram/Data Science/GitHub/Data-Science/R Programming/Kaggle/Deodorant")

rm(list=ls(all=TRUE))
# install.packages("arm")


library(caret)
library(arm)

data_train = read.csv("Data_train_reduced.csv", header = TRUE, sep=",")
data_test = read.csv("test_data.csv", header = TRUE, sep=",")

# data_train = read.table("../input/Data_train_reduced.csv", header = TRUE, sep=",")
# data_test = read.table("../input/test_data.csv", header = TRUE, sep=",")
summary(data_train)


# Removing some of the unnecessary text fields in train and test data
###################################################################   
data_train$Product.ID = NULL
data_train$Respondent.ID = NULL
data_train$s7.involved.in.the.selection.of.the.cosmetic.products = NULL
data_train$q8.3 = NULL
data_train$q8.4 = NULL
data_train$q8.14 = NULL
data_train$q8.15 = NULL
data_train$q8.16 = NULL
data_train$q8.2 = NULL
data_train$q8.7 = NULL
data_train$q8.8 = NULL
data_train$q8.9 = NULL
data_train$q8.10 = NULL
data_train$q8.12 = NULL
data_train$q8.17 = NULL
data_train$q8.18 = NULL
data_train$q8.20 = NULL

str(data_train)
summary(data_train)

######################################################################################################################################
# Linear Regression 
######################################################################################################################################
# Used Linear Regression Regression to understand the Data and to do some basic analyses. 
# But with several factors into consideration, Log Regression is more suitable and it did produce 100% result, whereas the Linear regression only produced 96% accuracy


# Data Split
###################################################################  
indexes <- sample(1:nrow(data_train), size = 0.7*nrow(data_train))
train <- data_train[indexes,]
test <- data_train[-indexes,]


# Linear Regression Modelling
###################################################################       

#   Normal Linear Regression
############################
model_linear_1 = lm(Instant.Liking~.,data=train,na.action = na.exclude)
summary(model_linear_1)

#   Step Linear Regression
############################
model_steplin_1 = step(lm(Instant.Liking~.,data=train),direction="both")
summary(model_steplin_1)

# Prediction on steplin model with OOB partial Test Dataset
###################################################################  
predict_model_steplin_1 = predict(model_steplin_1, newdata = test)
predict_model_steplin_1 = ifelse(predict_model_steplin_1>0.5,1,0)
test$predicted = predict_model_steplin_1

str(test$Instant.Liking)
str(test$predicted)
write.csv(test,"sampletest.csv")

confusionMatrix(as.integer(test$Instant.Liking),as.integer(test$predicted))
#   96% Accuracy

######################################################################################################################################
# Step Linear Regression 
######################################################################################################################################

# Step Linear Regression With Full data
###################################################################       
model_steplin_2 = step(lm(Instant.Liking~.,data = data_train),direction="both")
summary(model_steplin_2)

# Prediction on steplin model with Complete Test Dataset
###################################################################       
predict_model_steplin_2 = predict(model_steplin_2, newdata =  data_test)
predict_model_steplin_2 = ifelse(predict_model_steplin_2 > 0.5, 1, 0)
steplin_result_2 = data.frame(data_test$Respondent.ID, data_test$Product, predict_model_steplin_2)
summary(steplin_result_2)

write.csv(steplin_result_2, "Steplin.csv")
#   96% Accuracy (Compared with the actual solution)

######################################################################################################################################
# Logistic Regression 
######################################################################################################################################

# Convert Required vectors to factors
###################################################################    

# Train Data set
data_train$Instant.Liking = as.factor(data_train$Instant.Liking)
data_train$q11.time.of.day.would.this.Deodorant.be.appropriate = as.factor(data_train$q11.time.of.day.would.this.Deodorant.be.appropriate)
data_train$q12.which.occasions.would.this.Deodorant.be.appropriate = as.factor(data_train$q12.which.occasions.would.this.Deodorant.be.appropriate)
data_train$q2_all.words = as.factor(data_train$q2_all.words)
data_train$q8.1 = as.factor(data_train$q8.1)
data_train$q8.11 = as.factor(data_train$q8.11)
data_train$q8.13 = as.factor(data_train$q8.13)
data_train$q8.19 = as.factor(data_train$q8.19)
data_train$q8.5 = as.factor(data_train$q8.5)
data_train$q8.6 = as.factor(data_train$q8.6)
data_train$s11.marital.status = as.factor(data_train$s11.marital.status)
data_train$s12.working.status = as.factor(data_train$s12.working.status)
data_train$s13a.b.most.often = as.factor(data_train$s13a.b.most.often)
data_train$s8.ethnic.background = as.factor(data_train$s8.ethnic.background)
data_train$ValSegb = as.factor(data_train$ValSegb)

# Test Data Set
data_test$q11.time.of.day.would.this.Deodorant.be.appropriate = as.factor(data_test$q11.time.of.day.would.this.Deodorant.be.appropriate)
data_test$q12.which.occasions.would.this.Deodorant.be.appropriate = as.factor(data_test$q12.which.occasions.would.this.Deodorant.be.appropriate)
data_test$q2_all.words = as.factor(data_test$q2_all.words)
data_test$q8.1 = as.factor(data_test$q8.1)
data_test$q8.11 = as.factor(data_test$q8.11)
data_test$q8.13 = as.factor(data_test$q8.13)
data_test$q8.19 = as.factor(data_test$q8.19)
data_test$q8.5 = as.factor(data_test$q8.5)
data_test$q8.6 = as.factor(data_test$q8.6)
data_test$s11.marital.status = as.factor(data_test$s11.marital.status)
data_test$s12.working.status = as.factor(data_test$s12.working.status)
data_test$s13a.b.most.often = as.factor(data_test$s13a.b.most.often)
data_test$s8.ethnic.background = as.factor(data_test$s8.ethnic.background)
data_test$ValSegb = as.factor(data_test$ValSegb)



# Logistic Regression Modelling
###################################################################       

model_log_1 = glm(Instant.Liking~.,data=data_train,family = binomial(), control = list(maxit = 50))
summary(model_log_1)


# Log prediction With Full data 
###################################################################       
predict_model_log_1 = predict(model_log_1, newdata =  data_test)
predict_model_log_1= ifelse(predict_model_log_1> 0.5, 1, 0)
log_result_1 = data.frame(data_test$Respondent.ID, data_test$Product, predict_model_log_1)
summary(log_result_1)
write.csv(log_result_1, "log.csv")