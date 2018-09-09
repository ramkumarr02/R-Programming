rm(list = ls())

setwd("F:/Ram/Data Science/GitHub/Learning-DataScience/R/Edx - The Analytics Edge/Flu Dynamics")

flutrain = read.csv("FluTrain.csv")

head(flutrain)

View(flutrain)

attach(flutrain)

Week[which.max(ILI)]

Week[which.max(Queries)]

hist(ILI)

boxplot(ILI)

lILI = log(ILI)

plot(lILI ~ Queries)

lmodel1 = lm(log(ILI)~ Queries, data = flutrain)
summary(lmodel1)

cor(ILI,Queries)

c = cor(log(ILI),Queries)

exp(-0.5*c)

log(1/c)

c^2

flutest = read.csv("FluTest.csv")

predtest = exp(predict(lmodel1, newdata = flutest))

View(flutest)

n = which(flutest$Week == "2012-03-11 - 2012-03-17")

p = predtest[n]
o = flutest[n,2]

relative_error = (o - p)/o

E = predtest - flutest$ILI
SE = E^2
MSE = mean(SE)
RMSE = sqrt(MSE)

# install.packages("zoo")
# 
# library(zoo)

ililag2 = lag(zoo(flutrain$ILI),-2,na.pad = TRUE)

flutrain$ILIlag2 = coredata(ililag2)

View(flutrain)

attach(flutrain)
plot(log(ILIlag2) ~ log(ILI))


lmodel2 = lm(log(ILI) ~ Queries + log(ILIlag2), data = flutrain)

summary(lmodel2)


# Creating a timelag for test data

attach(flutest)
flutest$ILIlag2 = coredata(lag(zoo(flutest$ILI),-2,na.pad = TRUE))

summary(flutest)

View(flutrain)
View(flutest)

flutest$ILIlag2[1] = flutrain$ILI[nrow(flutrain)-1]
flutest$ILIlag2[2] = flutrain$ILI[nrow(flutrain)]

predILI = exp(predict(lmodel2, newdata = flutest))

E = predILI - flutest$ILI
SE = SE^2
SSE = sum(SE)
MSE = mean(SE)
RMSE = sqrt(MSE)
