# *************************************************************************************
# Demographics and employment stats of US
# *************************************************************************************
setwd("F:/Ram/Data Science/GitHub/Learning-DataScience/R/Edx - The Analytics Edge/Demographics & Employment in US")
cps = read.csv("CPSData.csv")
summary(cps)
str(cps)
head(cps)

sort(table(cps$Industry),decreasing = FALSE)

sort(table(cps$State),decreasing = FALSE)

sort(table(cps$Citizenship),decreasing = FALSE)

(116639 + 7073)/(116639 + 7073 + 7590)

attach(cps)

table(cps$Race, cps$Hispanic)

is.na(Married)
table(Region, is.na(Married))
table(Sex,is.na(Married))
table(Age,is.na(Married))
table(Citizenship,is.na(Married))

fix(cps)

table(State,is.na(MetroAreaCode))

table(Region,is.na(MetroAreaCode))

tapply(Age,Sex,mean)

list = tapply(is.na(MetroAreaCode),State,mean)

sort(list)
