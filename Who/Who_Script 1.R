install.packages("plyr")
install.packages("dplyr")
install.packages("data.table")
install.packages("ggplot2")


matrix(seq(1,9,1), nrow = 3, ncol = 3, byrow = TRUE)

Name = c("Ram","Madhu","Rishi")
Age = c(10,11,12)
std = c(1,2,3)
School = data.frame(Name, Age, std)
School

School = read.csv("School.csv")

i = 0
for (i in 1:10){cat("Number",i,"\n")}

i = 0
while(i < 10)
  {
    print(i)
    i = i + 1
  }


squareroot = function(x)
  {
    x = x^2
  }

x = squareroot(4)
x

who = read.csv("who.csv")
str(who)

summary(who$LiteracyRate)


plot(who$Population, who$LiteracyRate)

library(ggplot2)
data("who", "ggplot2")
ggplot(who, aes(x = Population, y = LiteracyRate)) + geom_point()


str(who)

whoNumeric = data.frame(
  who$Under15,
  who$Over60,
  who$FertilityRate,
  who$LifeExpectancy,
  who$ChildMortality,
  who$CellularSubscribers,
  who$LiteracyRate,
  who$PrimarySchoolEnrollmentFemale,
  who$PrimarySchoolEnrollmentMale)

str(whoNumeric)

cor(who$ChildMortality, who$LifeExpectancy)
cor(who$ChildMortality, who$FertilityRate)

ggplot(data = who, aes(x = ChildMortality, y = LifeExpectancy )) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  coord_cartesian(xlim = c(0,50), ylim = c(65, 80)) + 
  ggtitle("Child Mortality vs Life Expectancy", subtitle = "Inversely Proportionate") + 
  xlab("Child Mortality Rate")
  ylab("Life Expectancy")

str(who)

x = lm(who$ChildMortality ~ who$LifeExpectancy)
summary(x)

