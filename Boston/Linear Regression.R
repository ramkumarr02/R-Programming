library(MASS)
library(ggplot2)
head(Boston)

fix(Boston)

#pairs(Boston, upper.panel = NULL)

rownum = seq(1, nrow(Boston),1)

set.seed(1234)

train_rows = sample(rownum, 0.7*nrow(Boston))

Train = Boston[train_rows,]

Test = Boston[-train_rows,]

nrow(Train) 
nrow(Test)
nrow(Train) + nrow(Test)

plot(Boston$medv, Boston$lstat)

lmodel1 = lm(medv~lstat, data = Train)

summary(lmodel1)

names(lmodel1)

lmodel1$coefficients

Train_Residuals = lmodel1$residuals

MAPE = mean(abs(Train_Residuals/Train$medv))
MAPE

coef(lmodel1)

plot(Train$medv~Train$lstat)
abline(lmodel1, lwd =2, col = "red", lty = 2)


ggplot(data = Train, aes(x = medv, y = lstat)) + 
  geom_point() + 
  stat_smooth(method = "lmodel1")

Test$results = predict(lmodel1, Test)
head(Test)

Test$Error = Test$medv - Test$results

MAPE = mean(abs(Test$Error/Test$medv))
MAPE


# lmodel1 = lm(data = Train, medv~.)
# summary(lmodel1)
# 
# lmodel1 = lm(data = Train, medv~. -age)
# summary(lmodel1)
# 
# lmodel1 = lm(data = Train, medv~. -age -indus -crim)
# summary(lmodel1)


lmodel1 = lm(data = Train, medv~. -age -indus)
summary(lmodel1)

lmodel1 = lm(data = Train, medv~. +lstat*age -indus)
summary(lmodel1)


plot(lmodel1$residuals)

lmodel1$residuals

par(mfrow = c(2,2))
plot(lmodel1)

names(lmodel1)


par(mfrow = c(2,2))
hist(lmodel1$residuals, probability = TRUE)
m = mean(lmodel1$residuals)
sd = sd(lmodel1$residuals)
curve(dnorm(x, mean=m, sd=s), add = TRUE)

stuRes = studres(lmodel1)
hist(stuRes, probability = TRUE)
m = mean(stuRes)
s = sd(stuRes)
curve(dnorm(x, mean=m, sd=s), add =TRUE)

update.packages(ask = FALSE, repos = "https://cloud.r-project.org")

install.packages('https://cran.r-project.org/bin/windows/contrib/3.5/car_2.1-6.zip',repos = NULL)
install.packages('https://cran.r-project.org/bin/windows/contrib/3.5/pbkrtest_0.4-7.zip',repos = NULL)
install.packages('https://cran.r-project.org/bin/windows/contrib/3.5/lme4_1.1-14.zip',repos = NULL)
install.packages('https://cran.r-project.org/bin/windows/contrib/3.5/minqa_1.2.4.zip',repos = NULL)
install.packages('https://cran.r-project.org/bin/windows/contrib/3.5/nloptr_1.0.4.zip',repos = NULL)
install.packages('https://cran.r-project.org/bin/windows/contrib/3.5/quantreg_5.34.zip',repos = NULL)
install.packages('https://cran.r-project.org/bin/windows/contrib/3.5/SparseM_1.77.zip',repos = NULL)
install.packages('https://cran.r-project.org/bin/windows/contrib/3.5/MatrixModels_0.4-1.zip',repos = NULL)

available.packages()[,1]

library(car)

ncvTest(lmodel1)

vif(lmodel1)

installed.packages()[,1]
install.packages("Rcpp")

library(Rcpp)
?ncvTest
