# install library

install.packages("ISLR");
install.packages("here")

# Load library
library(ISLR)
library(here)
library(leaps)

# Load scripts
source(here::here('Documents/DS/ISLR-exercise/R/Utils', 'omit_missing_values.R'));

# remove the missing values row
names(Hitters);
summary(Hitters);
dim(Hitters);
Hitters = omit_missing_values(Hitters, Hitters$Salary);
attach(Hitters);

# Best Subset Selection
regfit.full=regsubsets(Salary~.,Hitters,nvmax = 19);
regfit.summary=summary(regfit.full);
names(regfit.summary)

par(mfrow=c(2,2))
plot(regfit.summary$rss, xlab="Number of variables", ylab = "RSS", type="l")
plot(regfit.summary$rsq,xlab="Number of variables", ylab = "RSQ", type="l")
max.adjr2=which.max(regfit.summary$adjr2)
points(max.adjr2, regfit.summary$adjr2[max.adjr2], col="red", cex=2, pch=20)

plot(regfit.summary$cp, xlab = "Number of variables", ylab="CP", type="l")
min.cp=which.min(regfit.summary$cp)
points(min.cp, regfit.summary$cp[min.cp],col="green",cex=2,pch=20)

plot(regfit.summary$bic, xlab="Number of variables", ylab="BIC", type = "l")
min.bic=which.min(regfit.summary$bic)
points(min.bic, regfit.summary$bic[min.bic], col="blue", cex=2, pch=20)

plot(regfit.full, scale="r2");

# Forward and Backward stepwise Selection

regfit.backward=regsubsets(Salary~., data=Hitters, method = "backward", nvmax = 19)
regfit.back.summary=summary(regfit.backward)
plot(regfit.back.summary$adjr2, xlab = "Number of varibales", ylab= "adjr2", type="l")
points(which.max(regfit.back.summary$adjr2), regfit.back.summary$adjr2[which.max(regfit.back.summary$adjr2)], col="red", cex=2, pch=20)

regfit.forward=regsubsets(Salary~.,data=Hitters, method = "forward",nvmax = 19)
summary(regfit.forward)

coef(regfit.forward, 7)
coef(regfit.forward,7)
coef(regfit.full,7)









