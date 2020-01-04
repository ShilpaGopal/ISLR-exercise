library(MASS)
library(ISLR)
install.packages("ISLR")

#View the data
fix(Boston)
attach(Boston)

# View the header 
names(Boston)

# Simple Linear regression
lm.fit=lm(medv~lstat,Boston)
lm.fit

summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=(c(5,10,15)), interval="confidence"))
plot(lstat,medv)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,pch=20)
plot(1:20,1:20,pch=1:20)

# partition the plot
par(mfrow=c(2,2))
plot(lm.fit)

# residual plot

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstandard(lm.fit))

# Leverage statistics 

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple linear regression

lm.mfit=lm(medv~lstat+age, Boston)
summary(lm.mfit)

# using all the predictors

lm.mfit=lm(medv~.,Boston)
summary(lm.mfit)
summary(lm.mfit)$r.sq


# variation inflation factor
install.packages("car")

library(car)
vif(lm.mfit)

lm.fit1=lm(medv~.-age,Boston)
summary(lm.fit1)

lm.fit1 = update(lm.mfit,~.-age)
summary(lm.fit1)

# Interaction term 
lm.intfit = lm(medv~lstat*age, Boston)
summary(lm.intfit)

# Non linear tranformation of the predictors 
lm.fit2=lm(medv~lstat+I(lstat^2), Boston)
summary(lm.fit2)

lm.fit=lm(medv~lstat, Boston)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5=lm(medv~poly(lstat,5), Boston)
par(mfrow=c(2,2))
plot(lm.fit5)

summary(lm(medv~log(rm), Boston))

# Qualitative predictors 

fix(Carseats)
names(Carseats)
attach(Carseats)

lm.fit=lm(Sales~.+Income:Advertising+Price:Age, Carseats)
summary(lm.fit)
contrasts(ShelveLoc)

# Writing functions

LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("SUccessfully loaded the libraries")
}

LoadLibraries()

