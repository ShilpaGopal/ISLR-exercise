# 8 Simple linear regression

Auto= read.csv("/Users/gshilpa/Documents/DS/r-sample/Auto.csv", header = T, na.strings = "?")
Auto=na.omit(Auto)
summary(Auto)

attach(Auto)

lm.fit=lm(mpg~horsepower,Auto)
summary(lm.fit)

predict(lm.fit, data.frame(horsepower=c(98), interval="confidence"))
predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")

plot(horsepower,mpg)
abline(lm.fit, col="red")

par(mfrow=c(2,2))
plot(lm.fit)

# 9 Multiple Linear regression
##scaterplot
pairs(Auto)
##correlation
cor(subset(Auto, select =-name))
##regression 
lm.fit1=lm(mpg~.-name, Auto)
summary(lm.fit1)

# Analysis
par(mfrow=c(2,2))
plot(lm.fit1)
plot(predict(lm.fit1), rstudent(lm.fit1))

#Interaction
lm.fit2 = lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.fit2)

#Transformation
lm.fit3 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)

plot(predict(lm.fit3), rstudent(lm.fit3))


lm.fit2<-lm(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
summary(lm.fit2)

par(mfrow=c(2,2)) 
plot(lm.fit2)

plot(predict(lm.fit2), rstudent(lm.fit2))

#10

library(ISLR)
attach(Carseats)
names(Carseats)

lm.fit=lm(Sales~Price+Urban+US,Carseats)
summary(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)

lm.fit=lm(Sales~Price+US,Carseats)
summary(lm.fit)
confint(lm.fit)

plot(predict(lm.fit), rstudent(lm.fit))
par(mfrow=c(2,2))
plot(lm.fit)

#11

set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)

lm.fit=lm(y~x+0)
summary(lm.fit)

lm.fit = lm(x~y+0)
summary(lm.fit)

lm.fit=lm(y~x)
summary(lm.fit)
lm.fit2 = lm(x~y)
summary(lm.fit2)

#12
set.seed(1)
x = rnorm(100)
y = 2*x
lm.fit = lm(y~x+0)
lm.fit2 = lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)

set.seed(1)
x=rnorm(100)
y=-sample(x,100)
sum(x^2)
sum(y^2)
lm.fit <- lm(y~x+0)
lm.fit2 <- lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)

#13

set.seed(1)
x=rnorm(100)
eps=rnorm(100, 0, sqrt(0.25))

y=-1+0.5*x+eps

summary(y)
plot(x,y)

lm.fit=lm(y~x)
summary(lm.fit)

plot(x,y)
abline(lm.fit,lwd=3 ,col="red")
abline(-1, 0.5, lwd=3, col=3)

legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

lm.fitsq=lm(y~x+I(x^2))
summary(lm.fitsq)



set.seed(1)
eps1 = rnorm(100, 0, 0.125)
x1 = rnorm(100)
y1 = -1 + 0.5*x1 + eps1
plot(x1, y1)
lm.fit1 = lm(y1~x1)
summary(lm.fit1)
abline(lm.fit1, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)


set.seed(1)
eps2 = rnorm(100, 0, 0.5)
x2 = rnorm(100)
y2 = -1 + 0.5*x2 + eps2
plot(x2, y2)
lm.fit2 = lm(y2~x2)
summary(lm.fit2)

abline(lm.fit2, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

confint(lm.fit)
confint(lm.fit2)
confint(lm.fit1)

#14 collinearity

set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)

cor(x1,x2)
plot(x1,x2)

lm.fit = lm(y~x1+x2)
summary(lm.fit)

lm.fit = lm(y~x1)
summary(lm.fit)

lm.fit = lm(y~x2)
summary(lm.fit)

x1=c(x1,0.1)
x2=c(x2,0.8)
y=c(y,6)

lm.fit1 = lm(y~x1+x2)
summary(lm.fit1)
lm.fit2 = lm(y~x1)
summary(lm.fit2)
lm.fit3 = lm(y~x2)
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit1)
par(mfrow=c(2,2))
plot(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit3)
plot(predict(lm.fit1), rstudent(lm.fit1))
plot(predict(lm.fit2), rstudent(lm.fit2))
plot(predict(lm.fit3), rstudent(lm.fit3))

#15

library(MASS)
summary(Boston)
attach(Boston)

chas <- factor(chas, labels = c("N","Y"))
summary(Boston)


lm.zn = lm(crim~zn)
summary(lm.zn)
lm.indus = lm(crim~indus)
summary(lm.indus)
lm.chas = lm(crim~chas) 
summary(lm.chas)
lm.nox = lm(crim~nox)
summary(lm.nox)
lm.rm = lm(crim~rm)
summary(lm.rm) 
lm.age = lm(crim~age)
summary(lm.age)
lm.dis = lm(crim~dis)
summary(lm.dis)
lm.rad = lm(crim~rad)
summary(lm.rad)
lm.tax = lm(crim~tax)
summary(lm.tax) 
lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio)
lm.black = lm(crim~black)
summary(lm.black)
lm.lstat = lm(crim~lstat)
summary(lm.lstat)
lm.medv = lm(crim~medv)
summary(lm.medv)

lm.fit=lm(crim~.,Boston)
summary(lm.fit)

x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.fit)[2:14]
plot(x,y)


lm.zn = lm(crim~poly(zn,3))
summary(lm.zn)
lm.indus = lm(crim~poly(indus,3))
summary(lm.indus)









