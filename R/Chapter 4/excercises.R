library(ISLR)
attach(Weekly)

summary(Weekly)
pairs(Weekly, col=Weekly$Direction)
cor(Weekly[,-9])

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,Weekly, family = binomial)
summary(glm.fit)

glm.probs=predict(glm.fit,Direction)

glm.pred=rep("Down", length(glm.probs))
glm.pred[glm.probs >.5] = "Up"
table(glm.pred,Direction)
mean(glm.pred==Direction)         


train= (Year < 2009)
weekly.9010=subset(Weekly[!train,])
glm.fit=glm(Direction~Lag2, data=Weekly, family=binomial,subset=train)
glm.probs=predict(glm.fit,weekly.9010,type="response")            
glm.pred=rep("Down", length(glm.probs))            
glm.pred[glm.probs>.5]="Up"            
table(glm.pred,weekly.9010$Direction)            
mean(glm.pred==weekly.9010$Direction)            

library(MASS)
lda.fit=lda(Direction~Lag2,data=Weekly, subset=train)
lda.pred=predict(lda.fit,weekly.9010)            
table(lda.pred$class,weekly.9010)            
mean(lda.pred$class == weekly.9010$Direction)            


qda.fit=qda(Direction~Lag2,data=Weekly, subset=train)
qda.class = predict(qda.fit, weekly.9010)$class
table(qda.class, weekly.9010$Direction)
mean(qda.class == weekly.9010$Direction)

library(class)
train.X=cbind(Lag2[train])
test.X=cbind(Lag2[!train])
train.direction=Direction[train]

set.seed(1)
knn.pred=knn(train.X,test.X,train.direction,1)
table(knn.pred,weekly.9010$Direction)
mean(knn.pred == weekly.9010$Direction)


# 11
library(ISLR)
summary(Auto)
attach(Auto)

mpg01=rep(0,length(mpg))
mpg01[mpg>median(mpg)]=1
Auto=data.frame(Auto,mpg01)

pairs(Auto,col=Auto$mpg01)
cor(Auto[,-9])

train=(year%%2==0)
test=!train
Auto.train=Auto[train,]
Auto.test=Auto[test,]
mpg01.test=mpg01[test]
#LDA
library(MASS)
lda.fit = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
lda.pred=predict(lda.fit,Auto.test)
table(lda.pred$class,mpg01.test)
mean(lda.pred$class==mpg01.test)
mean(lda.pred$class!=mpg01.test)

#QDA
qda.fit=qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
            subset = train)
qda.pred=predict(qda.fit,Auto.test)
mean(qda.pred$class!=mpg01.test)

#LR

glm.fit=glm(mpg01~cylinders+weight+displacement+horsepower, data=Auto, family=binomial, subset=train)
glm.probs=predict(glm.fit,Auto.test, type="response")
glm.pred=rep(0,length(glm.probs))
glm.pred[glm.probs>.5]=1
mean(glm.pred!=mpg01.test)

#knn

train.X=cbind(cylinders,weight,displacement,horsepower)[train,]
test.X=cbind(cylinders,weight,displacement,horsepower)[test,]
mpg01.train=mpg01[train]
set.seed(1)
knn.pred=knn(train.X,test.X,mpg01.train,1)
mean(knn.pred != mpg01.test)
knn.pred=knn(train.X,test.X,mpg01.train,10)
mean(knn.pred != mpg01.test)
knn.pred=knn(train.X,test.X,mpg01.train,100)
mean(knn.pred != mpg01.test)

# 12
power=function(){
  print(2^3)
}
power2=function(x,a){
  print(x^a)}
power2(2,3)
power2(10,3)
power2(131,8)

power3=function(x,a){
  result=x^a
  return(result)
}

x=1:10
plot(x,power3(x,2), log="xy", ylab = "Log of y = x^2", xlab = "Log of x", 
     main = "Log of x^2 versus Log of x")

PlotPower = function(x, a) {
  plot(x, power3(x, a))
}
PlotPower(1:10, 3)

#13

library(MASS)
attach(Boston)

summary(Boston)
crim01=rep(0,length(crim))
crim01[crim>median(crim)]=1
Boston=data.frame(Boston,crim01)


train=1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]

Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crim01.test = crim01[test]
# logistic regression
glm.fit = glm(crim01 ~ . -crim01 -crim, data = Boston, family = binomial, 
              subset = train)
glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != crim01.test)

glm.fit = glm(crim01 ~ . - crim01 - crim - chas - tax, data = Boston, family = binomial, 
              subset = train)
glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != crim01.test)

# LDA
lda.fit = lda(crim01 ~ . - crim01 - crim, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crim01.test)

lda.fit = lda(crim01 ~ . - crim01 - crim - chas - tax, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crim01.test)

lda.fit = lda(crim01 ~ . - crim01 - crim - chas - tax - lstat - indus - age, 
              data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crim01.test)


# KNN
library(class)
train.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
                lstat, medv)[train, ]
test.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
               lstat, medv)[test, ]
train.crim01 = crim01[train]
set.seed(1)
# KNN(k=1)
knn.pred = knn(train.X, test.X, train.crim01, k = 1)
mean(knn.pred != crim01.test)

knn.pred = knn(train.X, test.X, train.crim01, k = 10)
mean(knn.pred != crim01.test)


# KNN(k=100)
knn.pred = knn(train.X, test.X, train.crim01, k = 100)
mean(knn.pred != crim01.test)



# KNN(k=10) with subset of variables
train.X = cbind(zn, nox, rm, dis, rad, ptratio, black, medv)[train, ]
test.X = cbind(zn, nox, rm, dis, rad, ptratio, black, medv)[test, ]
knn.pred = knn(train.X, test.X, train.crime01, k = 10)
mean(knn.pred != crime01.test)
