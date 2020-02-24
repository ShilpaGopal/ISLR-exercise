install.packages("e1071")
library(e1071)

set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x, col=(3-y))

# factor teh data
sdata=data.frame(x=x, y=as.factor(y))
svmfit=svm(y~., data=data, kernel="linear", cost=10, scale = FALSE)
plot(svmfit, data)

svmfit$index
summary(svmfit)

# smaller value of cost function

svmfit=svm(y~., data=sdata, kernel="linear", cost=0.1, scale = FALSE)
plot(svmfit, sdata)
svmfit$index

set.seed(1)
tune.out=tune(svm, y~., data = sdata, kernel="linear", 
              ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmodel=tune.out$best.model
summary(bestmodel)

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]= xtest[ytest==1,]+1
testdata=data.frame(x=xtest, y=as.factor(ytest))
ypred=predict(bestmodel, testdata)
table(predict=ypred, truth=testdata$y)

#cost funtion 0.01
svmfit=svm(y~., data=sdata, kernel="linear", cost="0.01", scale = FALSE)
ypred= predict(svmfit, testdata)
table(predict=ypred, truth= testdata$y)

x[y==1,]= x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

sdata= data.frame(x=x, y=as.factor(y))
svmfit=svm(y~., data=sdata, kernel="linear", cost=1e+05)
summary(svmfit)
plot(svmfit, sdata)
svmfit=svm(y~., data=sdata, kernel="linear", cost=1)

#############

set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100, ]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150), rep(2,50))
sdata=data.frame(x=x, y=as.factor(y))

plot(x, col=y)
train=sample(200,100)
svmfit=svm(y~., data=sdata[train,], kernel="radial", gamma=1, cost=1)
plot(svmfit, sdata)
summary(svmfit)

# cross validation to select cost and gamma

set.seed(1)
tune.out= tune(svm, y~., data=sdata[train,], kernel="radial", ranges = list
               (cost=c(0.1,1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune.out)
bestmodel=tune.out$best.model

table(true=sdata[-train,"y"], pred=predict(bestmodel, newx=sdata[-train
                                                            ,]))

# ROC curves

install.packages("ROCR")
library(ROCR)
rocplot=function(pred, truth, ...){
  predob=prediction(pred, truth)
  perf=performance(predob, "tpr", "fpr")
  plot(perf, ...)
  }

svmfit.out= svm(y~., data = sdata[train,], kernel= "radial", gamma=2, cost=1, decision.values=T)

fitted = attributes(predict(svmfit.out, data[train, ], decision.values = TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted, sdata[train, "y"], main="Training data")


set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
sdata=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x, col=(y+1))

svm.fit=svm(y~., data=sdata, kernel= "radial", cost=10, gamma =1)
plot(svmfit, sdata)

###############

library(ISLR)
names(Khan)
attach(Khan)

dim(Khan$xtrain)
dim(Khan$xtest)

table(Khan$ytest)
table(Khan$ytrain)

sdata= data.frame(x=Khan$xtrain, y=as.factor(ytrain))
out= svm(y~., data= sdata, kernel= "linear", cost=10)
summary(out)
table(out$fitted, sdata$y)
tdata= data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))

pred.te= predict(out, newdata = tdata)
table(pred.te, tdata$y)







