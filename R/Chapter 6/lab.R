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

# Choosing among the model using validation set approach and cross validation

set.seed(1);
train=sample(c(TRUE,FALSE), nrow(Hitters), replace =TRUE)
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,], nvmax=19)
summary(regfit.best)

test.mat = model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best, id=i)
  pred=test.mat[, names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
which.min(val.errors)
coef(regfit.best,10)

regfit.best.full=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(regfit.best.full, 10)

predict.regsubset=function(object,newdata,id,...){
  form = as.formula(object$call[[2]]);
  mat=model.matrix(form,newdata);
  coefs=coef(object, id=id);
  xvars=names(coefs);
  mat[,xvars]%*%coefs
}

# cross validation

k =10;
set.seed(1)
folds=sample(1:k, nrow(Hitters),replace = TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,], nvmax=19);
  for(i in 1:19){
    pred=predict.regsubset(best.fit, Hitters[folds==j,], id =i);
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2);
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors, type = 'b')

reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
plot(summary(reg.best)$adjr2, xlab="Number of variables", ylab="Adjr2", type="l")
coef(reg.best,11)

# Ridge Regression
install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)

set.seed(1)
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda = grid)
ridge.mod
dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]

sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod, s=50, type="coefficients")[1:20,]


set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha = 0,lambda = grid,thresh = 1e-12)
reg.pred=predict(ridge.mod, s=4,newx = x[test,])
mean((reg.pred-y.test)^2)

# ridge regression with cross validation
set.seed(1)
cv.out=cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod, s=bestlam, newx = x[test,])
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0)
predict(out,type = "coefficients", s=bestlam)[1:20,]

#lasso

lasso.mod=glmnet(x[train,], y[train], alpha=1, lambda = grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda = grid)
lass.coef=predict(out, type="coefficients", s=bestlam)[1:20,]
lass.coef

# Principle component regression
install.packages("pls")
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~.,data=Hitters, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP")
##PCR on training set

set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit)
pcr.pred=predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)

#Partial least squares

set.seed(1)
pls.fit=plsr(Salary~.,data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)

pls.pred=predict(pls.fit,x[test,],ncomp = 2)
mean((pls.pred-y.test)^2)

pls.fit=plsr(Salary~.,data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)  
  
  
  
  
  
  
  
  
