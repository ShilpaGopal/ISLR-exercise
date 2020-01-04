require(ISLR)
install.packages("boot")
require(boot)
?cv.glm

plot(mpg~horsepower,data=Auto)
attach(Auto)

##LOOCV
glm.fit=glm(mpg~horsepower,data=Auto)
cv.glm(Auto,glm.fit)$delta

load("/Users/gshilpa/Documents/DS/r-sample/5.R.RData")
summary(Xy)
summary(lm(y~.,data=Xy))
matplot(Xy,type="l")

attach(Xy)

library(boot)

alpha = function(x,y){
  vx = var(x)
  vy = var(y)
  cxy= cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Xy$X1,Xy$y)
alpha.fn = function(data,index){
  with(data[index,],alpha(Xy$X1,Xy$y))
}

alpha.fn<-function(data, index) {
  fit1<-lm(y~., data=Xy[index,])
  coefficients(fit1)[['X1']]
}

set.seed(1)
alpha.fn (Xy,sample(1:100,100,replace=TRUE))
boot.out=boot(Xy,alpha.fn,R=1000)
boot.out

beta_hat_1 = function(data, index, formula) {
  
  model = lm(formula, data = data[index, ])
  
  summary(model)$coefficients[2, 1]
}

block_boot_model_51 <- tsboot(Xy, beta_hat_1, formula = y ~ X1 + X2, R = 15000, 
                              sim = "fixed", l = 100, parallel = "snow", ncpus = 4)

boot.ci(block_boot_model_51, conf = 0.9)

#validation set approach

set.seed(1);
train=sample(392,196)
lm.fit=lm(mpg~horsepower, data=Auto,subset=train)
summary(lm.fit)
attach(Auto)
mean((mpg - predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2);
train=sample(392,196)
lm.fit=lm(mpg~horsepower, data=Auto,subset=train)
mean((mpg - predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

###################
#LOOCV
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
library(boot)
cv.glm(Auto,glm.fit)$delta

cv.error=rep(0,5)
for(i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
  cv.error[i]=cv.glm(Auto, glm.fit)$delta[1]
  print(cv.error[i])
}

#####K holds

set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto);
  cv.error.10[i]=cv.glm(Auto, glm.fit, K = 10)$delta[1]
  print(cv.error.10[i])
} 

#Boot strap

boot.fn=function(data,index)
return(coef(lm(mpg~horsepower,data = Auto,subset = train)))  
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace = T))
boot(Auto,boot.fn,100)


boot.fn=function(data,index)
coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=train))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto,subset=train))


