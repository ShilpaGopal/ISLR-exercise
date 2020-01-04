#2
pr = function(n) 
  return(1 - (1 - 1/n)^n)
x = 1:100000;
plot(x, pr(x))

store=rep(1:100000);
for(i in 1:100000){
  store[i]=sum(sample(1:100,rep=TRUE) ==4) > 0
}

mean(store)

#5

library(ISLR)
summary(Default)
attach(Default)
set.seed(1)

glm.fit=glm(default~income+balance, data=Default, family=binomial)
summary(glm.fit)            
FiveB=function(){
  train=sample(dim(Default)[1],dim(Default)[1]/2);
  glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
                subset = train)
  glm.pred=rep("No", dim(Default)[1]/2)
  glm.prob=predict(glm.fit,Default[-train,],type="response")
  glm.pred[glm.prob>.5] ="Yes";
  return (mean(glm.pred != Default[-train,]$default))
}
FiveB()


train = sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial, 
              subset = train)
glm.pred = rep("No", dim(Default)[1]/2)
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)


#6

require(ISLR)
attach(Default)
set.seed(1)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)


boot.fn=function(data, index){
  return(coef(glm(default ~ income + balance, 
                  data = data, family = binomial, subset = index)))}

library(boot)
boot(Default, boot.fn, 50)

#7

library(ISLR)
summary(Weekly)

attach(Weekly)
glm.fit=glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)
summary(glm.fit)

glm.fit=glm(Direction~Lag1+Lag2, data =Weekly[-1,], family = binomial)
summary(glm.fit)
predict(glm.fit, Weekly[1,],type="response") > .5

count = rep(0, dim(Weekly)[1])

for(i in 1:dim(Weekly)[1]){
  glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ], family = binomial)
  is_up = predict.glm(glm.fit, Weekly[i, ], type = "response") > 0.5
  is_true_up = Weekly[i, ]$Direction == "Up"
  if (is_up != is_true_up) 
    count[i] = 1
}

sum(count)
mean(count)

#8

set.seed(1)

y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
plot(x,y)

library(boot)
Data = data.frame(x, y)
set.seed(10)
glm.fit = glm(y ~ x)
cv.glm(Data, glm.fit)$delta

glm.fit=glm(y~poly(x,4))
cv.glm(Data, glm.fit)$delta

for (i in 1:4){
  glm.fit=glm(y~poly(x,i))
  print(i)
  print(cv.glm(Data, glm.fit)$delta)
}
summary(glm.fit)
set.seed(10)

# 9

library(MASS)
summary(Boston)
set.seed(1)
attach(Boston)
medv.mean = mean(medv)
medv.mean

medv.err=sd(medv)/sqrt(length(medv))
medv.err

boot.fn = function(data, index){
  return(mean(data[index]))
} 

library(boot)
bstrap = boot(medv, boot.fn, 1000)
bstrap

t.test(medv)

c(bstrap$t0 - 2 * 0.4119, bstrap$t0 + 2 * 0.4119)

medv.med = median(medv)
medv.med
boot.fn = function(data, index) return(median(data[index]))
boot(medv, boot.fn, 1000)


boot.fn = function(data, index) return(quantile(data[index], c(0.1)))
boot(medv, boot.fn, 1000)
