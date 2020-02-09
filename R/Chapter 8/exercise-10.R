library(ISLR)
attach(Hitters)
sum(is.na(Hitters$Salary))
Hitters= Hitters[-which(is.na(Hitters$Salary)),]

Hitters$Salary= log(Hitters$Salary)

train = 1:200
train.hitters=Hitters[train,]
test.Hitters=Hitters[-train,]

library(gbm)

set.seed(103)
pows = seq(-10, -0.2, by = 0.1)
lambdas=10^pows
length.lambdas = length(lambdas)
train.errors = rep(NA, length.lambdas)
test.errors = rep(NA, length.lambdas)

for (i in 1:length.lambdas) {
  boot.hitters=gbm(Salary ~., data= train.hitters, distribution = "gaussian", 
                   n.trees = 1000, shrinkage = lambdas[i])
  train.pred= predict(boot.hitters, train.hitters, n.trees=1000)
  test.pred=predict(boot.hitters, test.Hitters, n.trees=1000)
  train.errors[i]= mean((train.hitters$Salary - train.pred)^2)
  test.errors[i]= mean((test.Hitters$Salary- test.pred))
  
}

plot(lambdas, train.errors, type="b", xlab = "shrinkage", ylab = "train error", 
     col="red", pch=20)
plot(lambdas, test.errors, type="b", xlab = "shrinkage", ylab = "test error", col="green", pch=20)
min(test.errors)
lambdas[which.min(test.errors)]


library(glmnet)
set.seed(134)
x = model.matrix(Salary ~ ., data = train.hitters)
y = train.hitters$Salary
x.test = model.matrix(Salary ~ ., data = test.Hitters)
lasso.fit = glmnet(x, y, alpha = 1)
lasso.pred = predict(lasso.fit, s = 0.01, newx = x.test)
mean((test.Hitters$Salary - lasso.pred)^2)

#Bosting
boost.best = gbm(Salary ~ ., data = train.hitters, distribution = "gaussian", 
                 n.trees = 1000, shrinkage = lambdas[which.min(test.errors)])
summary(boost.best)

#bagging

library(randomForest)
set.seed(21)
rf.hitters = randomForest(Salary ~ ., data =train.hitters, ntree = 500, mtry = 19)
rf.pred= predict(rf.hitters, test.Hitters)
mean((test.Hitters$Salary - rf.pred)^2)
