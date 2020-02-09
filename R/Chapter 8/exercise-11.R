library(ISLR)
attach(Caravan)
train = 1:1000

Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan.train=Caravan[train,]
Caravan.test=Caravan[-train,]

library(gbm)
boost.caravan=gbm(Purchase ~., data=Caravan.train, n.trees = 1000, 
                  shrinkage = 0.01, distribution = "bernoulli")
summary(boost.caravan)

pred.caravan=predict(boost.caravan, Caravan.test, n.trees = 1000, type = "response")

pred.caravan = ifelse(pred.caravan>0.2,1,0)
table(Caravan.test$Purchase, pred.caravan)

lm.caravan = glm(Purchase ~ ., data = Caravan.train, family = binomial)
lm.prob = predict(lm.caravan, Caravan.test, type = "response")
lm.pred = ifelse(lm.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, lm.pred)
58/(350+58)
