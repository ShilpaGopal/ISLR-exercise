library(ISLR)
attach(Weekly)
names(Weekly)
summary(Weekly)

train = sample(nrow(Weekly), nrow(Weekly)/2)
test = -train
result=c(1:4)

#logistic regression
glm.fit = glm(Direction ~ . - Year - Today, data = Weekly[train, ], 
              family = "binomial",control = list(maxit = 50))
glm.prob=predict(glm.fit, newdata= Weekly[test,], type="response")
glm.pred=rep("Down", length(glm.prob))
glm.pred[glm.prob > 0.5] = "Up"
table(glm.pred, Weekly$Direction[test])
result[1]=mean(glm.pred != Weekly$Direction[test])
#0.48

#Boosting
library(gbm)
Weekly$BinomialDirection = ifelse(Weekly$Direction == "Up", 1, 0)
boost.weekly = gbm(BinomialDirection ~ . - Year - Today - Direction, data = Weekly[train, ],
                   distribution = "bernoulli", n.trees = 5000)
yhat.boost = predict(boost.weekly, newdata = Weekly[test, ], n.trees = 5000)
yhat.pred = rep(0, length(yhat.boost))
yhat.pred[yhat.boost > 0.5] = 1
table(yhat.pred, Weekly$BinomialDirection[test])
result[2]=mean(yhat.pred != Weekly$BinomialDirection[test])

#Bagging
Weekly = Weekly[, !(names(Weekly) %in% c("BinomialDirection"))]
library(randomForest)
bag.weekly = randomForest(Direction ~ . - Year - Today, data = Weekly, subset = train, 
                          mtry = 6)
yhat.bag = predict(bag.weekly, newdata = Weekly[test, ])
table(yhat.bag, Weekly$Direction[test])
result[3]=mean(yhat.bag != Weekly$Direction[test])

#Random forests

rf.weekly = randomForest(Direction ~ . - Year - Today, data = Weekly, subset = train, 
                         mtry = 2)
yhat.bag = predict(rf.weekly, newdata = Weekly[test, ])
table(yhat.bag, Weekly$Direction[test])
result[4]=mean(yhat.bag != Weekly$Direction[test])
plot(1:4, result, xlab= "types", ylab = "mean", type="b")

