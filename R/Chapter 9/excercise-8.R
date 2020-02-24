library(ISLR)

attach(OJ)

set.seed(1)
train=sample(dim(OJ)[1],800)
OJ.train=OJ[train,]
OJ.test=OJ[-train,]

library(e1071)

svm.fit=svm(Purchase~., data=OJ, kernel="linear", cost="0.01")
summary(svm.fit)
train.pred = predict(svm.fit, OJ.train)
table(OJ.train$Purchase, train.pred)

(59+73)/(435+59+73+233)
test.pred = predict(svm.fit, OJ.test)
table(OJ.test$Purchase, test.pred)
(27+18)/(141+18+27+84)

tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "linear", 
                ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

svm.linear = svm(Purchase ~ ., kernel = "linear", 
                 data = OJ.train, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)
(73+55)/(439+233+73+55)


test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)
(19+32)/(140+19+32+79)

svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial")
summary(svm.radial)
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
(77+39)/(455+39+77+229)


test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)
(18+28)/(141+18+28+83)


tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "radial", ranges = list(cost = 10^seq(-2,                                                                                                1, by = 0.25)))
summary(tune.out)

svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial", cost = tune.out$best.parameters$cost)
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
(46+78)/(448+228+46+78)
test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)
(15+29)/(144+15+29+82)


svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2)
summary(svm.poly)
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)
(33+105)/(461+33+105+201)

test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)
(10+41)/(149+10+41+70)

tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, 
                ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)
(40+78)/(454+228+40+78)
test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)
(33+17)/(142+17+33+78)

