library(ISLR)
attach(Carseats)
set.seed(1)

# Train and test data
train=sample(dim(Carseats)[1], dim(Carseats)[1]/2)
Carseats.train=Carseats[train,]
Carseats.test=Carseats[-train,]

# regression tree
library(tree)
tree.carseats=tree(Sales~., data = Carseats.train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)

pred.carseats = predict(tree.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2)

# cross validation
cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
pruned.carseats=prune.tree(tree.carseats, best = 12)
par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty = 0)
pred.pruned=predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales- pred.pruned)^2)

#bagging

library(randomForest)
bag.carseats=randomForest(Sales~., data = Carseats.train, mtry=10, ntree=500, importance=T)
bag.pred=predict(bag.carseats, Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2)
importance(bag.carseats)

rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 5, ntree = 500, 
                           importance = T)
rf.pred = predict(rf.carseats, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)

importance(rf.carseats)




