library(ISLR)
attach(OJ)
names(OJ)
dim(OJ)
set.seed(1013)

#Data split
train=sample(dim(OJ)[1], 800)
OJ.train=OJ[train,]
OJ.test=OJ[-train,]

# construct tree
library(tree)
tree.oj=tree(Purchase~., data=OJ.train)
summary(tree.oj)

# Intepret the information
tree.oj

# create plot of teh tree
plot(tree.oj)
text(tree.oj, pretty = 0)
pred.tree=predict(tree.oj, OJ.test,type = "class")
table(OJ.test$Purchase, pred.tree)

# cross validation 

cv.tree.oj=cv.tree(tree.oj, FUN = prune.tree)
plot(cv.tree.oj$size, cv.tree.oj$dev, type="b", xlab = "Tree size", ylab="deviance")

pruned.oj = prune.tree(tree.oj, best = 6)
summary(pruned.oj)

pred.unpruned=predict(tree.oj, OJ.test, type="class")
misclass.unpruned= sum(OJ.test$Purchase != pred.unpruned)
misclass.unpruned/length(pred.unpruned)

pred.pruned = predict(pruned.oj, OJ.test, type = "class")
misclass.pruned= sum(OJ.test$Purchase != pred.pruned)
misclass.pruned/length(pred.pruned)



