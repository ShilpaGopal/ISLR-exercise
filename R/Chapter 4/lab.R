library(ISLR)
names(Smarket)
summary(Smarket)
#Correlation
cor(Smarket)
#Correlation shouldn't include qualitative parameter
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)
pairs(Smarket, col=Direction)

#Logistic Regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family = binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef[,4]
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)

glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="UP"
#Cpnfusion matrix
table(glm.pred,Direction)
#Fraction of days for which our prediction is currect
mean(glm.pred==Direction)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit,Smarket.2005, type="response")

glm.pred=rep("Down", 252)
glm.pred[glm.probs>.50]="Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

predict(glm.fit, 
        data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),
        type="response")


#Linear discriminat Analysis
library(MASS)

lda.fit=lda(Direction~Lag1+Lag2, data = Smarket, subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
table(lda.pred$class, Direction.2005)
mean(lda.pred$class==Direction.2005)
sum(lda.pred$posterior[,1] > .5)
sum(lda.pred$posterior[,1] < .5)
lda.pred$posterior[1:20,1]
lda.pred$class[1:20]

#Quadratic Discriminator Analysis
qda.fit=qda(Direction~Lag1+Lag2, data=Smarket, subset = train)
qda.fit
qda.pred=predict(qda.fit,Smarket.2005)
table(qda.pred$class, Direction.2005)
mean(qda.pred$class==Direction.2005)

#KNN

library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.direction,1)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

knn.pred=knn(train.X,test.X,train.direction,3)

------------
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
stand.X=scale(Caravan[,-86])
var(stand.X[,1])
var(stand.X[,2])

test=1:1000
train.X=stand.X[-test,]
test.X=stand.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
knn.pred=knn(train.X,test.X,train.Y,k=5)
