library(ISLR)

gas_mileage= ifelse(Auto$mpg > median(Auto$mpg), 1,0)
Auto$mpglevel=as.factor(gas_mileage)

library(e1071)

set.seed(1)
tune.out=tune(svm, mpglevel~., data= Auto, kernel="linear", 
              ranges = list(cost=c(0.01, 0.1, 1,5,10,100)))
summary(tune.out)

set.seed(1)
tune.out=tune(svm, mpglevel~., data=Auto, kernel="polynomial", 
              ranges = list(cost=c(0.1,1,5,10), degree=c(2,3,4)))
summary(tune.out)

set.seed(1)
tune.out=tune(svm, mpglevel~., data=Auto, kernel="radial",
              ranges = list(cost=c(0.1,1,5,10), gamma=c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

svm.linear = svm(mpglevel ~ ., data = Auto, kernel = "linear", cost = 1)
svm.poly = svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 10, 
               degree = 2)
svm.radial = svm(mpglevel ~ ., data = Auto, kernel = "radial", cost = 10, gamma = 0.01)

plotpairs = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
  }
}

plotpairs(svm.linear)
plotpairs(svm.poly)
plotpairs(svm.radial)

