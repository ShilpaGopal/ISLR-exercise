library(ISLR)
set.seed(1)
attach(USArrests)

dsc=scale(USArrests)
a = dist(dsc)^2
b = as.dist(1 - cor(t(dsc)))
summary(a/b)
