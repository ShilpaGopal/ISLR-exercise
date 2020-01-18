library(ISLR)
attach(Auto)
names(Auto)
set.seed(1)
pairs(Auto)

# polynomials
rss=rep(NA,10)
fits=list()
for(i in 1:10){
  fits[[i]]=lm(mpg~poly(displacement, i), data=Auto)
  rss[i]=deviance(fits[[i]])
}
anova(fits[[1]], fits[[2]], fits[[3]], fits[[4]], fits[[5]],fits[[6]],fits[[7]])

library(glmnet)
library(boot)

cv.error=rep(NA, 15)
for(i in 1:15){
  fit=glm(mpg~poly(displacement, i), data=Wage)
  cv.error[i]=cv.glm(Auto, fit, K=10)$delta[2]
}
which.min(cv.error) #11

cv.error=rep(NA,10)
for (i in 2:10) {
  Auto$displacement.cut=cut(displacement, i)
  fit=glm(mpg~displacement.cut, data=Auto)
  cv.error[i]=cv.glm(Auto, fit, K=10)$delta[2]
}
which.min(cv.error) #9

library(splines)
cv.error=rep(NA,10)
for (i in 3:10) {
  fit=glm(mpg~ns(displacement, df=i), data = Auto)
  cv.error[i]=cv.glm(Auto, fit, K=10)$delta[2]
}
which.min(cv.error) #9

library(gam)
fit=glm(mpg~s(displacement,4)+s(horsepower, 4), data = Auto)
summary(fit)
