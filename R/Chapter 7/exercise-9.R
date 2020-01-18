library(MASS)
attach(Boston)
names(Boston)
#cubic polinomial
set.seed(1)
fit.lm=lm(nox~poly(dis,3), data = Boston)
summary(fit.lm)

dis.limit=range(dis)
dis.grid=seq(from=dis.limit[1], to=dis.limit[2])
preds=predict(fit.lm, newdata = list(dis=dis.grid))
plot(nox~dis, col="darkgrey", data = Boston)
lines(dis.grid, preds, col="red", lwd=2)

rss=rep[NA,10]
for (i in 1:10) {
  lm.fit=lm(nox~poly(dis, i), data=Boston)
  rss[i]=sum(lm.fit$residuals^2)
}
plot(1:10, rss ,type = "l")
library(boot)
set.seed(1)
cv.error=rep(NA,10)
for(i in 1:10){
  fit=glm(nox~poly(dis,i), data = Boston)
  cv.error[i]=cv.glm(Boston, fit, K=10)$delta[2]
}

plot(1:10, cv.error, xlab = "degree", ylab = "cv error", type="l", pch=20, lwd=2)


library(splines)
sp.fit = lm(nox ~ bs(dis, df = 4, knots = c(4, 7, 11)), data = Boston)
summary(sp.fit)

sp.pred = predict(sp.fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, sp.pred, col = "red", lwd = 2)


all.error=rep(NA, 16)
for(i in 3:16){
  fit=lm(nox~bs(dis, df=i), data=Boston)
  all.error[i]=sum(fit$residuals^2)
}

all.error[-c(1,2)]

all.cv=rep(NA,16)
for(i in 3:16){
  fit=glm(nox~bs(dis, df=i), data=Boston)
  all.cv[i]=cv.glm(Boston, fit, K=10)$delta[2]
}

all.cv[-c(1,2)]
plot(3:16, all.cv[-c(1,2)],lwd=2, type="l", xlab="degree of freedom", ylab="cv error" )
