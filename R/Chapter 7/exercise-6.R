library(ISLR)
attach(Wage)
library(boot)

set.seed(1)
all.deltas = rep(NA, 10)
for (i in 1:10) {
  glm.fit = glm(wage~poly(age, i), data=Wage)
  all.deltas[i] = cv.glm(Wage, glm.fit, K=10)$delta[2]
}

plot(1:10, all.deltas, xlab="Degree", ylab = "cv error", type="l", pch=20, lwd=2, ylim=c(1590, 1700))
min.point = min(all.deltas)
sd.points = sd(all.deltas)
abline(h=min.point + 0.2 * sd.points, col="red", lty="dashed")
abline(h=min.point - 0.2 * sd.points, col="red", lty="dashed")
legend("topright", "0.2-standard deviation lines", lty="dashed", col="red")

fit.1 = lm(wage~poly(age, 1), data=Wage)
fit.2 = lm(wage~poly(age, 2), data=Wage)
fit.3 = lm(wage~poly(age, 3), data=Wage)
fit.4 = lm(wage~poly(age, 4), data=Wage)
fit.5 = lm(wage~poly(age, 5), data=Wage)
fit.6 = lm(wage~poly(age, 6), data=Wage)
fit.7 = lm(wage~poly(age, 7), data=Wage)
fit.8 = lm(wage~poly(age, 8), data=Wage)
fit.9 = lm(wage~poly(age, 9), data=Wage)
fit.10 = lm(wage~poly(age, 10), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)

plot(wage~age, data=Wage, col="darkgrey")

age.limits=range(age)
age.grid= seq(from=age.limits[1], to=age.limits[2])
lm.fit=lm(wage~poly(age,3), data=Wage)
preds=predict(lm.fit, newdata = list(age=age.grid))
lines(age.grid, preds, col="red")

all.cv.error=rep(NA, 10)
for(i in 2:10){
  Wage$age.cut = cut(Wage$age, i)
  lm.fit=glm(wage~age.cut, data = Wage)
  all.cv.error[i]=cv.glm(Wage, lm.fit, K=10)$delta[2]
}
plot(2:10, all.cv.error[-1], xlab = "Number of cutes", ylab="cv error", type="l", pch=20, lwd=2)

lm.fit=lm(wage~cut(age, 8), data=Wage)
age.limits=range(age)
age.grid= seq(from=age.limits[1], to=age.limits[2])
lm.pred=predict(lm.fit, newdata = list(age=age.grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age.grid, lm.pred, col="red", lwd=2)





