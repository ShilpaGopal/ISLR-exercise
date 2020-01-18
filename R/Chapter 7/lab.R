library(ISLR)
attach(Wage)

# Polynomial regression and Step function

fit = lm(wage~poly(age,4), data=Wage)
summary(fit)

fit2=lm(wage~poly(age,4, raw=T), data=Wage)
summary(fit2)

fit2a=lm(wage ~ age+I(age^2)+I(age^3)+I(age^4), data=Wage)
summary(fit2a)

fit2b=lm(wage~cbind(age, age^2,age^3,age^4), data=Wage)
summary(fit2b)

age.limits=range(age)
age.grid=seq(from=age.limits[1], to=age.limits[2])

preds= predict(fit, newdata = list(age=age.grid), se=TRUE)
se.band=cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(age, wage, xlim = age.limits, cex=.5, col="darkgrey")
title("4 degree polynomial", outer = T)
lines(age.grid, preds$fit, lwd=2, col="blue")

matlines(age.grid, se.band, lwd=1, col="red")

preds2=predict(fit2, newdata = list(age=age.grid), se=TRUE)
max(abs(preds$fit-preds2$fit))

fit1=lm(wage~age, data = Wage)
fit2=lm(wage~poly(age,2), data=Wage)
fit3=lm(wage~poly(age,3), data=Wage)
fit4=lm(wage~poly(age,4), data=Wage)
fit5=lm(wage~poly(age,5), data=Wage)
anova(fit1, fit2, fit3, fit4, fit5)
coef(summary(fit5))

fit.1 = lm (wage~ education+age, data = Wage)
fit.2 = lm (wage~ education+poly(age,2), data = Wage)
fit.3 = lm(wage~ education+poly(age,3), data = Wage)
anova(fit.1, fit.2,fit.3)


fit = glm(I(wage > 250)~ poly(age,4), data = Wage, family=binomial)
preds= predict(fit, newdata = list(age=age.grid), se=T)
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.band.logit= cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands=exp(se.band.logit)/(1+exp(se.band.logit))

preds=predict(fit, newdata = list(age=age.grid), type="response", se=T)

plot(age, I(wage>250), xlim = age.limits, type = "n", ylim=c(0,.2))
points(jitter(age), I((wage>250)/5), cex=.5, pch="|", col="darkgray")

lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)
table(cut(age,4))
fit=lm(wage~cut(age,4), data=Wage)
coef(summary(fit))


##Splines

library(splines)
fit = lm(wage~bs(age,knots = c(25,40,60)), data=Wage)
preds=predict(fit, newdata = list(age=age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid, preds$fit, lwd=2)
lines(age.grid, preds$fit+2*preds$se.fit, lty="dashed")
lines(age.grid, preds$fit-2*preds$se.fit, lty="dashed")

dim(bs(age, knots = c(25,40,60)))
dim(bs(age, df=6))
attr(bs(age,df=6), "knots")

fit2= lm(wage~ns(age,df=4), data=Wage)
preds2=predict(fit2, newdata = list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col="red", lwd=2)
plot(age, wage,xlim = age.limits, cex=.5, col="darkgray")
title("smooth spline")
fit=smooth.spline(age, wage, df=16)
fit2=smooth.spline(age,wage, cv=TRUE)
fit2$df
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend = c("16 DF", "6.8 DF"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)

plot(age, wage, xlim=age.limits, col="darkgray", cex=.5)
title("Local regression")
fit=loess(wage~age, span=2, data=Wage)
fit2=loess(wage~age, span=5, data=Wage)
lines(age.grid, predict(fit, data.frame(age=age.grid)), col="blue", lwd=2)
lines(age.grid, predict(fit2, data.frame(age=age.grid)), col="red", lwd=2)

legend("topright", legend = c("span=.2", "span=.5"), col = c("blue", "red"), lty=1, lwd=2, cex=.8)

#GAM
gaml=lm(wage~ns(year,4)+ ns(age,5)+ education, data=Wage)
install.packages("gam")
library(gam)

gam.m3=gam(wage~s(year,4)+s(age,5)+education, data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")
plot.Gam(gaml, se=TRUE, col="Red")

gam.m1=gam(wage~s(age,5)+education, data=Wage)
gam.m2=gam(wage~year+s(age,5)+ education, data = Wage)
anova(gam.m3, gam.m1, gam.m2)
summary(gam.m2)

preds=predict(gam.m1, newdata = Wage)
gam.lo=gam(wage~s(year, 4)+lo(age, span=0.7)+education, data= Wage)
plot.Gam(gam.lo, se=TRUE, col="red")

gam.lo.i=gam(wage~ lo(year, age, span=.7)+education, data=Wage)
install.packages("akima")
library(akima)
gam.lr=gam(I(wage>250)~year+s(age,5)+education, family = binomial, data=Wage)
par(mfrow=c(1,3))
plot(gam.lr, se=T, col="green")
table(education, I(wage>250))


gam.lr.s=gam(I(wage>250)~year+s(age,5)+education, data=Wage, family = binomial, subset=(education!="1.<HS Grad"))
plot(gam.lr.s, se=TRUE, col="yellow")
