library(ISLR)
set.seed(1)
attach(College)
library(leaps)

names(College)
train = sample(length(Outstate), length(Outstate)/2)
test = -train
College.train=College[train,]
College.test=College[test,]
reg.fit = regsubsets(Outstate~.,data=College.train, nvmax=17, method="forward")
reg.summary = summary(reg.fit)
par(mfrow = c(1, 3))
plot(reg.summary$cp, xlab = "Number of variables", ylab = "cp", type="l")
min.cp = min(reg.summary$cp)
std.cp=sd(reg.summary$cp)
abline(h = min.cp + 0.2 * std.cp, col = "red", lty = 2)
abline(h = min.cp - 0.2 * std.cp, col = "red", lty = 2)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min.bic = min(reg.summary$bic)
std.bic = sd(reg.summary$bic)
abline(h = min.bic + 0.2 * std.bic, col = "red", lty = 2)
abline(h = min.bic - 0.2 * std.bic, col = "red", lty = 2)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", 
     type = "l", ylim = c(0.4, 0.84))
max.adjr2 = max(reg.summary$adjr2)
std.adjr2 = sd(reg.summary$adjr2)
abline(h = max.adjr2 + 0.2 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.2 * std.adjr2, col = "red", lty = 2)
reg.fit = regsubsets(Outstate ~ ., data = College, method = "forward")
coefi = coef(reg.fit, id = 6)
names(coefi)


library(gam)
fit=gam(Outstate~Private+s(Room.Board, 2)+s(PhD, 2)+s(perc.alumni, 2)+s(Grad.Rate,2), data = College.train)
par(mfrow = c(2, 3))
plot(fit, se=T, col="blue")

gam.pred=predict(fit,newdata = College.test)
gam.err=mean((College.test$Outstate- gam.pred)^2)
gam.err
gam.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
test.rss = 1 - gam.err/gam.tss
test.rss
summary(fit)
