#c

set.seed(1)
control=matrix(rnorm(50*1000), ncol = 50)
treatment=matrix(rnorm(50*1000), ncol = 50)
X=cbind(control, treatment)
X[1,] = seq(-18, 18 - .36, .36)

pr.out = prcomp(scale(X))
summary(pr.out)$importance[,1]
X = rbind(X, c(rep(10, 50), rep(0, 50)))
pr.out = prcomp(scale(X))
summary(pr.out)$importance[,1]

