set.seed(1)
p = 100
n = 1000
x = matrix(ncol = p, nrow = n)
coefi = rep(NA, p)
for (i in 1:p) {
  x[, i] = rnorm(n)
  coefi[i] = rnorm(1) * 100
}
y = x %*% coefi + rnorm(n)
beta = rep(0, p)
max_iterations = 1000
errors = rep(NA, max_iterations + 1)
iter = 2
errors[1] = Inf
errors[2] = sum((y - x %*% beta)^2)
threshold = 1e-04
while (iter < max_iterations && errors[iter - 1] - errors[iter] > threshold) {
  for (i in 1:p) {
    a = y - x %*% beta + beta[i] * x[, i]
    beta[i] = lm(a ~ x[, i])$coef[2]
  }
  iter = iter + 1
  errors[iter] = sum((y - x %*% beta)^2)
  print(c(iter - 2, errors[iter - 1], errors[iter]))
}
plot(1:11, errors[3:13])
