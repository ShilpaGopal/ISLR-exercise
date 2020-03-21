#a
set.seed(2)
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1

#b
pca.out = prcomp(x)
summary(pca.out)
pca.out$x[,1:2]

plot(pca.out$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19) 

#c
km.out=kmeans(x, 3, nstart = 20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

#d
km.out=kmeans(x, 2, nstart = 20)
km.out$cluster

#e
km.out=kmeans(x, 4, nstart = 20)
km.out$cluster

#f
km.out=kmeans(pca.out$x[,1:2], 3, nstart = 20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

#g
km.out = kmeans(scale(x), 3, nstart=20)
km.out$cluster
          