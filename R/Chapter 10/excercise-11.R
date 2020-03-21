#a
data = read.csv("../../data/Ch10Ex11.csv", header=F)
dim(data)

#b
dd = as.dist(1 - cor(data))
plot(hclust(dd, method="complete"))
plot(hclust(dd, method="single"))
plot(hclust(dd, method="average"))

#c
pr.out = prcomp(t(data))
summary(pr.out)
total_load = apply(pr.out$rotation, 1, sum)
indices = order(abs(total_load), decreasing=T)
indices[1:10]
total_load[indices[1:10]]
