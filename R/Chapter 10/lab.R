library(ISLR)
attach(USArrests)
names(USArrests)

states=row.names(USArrests)
states

apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# PCA
pr.com=prcomp(USArrests, scale=TRUE)
names(pr.com)
pr.com$center
pr.com$scale
pr.com$rotation
biplot(pr.com, scale=0, cex=.3)

pr.com$rotation= -pr.com$rotation
pr.com$x=-pr.com$x
biplot(pr.com, scale = 0, cex=.4)
pr.com$sdev
pr.var= pr.com$sdev^2
pr.var

pvar=pr.var/sum(pr.var)
pvar

plot(pvar, xlab = "Principle compoments", ylab="proportion of the variance explaned", 
     ylim = c(0,1), type='b')

plot(cumsum(pvar), xlab="Principal components", ylab="cumulative proportion of the variance explaned", ylim=c(0,1), ttype = 'b')

#Clustering

set.seed(2)
x=matrix(rnorm(50*2), ncol = 2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
km.out=kmeans(x,2,nstart = 20)
km.out$cluster
plot(x, col=(km.out$cluster+1), main = "K-means clustering Results with k=2", xlab = "", ylab = "", pch=20, cex=2)

set.seed(3)
km.out=kmeans(x,3,nstart = 20)
km.out
km.out$tot.withinss

#Hierarchical Clustering

hc.complete=hclust(dist(x), method = "complete")
hc.average=hclust(dist(x), method = "average")
hc.single=hclust(dist(x), method = "single")
par(mfrow=c(1,3))
plot(hc.complete, xlab = "", ylab = "", main = "Complete linkage", cex=.9)
plot(hc.average, xlab = "", ylab = "", main = "average linkage", cex=.9)
plot(hc.single, xlab = "", ylab = "", main = "single linckage", cex=.9)

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

cutree(hc.single, 4)
xscale=scale(x)
plot(hclust(dist(xscale), method="complete"), main="Hlierarchical")

x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method = "complete"), main = "complete linckage")




















