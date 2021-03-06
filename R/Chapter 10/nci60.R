library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)

# PCA
pr.out=prcomp(nci.data, scale = TRUE)
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1", ylab = "Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="Z1", ylab = "Z2")

summary(pr.out)
plot(pr.out)
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type = "o", ylab="PVE", xlab = "Principle components", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab = "Principle components", col="brown3")

# Clustering 

sd.data=scale(nci.data)
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels = nci.labs, main = "Complete Linckage", xlab = "", ylab = "")
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linckage", xlab = "", ylab = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Average single", xlab = "", ylab = "")
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out, 4)
table(hc.clusters)
par(mfrow=c(1,1))
plot(hc.out, labels = nci.labs)
abline(h=139, col="red")
hc.out

#k-means 

set.seed(2)
km.out=kmeans(sd.data, 4, nstart = 20)
km.cluster=km.out$cluster
table(km.cluster, hc.clusters)

hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels = nci.labs)
table(cutree(hc.out, 4), nci.labs)
