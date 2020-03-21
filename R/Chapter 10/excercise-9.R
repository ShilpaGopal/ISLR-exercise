library(ISLR)
set.seed(1)

#a
hc.complete=hclust(dist(USArrests), method = "complete")
plot(hc.complete)

#b
cutree(hc.complete, 3)
table(cutree(hc.complete, 3))
#c
dsc = scale(USArrests)
hc.s.complete = hclust(dist(dsc), method="complete")
plot(hc.s.complete)

cutree(hc.s.complete, 3)
table(cutree(hc.s.complete, 3))
table(cutree(hc.s.complete, 3), cutree(hc.complete, 3))
