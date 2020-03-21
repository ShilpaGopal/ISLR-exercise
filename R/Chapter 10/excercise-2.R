#a
d= as.dist(matrix(c(0,0.3,0.4,0.7,
                    0.3,0,0.5,0.8,
                    0.4,0.5,0,0.45,
                    0.7,0.8,0.45,0), nrow=4))

plot(hclust(d, method = "complete"), main = "Complete linckage")

#b
plot(hclust(d, method = "single"), main = "Single linckage")

#e
plot(hclust(d, method="complete"), labels=c(2,1,4,3))
