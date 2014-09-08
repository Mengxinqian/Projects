#if(FALSE) {
if (!require("maps")) {
  install.packages("maps")
}
if (!require("mapdata")) {
  install.packages("mapdata")
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
}
if (!require("clValid")) {
  install.packages("clValid")
}

library(RColorBrewer)
library(maps)
library(mapdata)
library(clValid)
n.samples=10000
n.clusters = 3

#read in data and randomly sample for clustering
ling.data = read.table("binary-ling-data.data",header=TRUE)
ling.sample = ling.data[sample(nrow(ling.data), n.samples),]
ling.sample.numerical = ling.sample[,7:ncol(ling.sample)]

#perform pca dimension reduction
pca = prcomp(ling.sample.numerical, tol=0.8)
ling.reduced = pca$x
if (FALSE) {
#perform k-means clustering
set.seed(47)
cluster.labels.k = kmeans(ling.reduced, centers=n.clusters, iter.max=5)$cluster
set1 = brewer.pal(n.clusters, "Set1")


#find which questions influence each principal component
PC1 <- names(which(pca$rotation[,1] >= sort(pca$rotation[,1], decreasing=T)[4]))
PC2 <- names(which(pca$rotation[,2] >= sort(pca$rotation[,2], decreasing=T)[4]))
PC3 <- names(which(pca$rotation[,3] >= sort(pca$rotation[,3], decreasing=T)[4]))
PC4 <- names(which(pca$rotation[,4] >= sort(pca$rotation[,4], decreasing=T)[4]))

#plot pc1 against pc2, color by label, randomly sampled
par(mfrow=c(2,2))
map(database="state")
points(ling.sample$long, ling.sample$lat, pch=20, col=set1[as.factor(cluster.labels.k)])
plot(ling.reduced[,1], ling.reduced[,2], pch=20, col=set1[as.factor(cluster.labels.k)], xlab="PC1", ylab="PC2", main="kmeans PC1 vs PC2")
plot(ling.reduced[,1], ling.reduced[,3], pch=20, col=set1[as.factor(cluster.labels.k)], xlab="PC1", ylab="PC3", main="kmeans PC1 vs PC3")
plot(ling.reduced[,1], ling.reduced[,4], pch=20, col=set1[as.factor(cluster.labels.k)], xlab="PC1", ylab="PC4", main="kmeans PC1 vs PC4")

#perform hierarchical clustering on sampled data
cluster.labels.h = cutree(hclust(dist(ling.reduced)), k=n.clusters)
par(mfrow=c(2,2))
map(database="state")
points(ling.sample$long, ling.sample$lat, pch=20, col=set1[as.factor(cluster.labels.h)])
plot(ling.reduced[,1], ling.reduced[,2], pch=20, col=set1[as.factor(cluster.labels.h)], xlab="PC1", ylab="PC2", main="hclust PC1 vs PC2")
plot(ling.reduced[,1], ling.reduced[,3], pch=20, col=set1[as.factor(cluster.labels.h)], xlab="PC1", ylab="PC3", main="hclust PC1 vs PC3")
plot(ling.reduced[,1], ling.reduced[,4], pch=20, col=set1[as.factor(cluster.labels.h)], xlab="PC1", ylab="PC4", main="hclust PC1 vs PC4")
}
#if (FALSE) {
#examine stability of clusters
stability = clValid(ling.reduced, nClust=2:6, maxitems=n.samples, clMethods=c("hierarchical", "kmeans"), validation="stability")

par(mfrow=c(2,2))
summary(stability)


#stability.k = measures(stability)[,,1]
#stability.h = measures(stability)[,,2]
#stability.x = as.integer(colnames(measures(stability)[,,1]))

#par(mfrow=c(4,1))
#plot(stability.x, stability.k[1,], ylab="kmeans stability: APN", xlab="num clusters", col="red", type="l")
#plot(stability.x, stability.k[1,], ylab="kmeans stability: AD", xlab="num clusters", col="blue", type="l")
#plot(stability.x, stability.k[1,], ylab="kmeans stability: ADM", xlab="num clusters", col="green", type="l")
#plot(stability.x, stability.k[1,], ylab="kmeans stability: FOM", xlab="num clusters", col="purple", type="l")

#par(mfrow=c(4,1))
#plot(stability.x, stability.h[1,], ylab="hclust stability: APN", xlab="num clusters", col="red", type="l")
#plot(stability.x, stability.h[1,], ylab="hclust stability: AD", xlab="num clusters", col="green", type="l")
#plot(stability.x, stability.h[1,], ylab="hclust stability: ADM", xlab="num clusters", col="blue", type="l")
#plot(stability.x, stability.h[1,], ylab="hclust stability: FOM", xlab="num clusters", col="purple", type="l")

#}
