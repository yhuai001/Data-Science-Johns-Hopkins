#Hierarchical Clustering
#≤„¥Œæ€¿‡
#agglomerative approach: distance merging
#produces: a tree showing how close things are to each other
#Distances:Euclidean;Manhattan
#complete/average linkage
#correlation similarity
set.seed(1234)
par(mar=c(0,0,0,0))
x <- rnorm(12, mean = rep(1:3, each=4), sd=0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd=0.2)
plot(x,y,col="blue", pch=19, cex=2)
text(x+0.05, y+0.05, labels = as.character(1:12))

dataFrame <- data.frame(x=x,y=y)
distxy <- dist(dataFrame) #distance matrix
hClustering <- hclust(distxy)
plot(hClustering)

set.seed(143)
dataFrame <- data.frame(x=x,y=y)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)


#K-Means & Dimension Reduction
#1.Fix a number of clusters 2.Get centroids of each cluster 3.assign things to closest centroid 4.recalculate
dataFrame <- data.frame(x,y)
kmeansObj <- kmeans(dataFrame,centers = 3)
names(kmeansObj)
kmeansObj$cluster

par(mar=rep(0.2,4))
plot(x,y,col=kmeansObj$cluster, pch=19, cex=2)
points(kmeansObj$centers, col=1:3, pch=3, cex=3, lwd=3)

set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
kmeansObj <- kmeans(dataMatrix,centers=3)
par(mfrow=c(1,2), mar=c(2,4,0.1,0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt="n")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt="n")

#Principal Components Analysis
png("week3.png", width = 800, height = 800)

set.seed(12345)
par(mar=rep(0.2,4))
dataMatrix <- matrix(rnorm(400),nrow=40)
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)

set.seed(678910)

for(i in 1:40){
    coinFlip <- rbinom(1,size=1,prob=0.5)
    if(coinFlip){
        dataMatrix[i, ] <- dataMatrix[i, ]+rep(c(0,3),each=5)
    }
}
par(mar=rep(0.2,4))
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow=c(1,3))
image.default(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, xlab="Row Mean", ylab="Row", pch=19)
plot(colMeans(dataMatrixOrdered), xlab="Column", ylab="Column Mean", pch=19)

dev.off()
#PCA/SVD
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image.default(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, xlab="Row", ylab = "First left singular vector", pch=19)
plot(svd1$v[, 1], xlab="Column", ylab = "First left singular vector", pch=19)

par(mfrow=c(1,2))
plot(svd1$d, xlab="Column", ylab = "Singular value", pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab="Colum", ylab="Prop. of variance explained", pch=19)

svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale=TRUE)
plot(pca1$rotation[,1], svd1$v[,1], pch=19, xlab="Principal Component 1", ylab="Right Singular Vector 1")
abline(c(0,1))

#variance explained
constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image.default(t(constantMatrix)[, nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singular value", pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab="Column", ylab="Prop. of variance explained", pch=19)

set.seed(6789)
for(i in 1:40){
    coinFlip1 <- rbinom(1, size=1, prob = 0.5)
    coinFlip2 <- rbinom(1, size=1, prob = 0.5)
    if (coinFlip1){
        dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,5), each=5)
    }
    if (coinFlip2){
        dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,5),5)
    }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0,1),each=5), pch=19, xlab="Column", ylab = "Pattern 1")
plot(rep(c(0,1), 5), pch=19, xlab="Column", ylab = "Pattern 2")

plot(svd2$v[,1],xlab="Column",ylab="First right Singular vector", pch=19)
plot(svd2$v[,1],xlab="Column", ylab="Second right Singular vector", pch=19)

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,2))
plot(svd1$d,xlab="Column",ylab="Singular value", pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab="Column", ylab="Prop. of variance explained", pch=19)

#Missing values
library(impute)
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace = FALSE)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered))
svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2));plot(svd1$v[,1],pch=19);plot(svd2$v[,1],pch=19)

load("data/face.rda")
image.default(t(faceData)[, nrow(faceData):1])
svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2), pch=19)
approx1 <- svd1$u[,1] %*% t(svd1$v[,1])*svd1$d[1]
approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5])*t(svd1$v[,1:5])
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10])*t(svd1$v[,1:10])
par(mfrow=c(1,4))
image(t(approx1)[, nrow(approx1):1], main="(a)")
image(t(approx5)[, nrow(approx5):1], main="(b)")
image(t(approx10)[, nrow(approx10):1], main="(c)")

#Working with Color
heat.colors(volcano)
topo.colors(volcano)


library(grDevices)

library(RColorBrewer)
cols <- brewer.pal(3,"BuGn")
pal <- colorRampPalette(cols)
image.default(volcano,col=pal(20))

x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x,y)
plot(x,y,pch=19,col=rgb(0,0,0,0.02))
