###Dimension Reduction
set.seed(12345)
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
heatmap(dataMatrix)

####Hierarchical Clustering
plot(as.dendrogram(hc))
abline(h = 1.5,col="blue")
abline(h =.4, col = "red")
abline(h= .05, col = "green")
dist(dFsm)

heatmap(dataMatrix,col = cm.colors(25))
heatmap(mt)

###Kmeans Clustering
points(cx,cy,col = c("red","orange","purple"),pch =3,cex = 2, lwd = 2)

mdist(x,y,cx,cy)
apply(distTmp,2,which.min)

points(x,y,pch=19,cex =2,col = cols1[newClust])
tapply(x,newClust,mean)
tapply(y,newClust,mean)
points(newCx,newCy,pch=8,col = cols1,cex = 2, lwd = 2)
mdist(x,y,newCx,newCy)
apply(distTmp2,2,which.min)
points(x,y,pch=19,cex =2,col = cols1[newClust2])
tapply(x,newClust2,mean)
tapply(y,newClust2,mean)

kmeans(dataFrame,3)
plot(x,y,col=kmObj$cluster,pch=19,cex=2)
points(kmObj$centers,col=c("black","red","green"),pch=3,cex=3,lwd=3)

#####Dimension Reduction
source("addPatt.R",local = TRUE)
heatmap(dataMatrix)
##singular value decompostition
svd(mat)
matu %*% diag %*% t(matv)
svd(scale(mat))

##principal component analysis 
prcomp(scale(mat))


###clustering example 
sub1 <- subset(ssd, subject ==1 )
names(sub1[1:12])
myedit("showXY.R")

mdist <- dist(sub1[,1:3])
hclustering <- hclust(mdist)
myplclust(hclustering,lab.col = unclass(sub1$activity))
mdist <- dist(sub1[,10:12])

svd1 <- svd(scale(sub1[,-c(562,563)]))
dim(svd1$u)
maxCon <- which.max(svd1$v[,2])
mdist <- dist(sub1[,c(10:12,maxCon)])
hclustering <- hclust(mdist)                   
myplclust(hclustering,lab.col = unclass(sub1$activity))

kClust = kmeans(sub1[,-c(562,563)],centers = 6)
table(kClust$cluster,sub1$activity)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart=100)
laying <- which(kClust$size==29)
plot(kClust$centers[laying, 1:12],pch=19,ylab="Laying Cluster")
names(sub1[1:3])
walkdown <- which(kClust$size==49)
plot(kClust$centers[walkdown, 1:12],pch=19,ylab="Walkdown Cluster")

