#第十七章作业
rm(list=ls())

#17.1 iris K-Means分析
data(iris)
kfit <- kmeans(iris[,-5],3,nstart = 20)
(table <- table(Predicted=kfit$cluster,Actual=iris$Species))
(accuracy <- sum(diag(table))/sum(table))

#17.2 使用faithful数据进行聚类
data(faithful)
#（1）画散点图
plot(faithful,main="Data of Old Faithful Geyser")
par(mar=c(5,5,5,5))
#（2）K=2 进行聚类分析，展示聚类结果
fit <- kmeans(faithful,2)
plot(faithful,col=fit$cluster,main="Estimated Clusters(K=2)")
#（3）K=3 进行聚类分析，展示聚类结果
fit <- kmeans(faithful,3)
plot(faithful,col=fit$cluster,main="Estimated Clusters(K=3)")
#（4）通过手肘法则选择K
SSE <- numeric(15)
set.seed(1)
for (k in 1:15){
  fit <- kmeans(faithful,k,nstart=20)
  SSE[k] <- fit$tot.withinss
}
plot(1:15,SSE,xlab="K",type="b",main = "K-means Clustering")
abline(v=which.min(SSE),lty=2)
#（5）通过BIC信息准则选择K
BIC <- SSE+2*log(272)*(1:15)
plot(1:15,BIC,xlab="K",type="b",main = "K-means Clustering")
abline(v=which.min(SSE),lty=2)

#17.3 对faithful进行层次聚类分析
#（1）使用完全连接并画树状图
hc.complete <- hclust(dist(faithful),method = "complete")
par(mar=c(1,5,3,1))
plot(hc.complete,main = "Complete Linkage",cex=0.9,xlab="",sub="")
#（2）使用平均连接并画树状图
hc.average <- hclust(dist(faithful),method = "average")
plot(hc.average,main = "Average Linkage",cex=0.9,xlab="",sub="")
#（3）使用单一连接并画树状图
hc.single <- hclust(dist(faithful),method = "single")
plot(hc.single,main = "Single Linkage",cex=0.9,xlab="",sub="")
#（4）使用中心连接并画树状图
hc.complete <- hclust(dist(faithful),method = "centroid")
plot(hc.centroid,main = "Centroid Linkage",cex=0.9,xlab="",sub="")
#（5）通过树状图判断哪种连接方式最糟糕
#我觉得是single linkage
#（6）对于完全连接，假设K=2，用散点图展示聚类结果
fit <- cutree(hc.complete, k=3)
plot(faithful,col=fit,main="Data of Old Faithful Geyser")
par(mar=c(5,5,5,5))
