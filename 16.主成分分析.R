#第十六章作业
rm(list=ls())

#16.1 使用主成分分析对spam进行降维
#（1）spam特征标准化之后进行主成分分析
library(ElemStatLearn)
data(spam)
spam <- spam[,-58]
fit <- prcomp(spam,scale=TRUE)
#（2）汇报方差分解的结果，多少个主成分达到80%
summary(fit)
#（3）画陡坡图
pve <- summary(fit)$importance[2,]  # proportional of variance explained
plot(1:57,pve,type="b",main="PVE", xlab="Principal Component", ylab="Proportion of Variance Explained")
#（4）画PVE图
plot(1:57,cumsum(pve),type="b",main="Cumulative PVE", xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained")
abline(h=0.8,lty=2)
#（5）画双标biplot图
sample <- sample(4601,400)
biplot(fit,cex=0.8,col=c(1:4))

fits <- prcomp(spam[sample,],scale=TRUE)
biplot(fits,cex=0.8,col=c(1:4))

#16.2 使用abalone数据进行pca
rm(list=ls())
#（1）去掉响应变量rings和因子变量type，其余变量进行主成分分析
library(AppliedPredictiveModeling)
data(abalone)
abalone <- abalone[,-grep("Rings|Type",colnames(abalone))]
fits <- prcomp(abalone,scale=TRUE)
fit <- prcomp(abalone)
#（2）汇报方差分解的结果，需要多少个主成分才能解释90%
summary(fits)
#（3）根据主成分载荷向量，解释每一主成分的差异
fits
fit
