#第十章作业

#（1）展示数据结构，考察响应变量分布
#载入数据
rm(list=ls())
library(FNN)
library(mlbench)
data(PimaIndiansDiabetes)
#展示数据结构
summary(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)
attach(PimaIndiansDiabetes)
table(diabetes)
prop.table(table(diabetes))
test_label <- PimaIndiansDiabetes[-train_index,]$diabetes

#（2）画箱线图
boxplot(mass~diabetes,main='Boxplot for mass')

#（3）所有变量标准化
pidz <- as.data.frame(scale(PimaIndiansDiabetes[-9]))
apply(pidz,2,mean)
apply(pidz,2,sd)

#（4）划分测试集，用K=10估计
train_index <- sample(768,768-200)
train <- sample(pidz[train_index,])
test <- sample(pidz[-train_index,])
pred <- knn(train = train, test = test, cl = PimaIndiansDiabetes[train_index,]$diabetes, k=10, prob=TRUE)

#（5）预测并且展示混淆矩阵
(table <- table(pred,test_label))
(accuracy <- sum(diag(table))/sum(table))

#（6）改变参数寻找最佳k
accuracy <- numeric(50)
for (i in 1:50) {
  pred <- knn(train = train, test = test, cl = PimaIndiansDiabetes[train_index,]$diabetes, k=i, prob=TRUE)
  table <- table(pred,test_label)
  accuracy[i] <- sum(diag(table))/sum(table)
}
max(accuracy)
which.max(accuracy)
plot(accuracy,type = "b",xlab = "K",ylab="Accuracy",main = "Test Set Accuracy")

