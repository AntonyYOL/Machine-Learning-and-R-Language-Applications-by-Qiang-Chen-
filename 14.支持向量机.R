#第十四章作业

#14.1 使用UCI数据集中液体超声波流量计数据Meter_D，进行多分类问题SVM估计，响应变量为流量计的四种状态
#（1）读取数据，把第44个变量设为因子
rm(list=ls())
meter <- read.table("Meter_D.csv", sep=",",header = TRUE)
meter$V44 <- factor(meter$V44)

#（2）随机选取100个观测值
set.seed(1)
train_index <- sample(180,100)
train <- meter[train_index,]
test <- meter[-train_index,]
str(train)
prop.table(table(train$V44))
fit <- svm(V44~.,data = train,kernel="linear",cost=0.5)
pred <- predict(fit,test)
(table <- table(Predicted=pred, Actual=test$V44))
(test_error <- 1-sum(diag(table))/sum(table))

#（3）使用二次核进行SVM估计
fit <- svm(V44~.,data = train,kernel="polynomial",cost=0.5,gamma=0.5)
pred <- predict(fit,test)
(table <- table(Predicted=pred, Actual=test$V44))
(test_error <- 1-sum(diag(table))/sum(table))

#（4）使用径向核进行SVM估计
fit <- svm(V44~.,data = train,kernel="radial",cost=1,gamma=1)
pred <- predict(fit,test)
(table <- table(Predicted=pred, Actual=test$V44))
(test_error <- 1-sum(diag(table))/sum(table))

#（5）通过交叉验证，选择最佳成本参数cost，并且计算最佳模型的准确率
tune.out <- tune(svm,V44~.,data=train,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
tune.out
fit_best <- tune.out$best.model
pred <- predict(fit_best,test)
table <- table(Predicted=pred, Actual=test$V44)
(test_error <- 1-sum(diag(table))/sum(table))

#（6）通过测试集的预测准确率，选择线性核最优成本参数cost，并画图展示
test.out <- tune(svm,V44~.,data=train,validation.x=test[,-"V44"],validation.y=test[,"V44"],kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
test.out
plot(test.out,xlab = "C",ylab = "Test Error",main = "Test Error of Test Dataset")

#14.2 使用白葡萄酒质量数据winequality-white.csv，进行多分类SVM估计，其中quality为响应变量，是白葡萄酒评级，为1～10整数，样本中为3-9
#（1）read.table() 读入数据，并且把quality作为因子
white <- read.table("winequality-white.csv",sep = ";",header = TRUE)
white$quality <- as.factor(white$quality)

#（2）考察quality的分布，并画柱状图
prop.table(table(white$quality))
plot(white$quality,xlab="level",ylab="numbers",main="Distribution of levels")

#（3）使用set.seed(1)，选取1000个观测值作为测试集
set.seed(1)
train_index <- sample(4898,4898-1000)
train <- white[train_index,]
test <- white[-train_index,]

#（4）使用SVM进行估计，展示混淆矩阵
fit <- svm(quality~.,data=train,gamma=1,cost=1)
pred <- predict(fit,test)
(table <- table(Predicted=pred, Actual=test$quality))
(test_error <- 1-sum(diag(table))/sum(table))

#（5）对比多项逻辑回归的准确率
library(nnet)
fit <- multinom(quality~.,data=train,maxit=500)
pred <- predict(fit,test)
(table <- table(Predicted=pred, Actual=test$quality))
(test_error <- 1-sum(diag(table))/sum(table))
