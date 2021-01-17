#第十三章的作业

#13.1 使用concrete回归混凝土抗压强度
#（1）选取训练集，估计提升法模型
rm(list=ls())
library(AppliedPredictiveModeling)
library(gbm)
data(concrete)
set.seed(1)
train_index <- sample(1030, 730)
train <- concrete[train_index,]
test <- concrete[-train_index,]
set.seed(123)
fit <- gbm(CompressiveStrength~.,data=train,distribution="gaussian",n.trees=5000,cv.folds=5,interaction.depth=4)

#（2）变量重要性列表画图
summary(fit)

#（3）Age和Cement变量一维和二维偏依赖图
plot(fit,i.var="Age",main="Partial Dependence Plot",ylab="CompressiveStrength")
plot(fit,i.var="Cement",main="Partial Dependence Plot",ylab="CompressiveStrength")
library(viridis)
plot(fit,i.var=c("Age","Cement"),main="Partial Dependence Plot")

#（4）交叉误差随着决策树数量的变化，并且画图
gbm.perf(fit,method="cv")
abline(h=0,lty=2)
legend("top",legend=c("Training Error","CV Error"),lty=1,col=c("black","green"))

#（5）需要多少决策树达到最小值？这个最小值是多少
min(fit$train.error)
which.min(fit$train.error)
min(fit$cv.error)
which.min(fit$cv.error)

#（6）在测试集中预测，并且计算均方误差
pred <- predict(fit,newdata=test,n.trees=5000)
y.test <- test[,"CompressiveStrength"]
mean((pred-y.test)^2)
#试试看用CV最佳的
pred <- predict(fit,newdata=test,n.trees=1041)
y.test <- test[,"CompressiveStrength"]
mean((pred-y.test)^2)

#13.2 使用evtree的德国信用数据，进行二分类问题的梯度提升估计，有20个特征变量，响应变量位credit_risk，分为good和bad
#（1）预留300个观测值作为测试集
rm(list=ls())
#install.packages("evtree")
library(evtree)
data("GermanCredit")
set.seed(1)
train_index <- sample(1000,700)

#（2）所有变量变成虚拟变量
GCg <- as.data.frame(model.matrix(~.,GermanCredit)[,-1])
GCg <- GCg[,-31]
train <- GCg[train_index,]
test <- GCg[-train_index,]

#（3）估计提升树模型
library(gbm)
set.seed(123)
fit <- gbm(credit_riskbad~.,data = train, n.trees=1000,cv.folds = 5,shrinkage=0.01,interaction.depth = 2)

#（4）考察训练误差与交叉验证误差随决策树数目变化
gbm.perf(fit,method="cv")
abline(h=0,lty=2)
legend("top",legend=c("Training Error","CV Error"),lty=1,col=c("black","green"))
min(fit$train.error)
which.min(fit$train.error)
min(fit$cv.error)
which.min(fit$cv.error)

#（5）在测试集中预测，展示混淆矩阵和准确率
prob <- predict(fit,newdata=test,n.trees=1000,type="response")
pred <- prob > 0.5
y.test <- test[,"credit_riskbad"]
(table <- table(pred, y.test))
(error_rate <- 1-sum(diag(table))/sum(table))

prob <- predict(fit,newdata=test,n.trees=855,type="response")
(table <- table(pred, y.test))
(error_rate <- 1-sum(diag(table))/sum(table))
