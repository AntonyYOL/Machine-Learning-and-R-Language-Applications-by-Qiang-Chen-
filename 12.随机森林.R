#第十二章作业

#12.1 混凝土数据进行回归
#（1）读取和划分数据，并且用随机森林回归
rm(list=ls())
library(AppliedPredictiveModeling)
data("concrete")

set.seed(1)
train_index <- sample(1030,730)
train <- concrete[train_index,]
test <- concrete[-train_index,]
summary(concrete)

#使用随机森林回归
set.seed(123)
library(randomForest)
fit <- randomForest(CompressiveStrength~.,data = train,mtry=9,importance=TRUE)
fit

#（2）变量重要性列表并且画图
importance(fit)
varImpPlot(fit,main="Variable Importance Plot")

#（3）画Age和Cement的偏依赖图
partialPlot(fit,train,x.var=Age)
partialPlot(fit,train,x.var=Cement)

#（4）在测试集预测并展示均方误差
fit.pred <- predict(fit,newdata = test)
mean((fit.pred-test$CompressiveStrength)^2)

#（5）寻找最优参数mtry
#最小化OBB
MSE <- numeric(9)
set.seed(123)
for (i in 1:9) {
  fit <- randomForest(CompressiveStrength~.,data = train,mtry=i)
  MSE[i] <- mean(fit$mse[500])
}
which.min(MSE)
plot(1:9,MSE,type = "b",xlab = "mtry",main="OOB Errors")
#tenfold 交叉验证
foldid <- sample(1:10, size=1030, replace=TRUE)
head(foldid)
MSE <- matrix(rep(0,80),ncol=10)
for (i in 1:8) {
  for(j in 1:10){
    train_cv <- concrete[foldid!=j,]
    holdout <- concrete[foldid==j,]
    fit <- randomForest(CompressiveStrength~.,data = train_cv,mtry=i)
    pred <- predict(fit,newdata = holdout)
    y.test <- holdout[,"CompressiveStrength"]
    MSE[i,j] <- mean((pred-y.test)^2)
  }
}
cv.error <- apply(MSE,1,mean)
min(cv.error)
which.min(cv.error)
plot(1:8,cv.error,type = "b",xlab = "mtry",main="CV Error")
abline(v=which.min(cv.error),lty=2)

#（6）通过测试集误差选择mtry
MSE <- numeric(8)
set.seed(123)
for(i in 1:8){
  fit <- randomForest(CompressiveStrength~.,data=train,mtry=i)
  pred <- predict(fit,newdata=test)
  y.test <- test$CompressiveStrength
  MSE[i] <- mean((pred-y.test)^2)
}
min(MSE)
which.min(MSE)
plot(1:8,MSE,type="b",xlab = "mtry",main="Test Error")
abline(v=which.min(MSE),lty=2)

#12.2 蘑菇数据分类
rm(list=ls())
library(cba)
library(randomForest)
data("Mushroom")

#（1）查找缺失值
sum(is.na(Mushroom))
summary(Mushroom)

#（2）填写缺失值
Mushroom <- na.roughfix(Mushroom)

#（3）修改变量名
str(Mushroom)
names(Mushroom) <- make.names(names(Mushroom))

#（4）分割训练测试集
set.seed(1)
train_index <- sample(8124,7124)
train <- Mushroom[train_index,]
test <- Mushroom[-train_index,]
fit <- randomForest(class~.,data = train,importance=TRUE)
varImpPlot(fit,main="Var importance plot")
  
#（5）在测试集中预测
pred <- predict(fit, newdata = test)
table <- table(pred,test$class)
table
(error_rate <- 1-sum(diag(table))/sum(table))
