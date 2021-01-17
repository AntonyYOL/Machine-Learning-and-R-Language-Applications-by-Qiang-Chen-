#第十五章作业

#15.1预测Ablone的rings数据
#（1）把因子变量变成虚拟变量
rm(list=ls())
setwd("/Users/Antony/OneDrive/CoreTech/ML&R/MLR_Data")
library(AppliedPredictiveModeling)
data("abalone")
model.matrix(ablone$Tpye,data = abalone)
summary(abalone)
dummy <- model.matrix(~Type,data=abalone)
aba_data <- cbind(abalone[,-1],dummy[,-1])
y.test <- abalone[-train_index,"Rings"]

#（2）数据预处理，所有变量进行归一化处理
maxs <- apply(aba_data,2,max)
mins <- apply(aba_data, 2, min)
aba_s <- as.data.frame(scale(aba_data,center = mins,scale=maxs-mins))

#（3）选取1000个变量作为训练集
set.seed(1)
train_index <- sample(4177,1000)
train <- aba_s[train_index,]
test <- aba_s[-train_index,]
fit <- neuralnet(Rings~.,data = train,hidden = 3,act.fct = "logistic")

#（4）计算均方误差
plot(fit,fontsize = 10)
pred <- predict(fit,test)
pred <- pred*(max(abalone$Rings)-min(abalone$Rings))+min(abalone$Rings)
mean((pred-y.test)^2)

#（5）线性回归测试误差对比
fit.ols <- lm(Rings~., data=train)
pred.ols <- predict(fit.ols,test)
pred.ols <- pred.ols*(max(abalone$Rings)-min(abalone$Rings))+min(abalone$Rings)
mean((pred.ols-y.test)^2)
par(mfrow=c(1,2))
plot(pred,y.test,xlab="Prediction",ylab="rings",main="Neural Network")
abline(0,1)
plot(pred.ols,y.test,xlab="Prediction",ylab="rings",main="OLS Regression")
abline(0,1)
par(mfrow=c(1,1))

#（6）测试最小误差的神经元数目
MSE <- numeric(10)
for (i in 1:10){
  set.seed(123)
  fit <- neuralnet(Rings~.,data=train,hidden=i,act.fct = "logistic")
  pred <- predict(fit,test)
  pred <- pred*(max(abalone$Rings)-min(abalone$Rings))+min(abalone$Rings)
  MSE[i] <- mean((pred-y.test)^2)
}
min(MSE)
which.min(MSE)
plot(1:10,MSE,type = "b",xlab="hidden layers")

#15.2 使用R包mlbench中Ionosphere（电离层）数据，估计二分类问题
#（1）数据预处理：class变成因子变量，去掉无变化的V2，把两个水平的因子变量V1变成虚拟变量
rm(list=ls())
library(mlbench)
data("Ionosphere")
Ionosphere <- subset(Ionosphere,select = -c(V2))
#变量改名
library(reshape)
names(Ionosphere)[1]="V1-"
#利用package处理哑变量
library(caret)
str(Ionosphere)
dmi <- dummyVars(~.,data=Ionosphere)
dmio <- data.frame(predict(dmi,newdata=Ionosphere))

#（2）选取训练集，进行训练
set.seed(1)
train <- sample(351,251)
fit <- neuralnet(Class.good~.,data=dmio[train,],hidden=5,act.fct = "logistic")

#（3）测试集均方误差
prob <- predict(fit,dmio[-train,])
pred <- prob>0.5
(table <- table(Predicted=pred,Actual=dmio[-train,"Class.good"]))
(error <- 1-sum(diag(table))/sum(table))

#（4）对比逻辑回归
library(nnet)
fit.l <- multinom(Class.good~.,data=dmio[train,])
pred <- predict(fit.l,type="class",newdata = dmio[-train,])
(table <- table(Predicted=pred,Actual=dmio[-train,"Class.good"]))
(error <- 1-sum(diag(table))/sum(table))

#（5）在1:10之间for循环
MSE <- numeric(10)
for (i in 1:10){
  set.seed(123)
  fit <- neuralnet(Class.good~.,data=dmio[train,],hidden=i,act.fct = "logistic")
  prob <- predict(fit,dmio[-train,])
  pred <- prob>0.5
  (table <- table(Predicted=pred,Actual=dmio[-train,"Class.good"]))
  MSE[i] <- 1-sum(diag(table))/sum(table)
}
min(MSE)
which.min(MSE)
plot(1:10,MSE,type = "b",xlab="hidden layers")
