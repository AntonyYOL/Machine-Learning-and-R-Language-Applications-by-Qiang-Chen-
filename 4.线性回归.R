#第四章作业

#4.4 Cobb-douglas
#（1）lny对lnk与lnl进行回归
#路径
rm(list=ls())
#读取数据
data <- read.csv("cobb_douglas.csv",header=TRUE)
str(data)
attach(data)
summary(data)
#第一次回归
fit <- lm(lny~lnk+lnl,data=data)
summary(fit)

#（2）画图
拟合 <- predict(fit,data)
plot(year,lny)
lines(year,拟合,col="blue")

#（3）加入交互
fit交互 <- lm(lny~lnk*lnl,data=data)
summary(fit交互)

#（4）加入平方
fit平方 <- lm(lny~lnk*lnl+I(lny^2)+I(lnk^2),data = data)
summary(fit平方)

#4.5 鲍鱼年龄预测
#（1）抽取训练集
#读取数据
#install.packages("AppliedPredictiveModeling")
rm(list=ls())
library(AppliedPredictiveModeling)
data(abalone)
鲍鱼 <- abalone
attach(鲍鱼)
summary(鲍鱼)
#抽取训练集
set.seed(1)
训练集号码 <- sample(4177,3177) #只是抽取训练集的编号
测试集 <- 鲍鱼[-训练集号码,]
训练集 <- 鲍鱼[训练集,]

#（2）使用训练集回归
fit <- lm(Rings~.,data=鲍鱼) #"."表示对所有变量进行回归
summary(fit)

#（3）在验证集中预测
预测 <- predict(fit,测试集)
y.test <- 测试集$Rings
MSE <- mean((预测-y.test)^2)

#（4）交叉验证
library(boot)
fit交叉 <- glm(Rings~.,data = 鲍鱼)
cv.err <- cv.glm(鲍鱼,fit交叉,K=10)
cv.err$delta

#（5）留一验证LOOKCV
cv.err <- cv.glm(鲍鱼,fit交叉)
cv.err$delta
