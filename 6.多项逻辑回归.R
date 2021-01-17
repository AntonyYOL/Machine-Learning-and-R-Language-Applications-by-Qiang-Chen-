#第六章作业

#（1）wine的结构与响应变量分布
#读取数据
rm(list=ls())
#install.packages("rattle")
library(rattle)
data("wine")
attach(wine)
#描述性统计和响应变量分布
str(wine)
summary(wine)
table(Type)
prop.table(table(Type))
#控制小数点数目
options(digits=2)
prop.table(table(Type))

#（2）训练集和测试集
训练集号码 <- sample(178,100) #只是抽取训练集的编号
train <- wine[训练集号码,]
test <- wine[-训练集号码,]

#（3）测试集中进行预测
library(nnet)
fit <- multinom(Type~.,data = train)
options(digits = 3)
summary(fit)
prob_test <- predict(fit, type = "probs", newdata = test)
pred_test <- predict(fit, type = "class", newdata = test)
table <- table(Predicted=pred_test, Actual = test$Type)
Accuracy <- sum(diag(table))/sum(table)
Accuracy

#（4）计算kappa
library(vcd)
Kappa(table)

