##第八章作业

#（1）从R包中读取数据
rm(list=ls())
train <- read.csv("land_train.csv", header = TRUE)
test <- read.csv("land_test.csv", header = TRUE)
str(train)
summary(train)
#class变成因子
train$class <- factor(train$class)
test$class <- factor(test$class)
table(train$class)
prop.table(table(train$class))
plot(train$class, main="plot of classes")

#（2）进行朴素贝叶斯估计
library(e1071)
fit <- naiveBayes(class~.,data=train)
summary(fit)

pred_train <- predict(fit,newdata=train)
(table <- table(predict=pred_train,Acutal=train$class))

#（3）测试集中预测
pred_test <- predict(fit, newdata = test)
(table <- table(predict=pred_test,Acutal=test$class))
(Test_error_rate <- 1-sum(diag(table))/sum(table))

#（4）使用拉普拉斯修正
fit1 <- naiveBayes(class~., data=train, laplace = 2)
pred_train <- predict(fit1,newdata=train)
(table <- table(predict=pred_train,Acutal=train$class))

#（5）测试集中进行预测，展示混淆矩阵
pred_test <- predict(fit1,newdata=test)
(table <- table(Predict=pred_test,Acutal=test$class))
(Test_error_rate <- 1-sum(diag(table))/sum(table))

