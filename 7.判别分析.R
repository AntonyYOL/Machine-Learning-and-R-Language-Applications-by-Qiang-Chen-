##第七章作业

#（1）载入数据，把响应变量设为因子，展示数据结构
#从R包中读取数据
rm(list=ls())
seeds <- read.table("seeds_dataset.txt", header = TRUE)
str(seeds)
summary(seeds)
#把type变成因子变量
seeds$X1 <- factor(seeds$X1)
table(seeds$X1)
prop.table(table(seeds$X1))

#（2）分训练和测试集
set.seed(1)
train_index <- sample(209,209*(2/3))
train <- seeds[train_index,]
test <- seeds[-train_index,]

#（3）画出第一和第二线性判别元的散点图
library(MASS)
fit <- lda(X1~.,data=train)
fit
summary(fit)
plot(fit,abbrev=TRUE,col=as.numeric(train$X1),main="Linear Discriminants")  # Abbreviate variable names

#（4）测试集中预测，展示混淆矩阵
#fit <- lda(X1~.,data=train)
class_test <- predict(fit,newdata=test)$class
table <- table(Predicted=class_test,Actual=test$X1)
table
cat("LDA Test Error Rate =",mean(class_test!=test$X1),"\n")
#科恩kappa
library(vcd)
Kappa(table)

#（5）进行二次判别分析
fit <- qda(X1~.,data=seeds)
fit

#（6）在测试集中，计算二次判别分析的混淆矩阵和准确率
class_test <- predict(fit,newdata=test)$class
table(Predicted=class_test,Actual=test$X1)
table
(Accuracy <- (table[1,1]+table[2,2])/sum(table))
