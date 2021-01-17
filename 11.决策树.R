#第十一章作业

#11.1混凝土数据抗压强度回归

#（1）读取数据，用决策树回归
rm(list=ls())
library(AppliedPredictiveModeling)
data(concrete)
str(concrete)
summary(concrete)
set.seed(1)
train_index <- sample(1030,730)
train <- concrete[train_index,]
test <- concrete[-train_index,]
boxplot(concrete$CompressiveStrength)
library(rpart)
fit <- rpart(CompressiveStrength~.,data = concrete,subset=train_index)

#（2）画出估计的决策树
op <- par(no.readonly = TRUE)
par(mar=c(5,5,5,5))
plot(fit,margin=0.1)
text(fit)
par(op)

#（3）选择最优参数，并且画图
plotcp(fit)
fit$cptable
min_cp <-  fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
fit_best <- prune(fit, cp = min_cp)

#（5）测试集中预测并且计算均方误差
tree.pred <- predict(fit_best,newdata = test)
mean((tree.pred-test$CompressiveStrength)^2)   # MSE
plot(tree.pred,test$CompressiveStrength,main="Tree Prediction")
abline(0,1)  

fit_1se <- prune(fit,cp=0.02)
tree.pred.1se <- predict(fit_1se,newdata = test)
mean((tree.pred.1se-test$CompressiveStrength)^2)   # MSE

#（6）测试OLS的误差并且比较
ols.fit <- lm(CompressiveStrength~.,train)
ols.pred <- predict(ols.fit,test)
mean((ols.pred-test$CompressiveStrength)^2)   # MSE
plot(ols.pred,test$CompressiveStrength,main="OLS Prediction")
abline(0,1)


#11.2 预测蘑菇
#（1）划分训练集和测试集
install.packages("cba")
library(cba)
data(Mushroom)
str(Mushroom)
summary(Mushroom)
set.seed(123)
train_index <- sample(8124,7124)
train <- Mushroom[train_index,]
test <- Mushroom[-train_index,]
prop.table(table(Mushroom$class))
fit <- rpart(class~.,data = train)

#（2）选择最优cp并且画图展示
plotcp(fit)
fit$cptable
min_cp <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

#（3）通过最优cp画分类🌲
fit_best <- prune(fit, cp = min_cp)
op <- par(no.readonly = TRUE)
par(mar=c(1,1,1,1))
plot(fit_best,margin=0.1)
text(fit_best,cex=1.5)
par(op)

#（4）测试集中预测，展示混淆矩阵，并且计算准确率
tree.pred <- predict(fit_best,test,type="class")
y.test <- test$class
(table <- table(tree.pred,y.test))
(accuracy <- sum(diag(table))/sum(table))
(sensitivity <- table[2,2]/(table[1,2]+table[2,2]))

#（5）使用信息熵预测
fit <- rpart(class~.,data=train,parms=list(split="information"))
min_cp <-  fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
fit_best <- prune(fit, cp = min_cp)
tree.pred <- predict(fit_best,test,type="class")

#（6）混淆矩阵和准确率
(table <- table(tree.pred,y.test))
(accuracy <- sum(diag(table))/sum(table))

