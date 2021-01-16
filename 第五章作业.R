#第五章作业

#（1）chd设为因子变量，计算样本中冠心病比例
#从R包中读取数据
rm(list=ls())
setwd("/Users/Antony/OneDrive/CoreTech/ML&R/MLR_Scripts")
#install.packages("ElemStatLearn") 从本地安装了
library(ElemStatLearn)
#读取和描述性统计
data("SAheart")
summary(SAheart)
#统计冠心病比例
summary(SAheart)
head(SAheart)
SAheart$chd <- factor(SAheart$chd)
str(SAheart)
summary(SAheart)

attach(SAheart)
prop.table(table(chd))

#（2）分出训练测试集
set.seed(1)
训练集号码 <- sample(462,362) #只是抽取训练集的编号
train <- SAheart[训练集号码,]
test <- SAheart[-训练集号码,]

#（3）使用训练集逻辑回归
logit_fit <- glm(chd~.,data = train,family = binomial)
#?glm()
summary(logit_fit)

#（4）计算sudo R方
names(logit_fit)
sudo_r2 <- ((logit_fit$null.deviance-logit_fit$deviance)/logit_fit$null.deviance)
sudo_r2

#（5）计算平均边际效应并且画图
library(margins)
平均边际效应 <- margins(logit_fit)
summary(平均边际效应)
plot(平均边际效应, main="AME & Confidence Intervals")

#（6）在测试集中预测，计算准确率等等
测试集概率 <- predict(logit_fit, type = "response", newdata = test)
测试集预测 <- 测试集概率>0.5
table <- table(Predict=测试集预测,Actual=test$chd)
#各个指标开始填写
(Accuracy <- (table[1,1]+table[2,2])/sum(table))
(Error_rate <- (table[2,1]+table[1,2])/sum(table))
(Sensitivity <- table[2,2]/(table[1,2]+table[2,2]))
(Specificity <- table[1,1]/(table[1,1]+table[2,1]))
(Recall <- table[2,2]/(table[2,1]+table[2,2]))

#（7）画ROC曲线
install.packages("ROCR")
library(ROCR)
预测对象 <- prediction(测试集概率, test$chd)
表现 <- performance(预测对象,measure="tpr",x.measure="fpr")
#开始画图
plot(表现, main="ROC Curve", ldw=2,col="blue",xlab="real false", ylab='sensitivity')

#（8）计算AUC
auc_test <- performance(预测对象,measure = 'auc')
auc_test@y.values

#（9）计算卡帕
library(vcd)
Kappa(table)
