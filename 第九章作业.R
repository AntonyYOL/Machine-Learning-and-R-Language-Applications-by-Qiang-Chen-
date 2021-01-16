##第九章惩罚回归

#（1）载入数据
rm(list=ls())
setwd("/Users/Antony/OneDrive/CoreTech/ML&R/MLR_Data")
mat <- read.csv("student-mat.csv",sep=";")
str(mat)
summary(mat)

#（2）删掉两行
#smat <- mat[,-c(-3,-2)]
smat <- mat[ , !names(mat) %in% c("G1", "G2")]
summary(smat)
summary(mat)

#（3）画G3直方图
table(smat$G3)
prop.table(table(smat$G3))
hist(smat$G3,main = "distribution of G3")

#（4）进行ridge回归，10折交叉验证选择最优参数lambda
#ridge回归
library(glmnet)
x <- model.matrix(G3~.,smat[,])[,-1]
y <- smat$G3
rfit <- glmnet(x,y,alpha=0)
#10fold选择参数
set.seed(1)
cvrfit <- cv.glmnet(x,y,alpha=0)
plot(cvrfit)
cvrfit$lambda.min

#（5）Lasso回归
lfit <- glmnet(x,y,alpha=1)
plot(lfit,xvar="lambda",label=TRUE)
#选择参数
cvrfit <- cv.glmnet(x,y,alpha=1)
plot(cvrfit)
cvrfit$lambda.min

#（6）elastic net回归
efit <- glmnet(x,y,alpha=0.5)
plot(lfit,xvar="lambda",label=TRUE)
#选择参数
cvrfit <- cv.glmnet(x,y,alpha=0.5)
plot(cvrfit)
cvrfit$lambda.min

#（7）for循环10fold选择参数
foldid <- sample(1:10, size = 395, replace = TRUE)
cv.error <- numeric(11)
for(i in 1:11){
  cvfit <- cv.glmnet(x,y,foldid=foldid, alpha=(i-1)/10)
  cv.error[i] <- min(cvfit$cvm)
}
cv.error
min(cv.error)
plot((1:11-1)/10,cv.error,type="b",xlab=expression(alpha),main="CV Error")

#（8）预留100个观测值作为测试集，选择最优的弹性网络参数
#训练测试
set.seed(1)
train <- sample(395,295)
cvfit <- cv.glmnet(x[train,],y[train],aplha=0)
bestlam <- cvfit$lambda.min
pred_train <- predict(efit,newx =x[train,],s=bestlam)  # use best lamdba for prediction
mean((pred_train-y[train])^2)

## Test error 
pred_test <- predict(efit,newx =x[-train,],s=bestlam )
mean((pred_test-y[-train])^2)

## Test error with 1se 
bestlam_1se <- cvfit$lambda.1se
pred_test_1se <- predict(efit,newx =x[-train,],s=bestlam_1se )
mean((pred_test_1se-y[-train])^2)
