#调包
library(randomForest)
library("pROC")
library(caret)
library(xlsx)
library(foreign)
library(party)
library(caret)
library(ggplot2)
library(C50)
library(rpart)
library(rpart.plot)

#Australia
#导入数据
dataA<-read.xlsx(file="C:/Users/MarsR/Desktop/202015116/Australiadata.xlsx",1,header=TRUE)
summary(dataA)
#划分数据集测试集
trainlistA <- createDataPartition(dataA$Y,times = 1,p=0.7,list = F)
View(trainlistA)
trainsetA <- dataA[trainlistA, ]
testsetA <- dataA[-trainlistA, ]
#查看训练集测试集大小
dim(trainsetA)
dim(testsetA)
#构建模型
library(rpart)
dtree<-rpart(Y~.,data=trainsetA,method="class", parms=list(split="information"))
printcp(dtree)
#plotcp(dtree)
print(dtree)
#tree<-prune(dtree,cp=0.013575)
tree<-prune(dtree,cp=dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"])
opar<-par(no.readonly = T)
par(mfrow=c(1,2))
library(rpart.plot)
png(file = "C:/Users/MarsR/Desktop/202015116/tree1.png")
rpart.plot(dtree,branch=1,type=2, fallen.leaves=T,cex=0.8, sub="剪枝前")
png(file = "C:/Users/MarsR/Desktop/202015116/tree2.png")
rpart.plot(tree,branch=1, type=4,fallen.leaves=T,cex=0.8, sub="剪枝后")
par(opar)
dev.off()
predtree<-predict(tree,newdata=testsetA,type="class")   #利用预测集进行预测
table(testsetA$Y,predtree,dnn=c("真实值","预测值"))    #输出混淆矩阵