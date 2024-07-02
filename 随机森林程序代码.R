#调包
library(randomForest)
library("pROC")
library(caret)
library(xlsx)
library(foreign)
library(party)
library(caret)
library(ggplot2)
#Japan
#导入数据
dataJ<-read.xlsx(file="C:/Users/MarsR/Desktop/202015116/Japandata.xlsx",1,header=TRUE)
summary(dataJ)
#划分数据集测试集
trainlistJ <- createDataPartition(dataJ$Y,p=0.7,list = F)
View(trainlistJ)
trainsetJ <- dataJ[trainlistJ, ]
testsetJ <- dataJ[-trainlistJ, ]
#查看训练集测试集大小
dim(trainsetJ)
dim(testsetJ)
#构建模型
set.seed(2022)
rf.trainJ <- randomForest(as.factor(Y)~.,data = trainsetJ,importance = T,proximity=TRUE,na.action = na.pass)
rf.trainJ
plot(rf.trainJ,main="randomforest origin")
#画决策森林
fit<-ctree(Y ~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15,
           data=trainsetJ,controls = ctree_control(maxsurrogate = 14))
fit
plot(fit)
#预测
rf.testJ <- predict(rf.trainJ,newdata=testsetJ,type="class")
rf.cfJ <- caret::confusionMatrix(as.factor(rf.testJ),as.factor(testsetJ$Y))
rf.cfJ #kappa值为0.7091，比较理想
#ROC/AUC
rf.testJ2 <- predict(rf.trainJ,newdata = testsetJ,type = "prob")
head(rf.testJ2)
roc.rfJ <- multiclass.roc(testsetJ$Y,rf.testJ2)
roc.rfJ #AUC值为0.9256比较理想
#输出变量重要性
importanceJ<-importance(rf.trainJ) 
write.csv(importanceJ,file="C:/Users/MarsR/Desktop/202015116/importanceJ.csv",row.names=T,quote=F)
barplot(rf.trainJ$importance[,1],main="输入变量重要性测度指标柱形图")
box()
#提取随机森林模型中以准确率递减方法得到维度重要性值
importance(rf.trainJ,type=1)
varImpPlot(x=rf.trainJ,sort=TRUE,n.var=nrow(rf.trainJ$importance),main="输入变量重要性测度散点图")
#信息展示
print(rf.trainJ)    #展示随机森林模型简要信息
hist(treesize(rf.trainJ))   #展示随机森林模型中每棵决策树的节点数
max(treesize(rf.trainJ));min(treesize(rf.trainJ))
MDSplot(rf.trainJ,trainsetJ$Y,palette=rep(1,2),pch=as.numeric(trainsetJ$Y))    #展示数据集在二维情况下各类别的具体分布情况
#检测
predJ<-predict(rf.trainJ,newdata=testsetJ)  
pred_out_1J<-predict(object=rf.trainJ,newdata=testsetJ,type="prob")  #输出概率
tableJ <- table(predJ,testsetJ$Y)  
sum(diag(tableJ))/sum(tableJ)  #预测准确率

#Australia
#导入数据
dataA<-read.xlsx(file="C:/Users/MarsR/Desktop/202015116/Australiadata.xlsx",1,header=TRUE)
summary(dataA)
#划分数据集测试集
trainlistA <- createDataPartition(dataA$Y,p=0.7,list = F)
View(trainlistA)
trainsetA <- dataA[trainlistA, ]
testsetA <- dataA[-trainlistA, ]
#查看训练集测试集大小
dim(trainsetA)
dim(testsetA)
#构建模型
set.seed(2022)
rf.trainA <- randomForest(as.factor(Y)~.,data = trainsetA,importance = T,na.action = na.pass,proximity = T)
rf.trainA
plot(rf.trainA,main="randomforest origin")
#画决策森林
fitA<-ctree(Y ~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14,
           data=trainsetA,controls = ctree_control(maxsurrogate = 3))
fitA
plot(fitA)
#预测
rf.testA <- predict(rf.trainA,newdata=testsetA,type="class")
rf.cfA <- caret::confusionMatrix(as.factor(rf.testA),as.factor(testsetA$Y))
rf.cfA #kappa值为0.7652，比较理想
#ROC/AUC
rf.testA2 <- predict(rf.trainA,newdata = testsetA,type = "prob")
head(rf.testA2)
roc.rfA <- multiclass.roc(testsetA$Y,rf.testA2)
roc.rfA #AUC值为0.9203比较理想
#输出变量重要性
importanceA<-importance(rf.trainA) 
write.csv(importanceA,file="C:/Users/MarsR/Desktop/202015116/importanceA.csv",row.names=T,quote=F)
barplot(rf.trainA$importance[,1],main="输入变量重要性测度指标柱形图")
box()
#提取随机森林模型中以准确率递减方法得到维度重要性值
importance(rf.trainA,type=1)
varImpPlot(x=rf.trainA,sort=TRUE,n.var=nrow(rf.trainA$importance),main="输入变量重要性测度散点图")
#信息展示
print(rf.trainA)    #展示随机森林模型简要信息
hist(treesize(rf.trainA))   #展示随机森林模型中每棵决策树的节点数
max(treesize(rf.trainA));min(treesize(rf.trainA))
MDSplot(rf.trainA,trainsetA$Y,palette=rep(1,2),pch=as.numeric(trainsetA$Y))    #展示数据集在二维情况下各类别的具体分布情况
#检测
predA<-predict(rf.trainA,newdata=testsetA)  
pred_out_1A<-predict(object=rf.trainA,newdata=testsetA,type="prob")  #输出概率
tableA <- table(predA,testsetA$Y)  
sum(diag(tableA))/sum(tableA)  #预测准确率
#Germandata
#导入数据
dataG<-read.xlsx(file="C:/Users/MarsR/Desktop/202015116/Germandata.xlsx",1,header=TRUE)
summary(dataG)
#划分数据集测试集
trainlistG <- createDataPartition(dataG$Y,p=0.7,list = F)
View(trainlistG)
trainsetG <- dataG[trainlistG, ]
testsetG <- dataG[-trainlistG, ]
#查看训练集测试集大小
dim(trainsetG)
dim(testsetG)
#构建模型
set.seed(2022)
rf.trainG <- randomForest(as.factor(Y)~.,data = trainsetG,importance = T,na.action = na.pass,proximity = T)
rf.trainG
plot(rf.trainG,main="randomforest origin")
#预测
rf.testG <- predict(rf.trainG,newdata=testsetG,type="class")
rf.cfG <- caret::confusionMatrix(as.factor(rf.testG),as.factor(testsetG$Y))
rf.cfG #kappa值为0.3991，不太理想
#ROC/AUC
rf.testG2 <- predict(rf.trainG,newdata = testsetG,type = "prob")
head(rf.testG2)
roc.rfG <- multiclass.roc(testsetG$Y,rf.testG2)
roc.rfG #AUC值为0.7919比较理想
