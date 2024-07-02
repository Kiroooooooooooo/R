#SVM

library(e1071)
library("pROC")
library(xlsx)

dataA<-read.xlsx(file="C:/Users/MarsR/Desktop/202015116/Australiadata.xlsx",1,header=TRUE)#读取数据
dataA$Y<-factor(dataA$Y)
#划分数据集
trainlistA <- createDataPartition(dataA$Y,p=0.7,list = F)
View(trainlistA)
trainsetA <- dataA[trainlistA, ]
testsetA <- dataA[-trainlistA, ]

#模型建立
svm_model = svm(Y~.,data=trainsetA,knernel = "radial",cost=0.1,gamma=0.01)
summary(svm_model)

#模型预测
svm_pred=predict(svm_model,testsetA,decision.values = TRUE)
testsetA$svm_pred = svm_pred
head(testsetA)
table(testsetA$Y,testsetA$svm_pred)


#绘制ROC曲线

ran_roc <- roc(testsetA$Y,as.numeric(svm_pred))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='SVM模型ROC曲线')


attr(svm_pred,"decision.values")[1:9,]
plot(cmdscale(dist(testsetA[,-10])),
     col = as.integer(testsetA[,10]),
     pch=c("o","+")[1:132 %in% svm_model$index+1])


