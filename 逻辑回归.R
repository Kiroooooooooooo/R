library(ggplot2)
library(Rcpp)
library(car)
library(pROC)
library(ROSE)
library(xlsx)
library(tidyverse)
library(caret)

dataG <- read.xlsx("C:/UserS/MarsR/Desktop/202015116/Australiadata.xlsx",1,header = T) 
summary(dataG)
trainlistG <- createDataPartition(dataG$Y,p=0.7,list = F)
View(trainlistG)
trainsetG <- dataG[trainlistG, ]
testsetG <- dataG[-trainlistG, ]
#查看训练集测试集大小
dim(trainsetG)
dim(testsetG)
#建模
dataG$Y <- factor(dataG$Y)
logitG <- glm(Y~., data = dataG, family = "binomial")
summary(logitG)$coef
coef(logitG)
step <-step(logitG,direction = 'both')
summary(step)
#模型拟合和回归诊断
influencePlot(logitG)

#预测
probabilities <- logitG %>% predict(testsetG, type = "response")
head(probabilities)
predicted.classes <- ifelse(probabilities > 0.5, 1,0)
head(predicted.classes)
mean(predicted.classes == testsetG$Y)
#画图
ran_roc <- roc(testsetG$Y,as.numeric(predicted.classes))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='logic模型ROC曲线')

