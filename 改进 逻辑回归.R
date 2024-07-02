library(ggplot2)
library(Rcpp)
library(car)
library(pROC)
library(ROSE)
library(xlsx)

dataG <- read.xlsx("C:/UserS/MarsR/Desktop/202015116/Germandata.xlsx",1,header = T) 
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
summary(logitG)
step <-step(logitG,direction = 'both')
summary(step)
#模型拟合和回归诊断
influencePlot(logitG)
# 使用测试集去预测模型
pred <- predict(step,testsetG,type='response')
head(pred)
fitted.r <- ifelse(pred>0.5,1,0)
# 模型的精度
accuracy <- table(fitted.r,testsetG$defect)
dim(fitted.r)
dim(testsetG$defect)#发现两个数据集均为空
#查看数据集
table(trainsetG$Y)
prop.table(table(trainsetG$Y))
#平衡结果
# 欠采样
balance.under <- ovun.sample(Y~.,data = trainsetG,p=0.5,seed = 1,method = "under")$data
# 查看均衡处理后的数据
table(balance.under$Y)
#重新建模
newlogitG =  glm(Y~.,data=balance.under,family=binomial,maxit=1000)

step = step(newlogitG,direction = "both")
summary(step)
#预测模型
pred = predict(step,testsetG,type="response")


fitted.r=ifelse(pred>0.5,1,0)
accuracy = table(fitted.r,testsetG$Y)

misClasificError = mean(fitted.r!=testsetG$Y)

#画图
ran_roc <- roc(testsetG$Y,as.numeric(pred))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='logic模型ROC曲线')
