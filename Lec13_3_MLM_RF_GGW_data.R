library(DMwR2)

#1) 입력파일(csv) 읽어들이기
#1) 입력파일(csv) 읽어들이기
GGW_data<-  read.csv("GGW_data.csv") 
GGW_data1 <- GGW_data
GGW_data1$c.dominant <- GGW_data1$Cyano/(GGW_data1$Cyano+GGW_data1$Green+GGW_data1$Diatom)
GGW_data1 <- centralImputation(GGW_data1)
#GGW_data1 <- knnImputation(GGW_data1)

GGW_data1[!complete.cases(GGW_data1),]
nrow(GGW_data1[!complete.cases(GGW_data1),])

mydata <-subset(GGW_data1, select = c(Temp,temp.differ, EC, Q7day,APRCP7, TOC, TP, PO4P, TN, TN.TP, Fe, c.dominant))


#2) 비교모델로 중회귀모델 만들기
lm.fit <- lm(c.dominant~., data=mydata)
summary(lm.fit)
pr.lm <- predict(lm.fit,mydata)
RMSE.lm <- (sum((pr.lm-mydata$c.dominant)^2)/length(mydata$c.dominant))^(1/2)
RMSE.lm
summary(lm.fit)$r.squared
# R2, Adjusted R2 계산함수생성
R2gauss<- function(y,model){
  moy<-mean(y)
  N<- length(y)
  p<-length(model$coefficients)-1
  SSres<- sum((y-predict(model))^2)
  SStot<-sum((y-moy)^2)
  R2<-1-(SSres/SStot)
  Rajust<-1-(((1-R2)*(N-1))/(N-p-1))
  return(data.frame(R2,Rajust,SSres,SStot))
}
R2gauss(mydata$c.dominant, lm.fit)

# pr.lm.train <- predict(lm.fit,train)          #훈련 데이터 예측
# pr.lm <- predict(lm.fit,test)                 #데스트 데이터 예측
# MSE.lm <- sum((pr.lm - test$c.dominant_mini)^2)/nrow(test)
# MSE.lm.train <- sum((pr.lm.train - train$c.dominant_mini)^2)/nrow(train)
# MSE.lm
# MSE.lm.train

#3) RandonmForest Regresson Model 만들기

library(randomForest)

#3-1) beset lm model과 동일한 변수 적용
set.seed(1341)
c.dominant.rf2 <- randomForest(c.dominant~., mydata, ntree=500, mtry=8, importance=TRUE)
print(c.dominant.rf2)
imsi=importance(c.dominant.rf2)
View(imsi)
predicted.rf2 <- predict(c.dominant.rf2, newdata=mydata[,-12])
summary(c.dominant.rf2)
RMSE.rf2 <- (sum((predicted.rf2-mydata$c.dominant)^2)/length(mydata$c.dominant))^(1/2)
RMSE.rf2

c.dominant.rf2.r2 <- 1-sum((mydata$c.dominant-predicted.rf2)^2)/sum((mydata$c.dominant-mean(mydata$c.dominant))^2)
c.dominant.rf2.r2
c.dominant.rf2.Rajust<-1-(((1-c.dominant.rf2.r2)*(length(mydata$c.dominant)-1))/(length(mydata$c.dominant)-10-1))
c.dominant.rf2.Rajust

#3-3) RF Veriable Importance 선정 상위 5개 사용
set.seed(1341)
c.dominant.rf3 <- randomForest(c.dominant~EC+Temp+temp.differ+TOC+Q7day, mydata, ntree=500, mtry=4, importance=TRUE)
print(c.dominant.rf3)
imsi=importance(c.dominant.rf3)
View(imsi)
predicted.rf3 <- predict(c.dominant.rf3, newdata=mydata[,-12])
summary(c.dominant.rf3)
RMSE.rf3 <- (sum((predicted.rf3-mydata$c.dominant)^2)/length(mydata$c.dominant))^(1/2)
RMSE.rf3
c.dominant.rf3.r2 <- 1-sum((mydata$c.dominant-predicted.rf3)^2)/sum((mydata$c.dominant-mean(mydata$c.dominant))^2)
c.dominant.rf3.r2
c.dominant.rf3.Rajust<-1-(((1-c.dominant.rf3.r2)*(length(mydata$c.dominant)-1))/(length(mydata$c.dominant)-10-1))
c.dominant.rf3.Rajust

# 5-7) Scattered plots
par(mfrow=c(1,1))
par(mar=c(5.1, 6.1, 4.1, 4.1), xpd=TRUE) #margin bottom, left, top, right
plot(mydata$c.dominant,predicted.rf2,xlim=c(0,1), ylim=c(0,1), col='black',pch=16,cex=2.0, cex.axis=2.0, cex.lab=2.0, 
     xlab=expression(paste('Cyano_Rate Measured [%]',sep='')), 
     ylab=expression(paste('Cyano_Rate Simulated [%]',sep=''))
     )
points(mydata$c.dominant,pr.lm,col='black',pch=1,cex=1.8)
opts = c("p","l","o","b","c","s","S","h") 
x <- c(0:1); y<-x
lines(x,y, type=opts[2])
legend(0.0,1.2, legend=c("RF model", "MLR Model"), pch=c(16,1),pt.cex=c(2.0,2.0),col=c("black", "black"), cex=2.0, bty="n")

