library(randomForest)

#1) 입력파일(csv) 읽어들이기
data<-read.csv("DR_NAF.csv", header=TRUE, na.strings = "NA")

# 2) 결측치(NA) 확인 및 처리  
data[!complete.cases(data),]
sum(is.na(data))
apply(data,2,function(x) sum(is.na(x))) #결측치 확인 Chla 86개

data1 <- na.omit(data) #결측치 행 삭제
apply(data1,2,function(x) sum(is.na(x))) #결측치 제거후

# 3) 학습데이터(75%)와 검정데이터(25%) 나누기 
index <- sample(1:nrow(data1),round(0.75*nrow(data1)))
train <- data1[index,]
test <- data1[-index,]

# 4) 비교모델로 중회귀모델 만들기
lm.fit <- glm(NAF~., data=train)
summary(lm.fit)
pr.lm.train <- predict(lm.fit,train)          #훈련 데이터 예측
pr.lm <- predict(lm.fit,test)                 #데스트 데이터 예측
MSE.lm <- sum((pr.lm - test$NAF)^2)/nrow(test)


# 5) RandonmForest Regresson Model 만들기
set.seed(1341)
NAF.rf <- randomForest(NAF ~ ., train)
print(NAF.rf)
predicted.train= predict(NAF.rf, newdata = train[,1:6])
predicted.test= predict(NAF.rf, newdata = test[,1:6])
plot(train$NAF, predicted.train, xlab="Observed", ylab="Predicted", main="Train Period")
plot(test$NAF, predicted.test, xlab="Observed", ylab="Predicted", main="Test Period")

# 6) RF 모델과 중회귀 모델 플로팅  

# 6-1) Scattered plots

# Training Period
par(mar=c(1,1,1,1))
plot(train$NAF,predicted.train,col='red',main='Training Period',pch=18,cex=1.0, xlab="Observed", ylab="Simulated")
points(train$NAF,pr.lm.train,col='blue',pch=18,cex=1.0)
abline(0,1,lwd=2)

# Testing Period
plot(test$NAF,predicted.test,col='red',main='Testing Period',pch=18,cex=1.0, xlab="Observed", ylab="Simulated")
points(test$NAF,pr.lm,col='blue',pch=18,cex=1.0)
abline(0,1,lwd=2)

# 6-2) Time Series plots

# Training Period
plot(predicted.train,col='red',main='Training Period',pch=18,cex=1.0, type="l", xlab="Data Index", ylab="NAF")
lines(pr.lm.train,col='blue',pch=18,cex=1.0)
points(train$NAF,col='black',pch=18,cex=1.0)

# Testing Period
plot(predicted.test,col='red',main='Testing Period',pch=18,cex=0.7, type="l", xlab="Data Index", ylab="NAF")
lines(pr.lm,col='blue',pch=18,cex=0.7)
points(test$NAF,col='black',pch=18,cex=1.0)
