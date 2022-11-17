data<-read.csv("Paldang.csv", header=TRUE, na.strings = "NA")
data1 <- na.omit(data)
#plot(data1)

#Partioning data into train and test
index <- sample(1:nrow(data1),round(0.60*nrow(data1)))
train <- data1[index,]
test <- data1[-index,]

#linear model
lm.fit <- glm(Chla~., data=train)
summary(lm.fit)
pr.lm.train <- predict(lm.fit,train)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$Chla)^2)/nrow(test)
MSE.lm

#Data Normalization for ANN model
maxs<-apply(data1,2, max)
mins<-apply(data1,2, min)
scaled <- as.data.frame(scale(data1, center = mins, scale = maxs - mins))

train.scaled<- scaled[index,]
test.scaled <- scaled[-index,]

#ANN Model coding
library(neuralnet)
n<-names(train.scaled)
f<-as.formula(paste("Chla ~", paste(n[!n %in%"Chla"], collapse = "+")))
nn<-neuralnet(f, data=train.scaled, hidden = c(5,3), linear.output = T)
plot(nn)

#Predicting Chla using the neural network
pr.nn.train <- compute(nn,train.scaled[,1:14])
pr.nn <- compute(nn,test.scaled[,1:14]) 
pr.nn.train <- pr.nn.train$net.result*(max(data1$Chla)-min(data1$Chla))+min(data1$Chla)   #Reverse Sacling
pr.nn <- pr.nn$net.result*(max(data1$Chla)-min(data1$Chla))+min(data1$Chla)   #Reverse Sacling
train.r <- (train.scaled$Chla)*(max(data1$Chla)-min(data1$Chla))+min(data1$Chla)
test.r <- (test.scaled$Chla)*(max(data1$Chla)-min(data1$Chla))+min(data1$Chla)

MSE.nn <- sum((pr.nn - test$Chla)^2)/nrow(test)
MSE.nn

#SVM regression
library(e1071)
library(caret)
set.seed(123)
svm_reg = svm(Chla~., data=train, scale = TRUE)
print(svm_reg)

#Predicting Chla using the svm
pr.svm.train <- predict(svm_reg,train)
pr.svm.test <- predict(svm_reg,test)

# accuracy check
MSE.svm <- sum((pr.svm.test - test$Chla)^2)/nrow(test)
MSE.svm

#ANN  & lm results scatter plots
par(mar=c(1,1,1,1))
windows(width = 10, height = 10)
par(mfrow=c(2,2))
plot(train$Chla,pr.nn.train,col='red',main='Training Period',pch=18,cex=1.0, xlab="Observed", ylab="Simulated")
points(train$Chla,pr.lm.train,col='blue',pch=18,cex=1.0)
points(train$Chla,pr.svm.train,col='grey',pch=18,cex=1.0)
abline(0,1,lwd=2)
legend("topright",legend=c('NN','LM', 'SVM'),pch=18,col=c('red','blue', 'grey'), horiz = TRUE)

plot(test$Chla,pr.nn,col='red',main='Testing Period',pch=18,cex=1.0, xlab="Observed", ylab="Simulated")
points(test$Chla,pr.lm,col='blue',pch=18,cex=1.0)
points(test$Chla,pr.svm.test,col='grey',pch=18,cex=1.0)
abline(0,1,lwd=2)
legend("topright",legend=c('NN','LM', 'SVM'),pch=18,col=c('red','blue', 'grey'), horiz = TRUE)

#ANN  & lm results line plots
plot(pr.nn.train,col='red',main='Training Period',pch=18,cex=1.0, type="l", xlab="Data Index", ylab="Chla")
lines(pr.lm.train,col='blue',pch=18,cex=1.0)
lines(pr.svm.train,col='grey',pch=18,cex=1.0)
points(train$Chla,col='black',pch=18,cex=1.0)
legend("topright",legend=c('NN','LM','SVM', 'Obs'),pch=18,col=c('red','blue','grey', 'black'), horiz = TRUE)

plot(pr.nn,col='red',main='Testing Period',pch=18,cex=0.7, type="l", xlab="Data Index", ylab="Chla")
lines(pr.lm,col='blue',pch=18,cex=0.7)
lines(pr.svm.test,col='grey',pch=18,cex=0.7)
points(test$Chla,col='black',pch=18,cex=1.0)
legend("topright",legend=c('NN','LM','grey','Obs'),pch=18,col=c('red','blue','grey','black'), horiz = TRUE)

