# https://www.svm-tutorial.com/2014/10/support-vector-regression-r/

# use a simple regression data
data <- read.csv("regression.csv")
# Plot the data
plot(data, pch=16)

# 1) Linear regression model
model.lm <- lm(Y ~ X, data)

# Add the fitted line
abline(model.lm)

# make a prediction for each X
plot(data, pch=16)
model.lm <- lm(Y ~ X, data)
predictedY <- predict(model.lm, data)

# display the predictions
points(data$X, predictedY, col = "blue", pch=4, type = "l")

# RMSE
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- model.lm$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   
predictionRMSE # 5.703778

# 2) Support Vector Regression
library(e1071)
model.svm <- svm(Y ~ X, data)

predictedY.svm <- predict(model.svm, data)

plot(data, pch=16)
points(data$X, predictedY.svm, col = "red", pch=4)
points(data$X, predictedY.svm, col = "red", 
       pch=4, type = "l")

error <- model.svm$residuals
predictionRMSE <- rmse(error) 
predictionRMSE  #3.157061

# 3) Tuning support vector regression model
# perform a grid search
set.seed(123)
tuneResult <- tune(svm, Y ~ X,  data = data,
                   ranges = list(epsilon = seq(0,1,0.1), 
                                 cost = 2^(2:9)))
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)

tuneResult <- tune(svm, Y ~ X,  data = data,
                      ranges = list(epsilon = seq(0,0.2,0.01), 
                                    cost = 2^(2:9))
)

print(tuneResult)
plot(tuneResult)

tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, data)
error <- data$Y - tunedModelY
tunedModelRMSE <- rmse(error)
tunedModelRMSE  #2.1314
plot(data, pch=16)
points(data$X, predictedY.svm, col = "blue", pch=4)
points(data$X, tunedModelY, col = "red", pch=4)
points(data$X, predictedY.svm, col = "blue", pch=4, type = "l")
points(data$X, tunedModelY, col = "red", pch=4, type = "l")


# use data paldang BOD, COD
pd <- read.csv("Paldang.csv")
data <- pd[, c(3,4)]

# Plot the data
plot(data, pch=16)

# 1) Linear regression model
model.lm <- lm(COD ~ BOD, data)

# Add the fitted line
abline(model.lm)

# make a prediction for each X
plot(data, pch=16)
model.lm <- lm(COD ~ BOD, data)
predictedY <- predict(model.lm, data)

# display the predictions
points(data$BOD, predictedY, col = "blue", pch=4)

# RMSE
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- model.lm$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 0.3188758

# 2) Support Vector Regression
library(e1071)
model.svm <- svm(COD ~ BOD , data)

predictedY.svm <- predict(model.svm, data)

points(data$BOD, predictedY.svm, col = "red", pch=4)

error <- model.svm$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error) 
predictionRMSE  #0.3078978

# 3) Tuning support vector regression model
# perform a grid search
set.seed(123)
tuneResult <- tune(svm, COD ~ BOD,  data = data,
                      ranges = list(epsilon = seq(0,1,0.1), cost = 2^(1:9))
)
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)

tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, data)

error <- data$COD - tunedModelY

# this value can be different on your computer
# because the tune method  randomly shuffles the data
tunedModelRMSE <- rmse(error)  # 
tunedModelRMSE
plot(data, pch=16)
points(data$BOD, tunedModelY, col = "red", pch=4)

