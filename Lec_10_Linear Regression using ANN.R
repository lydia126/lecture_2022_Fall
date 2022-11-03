
# Step 1 ??? collecting data
## download the concrete.csv file from the Packt Publishing website
## https://github.com/PacktPublishing/Machine-Learning-with-R-Second-Edition/blob/master/Chapter%2007/concrete.csv

# Step 2 exploring and preparing the data
concrete <- read.csv("concrete.csv")
str(concrete)

## use normalize() for normalize data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)
summary(concrete$strength)

## partition the data into a training set(75%) and a testing set(25%)
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]


# Step 3 ??? training a model on the data
# install.packages("neuralnet")
library(neuralnet)
concrete_model <- neuralnet(strength ~ cement + slag 
              + ash + water + superplastic + coarseagg + fineagg + age, 
                            data = concrete_train)
plot(concrete_model)

# Step 4 ??? evaluating model performance
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)
plot(predicted_strength, concrete_test$strength)


# Step 5 improving model performance by adding a hindden layer with 5 units
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic +
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)
plot(concrete_model2)

model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
plot(predicted_strength2, concrete_test$strength)

# Extra work: Conventional Multiple Linear Regression  
MLR <-lm(strength ~ cement + slag 
         + ash + water + superplastic + coarseagg + fineagg + age, 
         data = concrete_train)
summary(MLR)
MLR_prediected=predict(MLR, newdata = concrete_test[1:8])
cor(MLR_prediected, concrete_test$strength)
plot(MLR_prediected, concrete_test$strength)


# Paldang Chl-a
data<-read.csv("Paldang.csv", header=TRUE, na.strings = "NA")
data_norm <- as.data.frame(lapply(data, normalize))

## partition the data into a training set(75%) and a testing set(25%)
Chla_ann <- neuralnet(Chla ~ .,
                             data = data_norm, hidden = 5)
plot(Chla_ann)

model_results3 <- compute(Chla_ann, data_norm[1:14])
predicted_Chla <- model_results3$net.result
cor(predicted_Chla, data_norm$Chla)
plot(predicted_Chla, data_norm$Chla)
