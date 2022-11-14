# SVM Examples

library(e1071)   # for use svm()
library(ggplot2)

# 두개의 독립변수, iris 두 종류를 분류하는 예제
iris.part = subset(iris, Species != 'versicolor')
iris.part$Species = factor(iris.part$Species)
iris.part = iris.part[, c(1,2,5)]

# plot data
ggplot(iris.part, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape = Species),
             size=2)

# 1) svm linear model (cost = 1)
set.seed(123)
fit.linear = svm(Species ~ ., data=iris.part, type='C-classification', kernel='linear',
                 cost = 1, scale = FALSE)
summary(fit.linear)

# extract svm results
fit.linear$index
fit.linear$SV
iris.part[fit.linear$index, ]

ggplot(iris.part, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape = Species),
             size=2) +
  geom_point(data=iris.part[fit.linear$index, c(1,2)],
             color="darkblue", shape = 21,
             stroke=1.0, size =5)
w <- t(fit.linear$coefs) %*% fit.linear$SV
w
b <- -fit.linear$rho
b

# plot svm results
ggplot(iris.part, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape = Species),
             size=2) +
  geom_point(data=iris.part[fit.linear$index, c(1,2)],
             color="darkblue", shape = 21,
             stroke=1.0, size =5) +
  geom_abline(intercept = -b/w[1,2],
              slope = -(w[1,1]/w[1,2]),
              color = "dimgray", lty = "solid", lwd =1)

# Predict and test svm performance
fit.linear.pred <- predict(fit.linear, newdata=iris.part)
head(fit.linear.pred)

table(iris.part$Species, fit.linear.pred,
      dnn = c("Actual", "Predicted"))
mean(iris.part$Species==fit.linear.pred)

# Change Cost (1 -> 100)
set.seed(123)
fit.linear2 = svm(Species ~ ., data=iris.part, type='C-classification', kernel='linear',
                 cost = 100, scale = FALSE)
summary(fit.linear2)

# plot data
w <- t(fit.linear2$coefs) %*% fit.linear2$SV
w
b <- -fit.linear2$rho
b

ggplot(iris.part, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape = Species),
             size=2) +
  geom_point(data=iris.part[fit.linear2$index, c(1,2)],
             color="darkblue", shape = 21,
             stroke=1.0, size =5) +
  geom_abline(intercept = -b/w[1,2],
              slope = -(w[1,1]/w[1,2]),
              color = "dimgray", lty = "solid", lwd =1)

fit.linear2.pred <- predict(fit.linear2, newdata=iris.part)
head(fit.linear2.pred)

table(iris.part$Species, fit.linear2.pred,
      dnn = c("Actual", "Predicted"))
mean(iris.part$Species==fit.linear2.pred)


# 2) svm non-linear model
iris.part2 = subset(iris, Species != 'setosa')
iris.part2$Species = factor(iris.part2$Species)
iris.part2 = iris.part2[, c(1,2,5)]

ggplot(iris.part2, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape = Species),
             size=2)

# kernel = polynomial

fit.poly = svm(Species ~ ., data=iris.part2, type='C-classification',
               kernel='polynomial', degree=3, cost = 1)

summary(fit.poly)
plot(fit.poly, iris.part2)

# extract svm results
fit.poly$index
fit.poly$SV
iris.part[fit.poly$index, ]

ggplot(iris.part2, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape = Species),
             size=2) +
  geom_point(data=iris.part2[fit.poly$index, c(1,2)],
             color="darkblue", shape = 21,
             stroke=1.0, size =5)
w <- t(fit.poly$coefs) %*% fit.poly$SV
w
b <- -fit.poly$rho
b

plot(fit.poly, iris.part2)

# Predict and test svm performance
fit.poly.pred <- predict(fit.poly, newdata=iris.part2)
head(fit.poly.pred)

table(iris.part2$Species, fit.poly.pred,
      dnn = c("Actual", "Predicted"))
mean(iris.part2$Species==fit.poly.pred)


# kernel = radial, cost = 1 and gamma = 1

fit.radial = svm(Species ~ ., data=iris.part2, type='C-classification', 
                 kernel='radial', gamma=1, cost = 1)

summary(fit.radial)
plot(fit.radial, iris.part2)
fit.radial.pred <- predict(fit.radial, newdata=iris.part2)
head(fit.radial.pred)

table(iris.part2$Species, fit.radial.pred,
      dnn = c("Actual", "Predicted"))
mean(iris.part2$Species==fit.radial.pred)

# kernel = radial, cost = 1 and gamma = 10

fit.radial2 = svm(Species ~ ., data=iris.part2, type='C-classification', 
                  kernel='radial', gamma=10, cost = 1)

summary(fit.radial2)
plot(fit.radial2, iris.part2)

fit.radial2.pred <- predict(fit.radial2, newdata=iris.part2)
head(fit.radial2.pred)

table(iris.part2$Species, fit.radial2.pred,
      dnn = c("Actual", "Predicted"))
mean(iris.part2$Species==fit.radial2.pred)


# 3) Use AER package
# install.packages("AER")
library(AER)
data("Affairs")
str(Affairs)

aff <- Affairs
aff$affairs <- factor(ifelse(aff$affairs > 0, 1, 0),
                      levels = c(0, 1),
                      labels = c("No", "Yes"))
str(aff)
table(aff$affairs)
prop.table(table(aff$affairs))

set.seed(123)
train <- sample(nrow(aff), 0.7*nrow(aff))
aff.train <- aff[train, ]
aff.test <- aff[-train, ]
table(aff.train$affairs)
table(aff.test$affairs)

set.seed(123)
aff.svm <- svm(affairs ~ ., data = aff.train)

summary(aff.svm)

aff.svm.pred <- predict(aff.svm, newdata = aff.test)
head(aff.svm.pred)
table(aff.test$affairs, aff.svm.pred,
      dnn = c("Actual", "Predicted"))
mean(aff.test$affairs==aff.svm.pred)

set.seed(123)
aff.svm2 <- svm(affairs ~ ., data = aff.train, 
                probability = TRUE)
aff.svm.pred2 <- predict(aff.svm2, newdata = aff.test,
                         probability = TRUE)
str(aff.svm.pred2)
attr(aff.svm.pred2, "probabilities")[1:6,]

# tunning hyperparameters: gamma and cost
set.seed(123)
aff.svm.tuned <- tune.svm(affairs ~ ., data=aff.train,
                          gamma = 10^(-3:3),
                          cost=2^(-5:5))
summary(aff.svm.tuned)

aff.svm.tuned$best.model$gamma
aff.svm.tuned$best.model$cost

set.seed(123)
aff.svm <- svm(affairs ~ ., data = aff.train,
               gamma=0.001, cost=0.03125)
aff.svm.pred <- predict(aff.svm, newdata = aff.test)
head(aff.svm.pred)
table(aff.test$affairs, aff.svm.pred,
      dnn = c("Actual", "Predicted"))
mean(aff.test$affairs==aff.svm.pred)

#3) multiple classification
set.seed(123)
train <- sample(nrow(iris), 0.7*nrow(iris))
iris.train <- iris[train, ]
iris.test <- iris[-train, ]
table(iris.train$Species)
table(iris.test$Species)

set.seed(123)
iris.svm <- svm(Species ~ ., data = iris.train)
summary(iris.svm)

# plot
library(ggplot2)
iris.mds <- data.frame(cmdscale(dist(iris.train[,-5])))
ggplot(iris.mds, aes(x=X1, y=X2)) +
  geom_point(aes(color=iris.train[,5],
                 shape=iris.train[,5]), size=2) +
  geom_point(data=iris.mds[iris.svm$index, ],
             color="dimgray", shape=21, 
             stroke=1.0, size=5) +
    labs(color="Species", shape="Species", x="Dimension 1", y = "Dimension 2",
         title = "SVM multi-classification for IRIS data") +
    theme(plot.title = element_text(face="bold"))

iris.svm.pred <- predict(iris.svm, iris.test)
table(na.omit(iris.test)$Species, iris.svm.pred, 
      dnn=c("Acutual", "Predicted"))
mean(na.omit(iris.test)$Species==iris.svm.pred)
  