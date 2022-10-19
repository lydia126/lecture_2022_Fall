# Lecture 08-2. Linear Regression 
    
# 1. 단순 선형회귀 모델
## load data
    cars <- read.csv("cars.csv")
## linear regression between dist and speed    
    m<-lm(dist~speed, cars)
## 선형회귀결과 추출    
    coef(m) 
## 모델에 의해 적합된(fitted) 값 출력    
    fitted(m)[1:4]
## 잔차 
    residuals(m)[1:4] 
# 적합된 값과 잔차의 합은 실측값    
    fitted(m)[1:4]+residuals(m)[1:4]  
    cars$dist[1:4]
## 회귀계수의 신뢰구간
    confint(m)  #confidence interval of parameters
    deviance(m) #sum of squared residual
## 신뢰구간과 예측구간
    predict(m,newdata = data.frame(speed=c(3))) #회귀식을 이용해 특정 독립변수에 대한 종속변수예측
    predict(m,newdata = data.frame(speed=c(3)), interval="confidence") #종속변수의 신뢰구간
    predict(m,newdata = data.frame(speed=c(3)), interval="prediction") #종속변수의 예측구간 
## 모델평가
    summary(m)

## 선형회귀 모델의 평가 및 모델 간 비교(anova)
    anova(m) #선형회귀모델의 F 통계량 평가
    full<-lm(dist~speed,data=cars)    # 선형회귀
    full
    reduced <-lm(dist~1, data = cars)   # 상수 모델. 결과는 평균값과 동일
    reduced
    mean(cars$dist)
    anova(reduced, full)
## 회귀직선의 시각화
    plot(cars$speed, cars$dist)
    abline(m)
## 신뢰구간 표시
    summary(cars$speed) #X값 최소, 최대 찾기
    speed<-seq(min(cars$speed), max(cars$speed),.1)
    ys=predict(m, newdata = data.frame(speed=speed), interval = "confidence")
    matplot(speed, ys, type='n')   # Matrix 형태로 주어진 데이터 plot
    matlines(speed, ys, lty=c(1,2,2), col=c(2,1,1)) # Matrix 형태로 주어진 데이터 plot에 선을 그리기
    
# 2. 중회귀분석
    data(iris)
    m<-lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width, data=iris)
    summary(m)
## 범주형 변수 Species 포함
    m<-lm(Sepal.Length~., data=iris)
    summary(m)
## 모델 구조 확인
    model.matrix(m)[c(1,51,101),] #1행,51행,101행 데이터가 어떻게 모델에 사용되었는지 보여줌
    anova(m)
## 중회귀 모델의 시각화
## 잎 폭과 길이를 종별로 심볼을 다르게 플로팅
    with(iris,plot(Sepal.Width, Sepal.Length, cex=0.7, pch=as.numeric(Species)))
    m<-lm(Sepal.Length~Sepal.Width+Species, data=iris)
    coef(m)
    abline(2.25, 0.80, lty=1)
    abline(2.25+1.45, 0.8, lty=2)
    abline(2.25+1.94, 0.8, lty=3)
    legend("topright", levels(iris$Species), pch=1:3, bg="white")

# 3. gapminder 자료 분석        
    library(gapminder)
    data("gapminder")
    
    summary(gapminder)
    x<- mean(gapminder$gdpPercap)
    attach(gapminder)
    median(pop)
    hist(lifeExp)
    hist(log(pop))
    boxplot(lifeExp~continent)
    plot(lifeExp~gdpPercap)
    plot(lifeExp~log(gdpPercap))
    
## package dplyr - > use piper operator
    library(dplyr)
    gapminder %>%    # use pipe operator
      select(country, lifeExp) %>%
      filter(country == "South Africa" |
               country == "Ireland") %>%
      group_by(country) %>%
      summarise(Average_life=mean(lifeExp))
    
## t-test
    df1<-gapminder %>%    # use pipe operator
      select(country, lifeExp) %>%
      filter(country == "South Africa" |
               country == "Ireland")
    t.test(data=df1, lifeExp~country)
    
## ggplot
    library(ggplot2)
    
    gapminder%>%
      filter(gdpPercap<50000) %>%
      ggplot(aes(x=gdpPercap, y=lifeExp))+
      geom_point()
    
    gapminder%>%
      filter(gdpPercap<50000) %>%
      ggplot(aes(x=gdpPercap, y=lifeExp, col=continent))+
      geom_point(alpha=0.5)
    
    gapminder%>%
      filter(gdpPercap<50000) %>%
      ggplot(aes(x=gdpPercap, y=lifeExp, col=continent, size=pop))+
      geom_point(alpha=0.3)
    
    gapminder%>%
      filter(gdpPercap<50000) %>%
      ggplot(aes(x=log(gdpPercap), y=lifeExp, col=continent, size=pop))+
      geom_point(alpha=0.3)+
      geom_smooth()
    
    gapminder%>%
      filter(gdpPercap<50000) %>%
      ggplot(aes(x=log(gdpPercap), y=lifeExp, col=continent, size=pop))+
      geom_point(alpha=0.3)+
      geom_smooth(method = lm)
    
    gapminder%>%
      filter(gdpPercap<50000) %>%
      ggplot(aes(x=log(gdpPercap), y=lifeExp, col=continent, size=pop))+
      geom_point(alpha=0.3)+
      geom_smooth(method = lm)+
      facet_wrap(~continent)
    
    gapminder%>%
      filter(gdpPercap<50000) %>%
      ggplot(aes(x=log(gdpPercap), y=lifeExp, col=year, size=pop))+
      geom_point(alpha=0.3)+
      geom_smooth(method = lm)+
      facet_wrap(~continent)
    
## lm
    lm(lifeExp~gdpPercap)
    summary(lm(lifeExp~gdpPercap))
    summary(lm(lifeExp~gdpPercap+pop))

# 4. 단계별 회귀분석. 모델 선정
library(tidyverse)    # easy data manipulation and visualization
library(caret)        # easy machine learning workflow
library(leaps)        # computing stepwise regression
library(MASS)         # MASS::stepAIC() choose the best model by AIC.

## Fit the full model
View(swiss)
?swiss
full.model <- lm(Fertility ~., data = swiss)    
## Stepwise regression model
step.model <- stepAIC(full.model, direction = "both",trace = FALSE)
step.model <- stepAIC(full.model, direction = "both",trace = TRUE)    
## leaps::regsubsets() tuning parameter nvmax( max number of predictors to incorporate in the model)
models <- regsubsets(Fertility~., data = swiss, nvmax = 5,
                     method = "seqrep")
summary(models)

## swiss data stepwise mode using backward selection and 10-fold cross-validation
## Set seed for reproducibility
set.seed(123)
## Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
## Train the model
step.model <- train(Fertility ~., data = swiss,
                    method = "leapBackward",
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results
step.model$bestTune
## function summary() reports the best set of variables for each model size
summary(step.model$finalModel)
## The regression coefficients of the final model (id = 4) can be accessed as follow
coef(step.model$finalModel, 4)
lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, data = swiss)

## caret::train() has method to compute stepwise regression using the MASS package 
## (method = "lmStepAIC"):
step.model <- train(Fertility ~., data = swiss,
                    method = "lmStepAIC",
                    trControl = train.control,
                    trace = TRUE
)
## Model accuracy
step.model$results
## Final model coefficients
step.model$finalModel
## Summary of the model
summary(step.model$finalModel)
