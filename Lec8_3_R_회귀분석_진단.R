# 회귀분석 가정, 진단, 모델 수정
str(mtcars)
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data = mtcars)
plot(mtcars.lm)  #회귀분석 가정 진단

library(car)  # car 패키지 VIF 함수 사용
vif(mtcars.lm)

#변수 정규성 변환 계수 산정
powerTransform(mtcars$mpg)  # ramda = 0.029 -> 0에 근접, ln 변환
summary(powerTransform(mtcars$mpg))  #ramda = 1이라는 귀무가설 검정

#변수 선형성 변환 계수 산정
boxTidwell(mpg~hp+wt, data=mtcars)  # 변수 변환 필요

#종속변수의 등분산성 변환 계수 산정
spreadLevelPlot(lm(mpg~hp+wt, data = mtcars))

mtcars2 <- mtcars[, c("mpg", "wt", "disp", "hp", "drat")]
mtcars2$mpg <- log(mtcars2$mpg)
mtcars2$hp <-1/sqrt(mtcars2$hp) 
mtcars2$hp <-1/sqrt(mtcars2$wt) 

mtcars2.lm <- lm(mpg ~ hp + wt + drat, data = mtcars2)
plot(mtcars2.lm)  #회귀분석 가정 진단


