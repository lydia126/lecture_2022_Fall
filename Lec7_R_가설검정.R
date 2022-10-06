# Lecture 7. Hypothesis tests and ANNOVA
 #1-1. 일표본 평균 검정: t.test()
  x<-rnorm(30, mean=0, sd=1)   # 평균 0, 표준편차 1인 정규분포 무작위 30개 추출
  par(mar=c(2,2,2,2))
  hist(x, main="Random draws from Std Normal", cex.axis=.8, xlim=c(-4,4))
  t.test(x, mu=0)    # 귀무가설 모집단 평균 u=0
  
  set.seed(3000)
  xseq<-seq(-4,4,.01)
  densities<-dnorm(xseq, 0,1)
  plot(xseq, densities, col="darkgreen", xlab="", ylab="Density", 
       type="l", cex=2, cex.axis=1.5)
  t.test(xseq)    # 귀무가설 모집단 평균 u=0

  x<-rnorm(30, mean=10, sd=1)
  t.test(x)    # 귀무가설 모집단 평균 u=0
  t.test(x, mu=10)    # 귀무가설 모집단 평균 u=10
  
  paldang <- read.csv("paldang.csv")
  t.test(paldang$pH, mu=mean(paldang$pH))
  t.test(paldang$pH, mu=7.5)
  result <- t.test(paldang$pH, mu=7.5)
  # printing the p-value
  result$p.value
  # printing the mean
  result$estimate
  # printing the confidence interval
  result$conf.int
  
  #1-2. 비모수 일표본 평균 검정: One-Sample Wilcoxon Signed Rank Test in R 
  paldang <- read.csv("paldang.csv")
  summary(paldang$Chla)
  shapiro.test(paldang$Chla)   # 정규성 확인 p > 0.05이면 정규분포, p < 0.05 비정규분포
  wilcox.test(paldang$Chla, mu = 16.0, alternative = "two.sided")  # "equal" or "not equal"
  wilcox.test(paldang$Chla, mu = 16.0, alternative = "less")
  
  #2. 독립(unpaird) 이표본 평균 검정: t.test()
  ?sleep   #t.test 예제, 약물 그룹별 환자(ID)의 수면 증가량
  sleep2<-sleep[,-3]  # ID 생략하고 약물 그룹별 효과 차이 검정
  library("ggpubr")
  ggboxplot(sleep2, x = "group", y = "extra",
            color = "group", palette = c("#00AFBB", "#E7B800"),
            ylab = "extra", xlab = "group")  
  tapply(sleep2$extra, sleep2$group, mean)   # 그룹으로 나누어 수면증가시간 평균값 구함
  var.test(extra~group, sleep2)   # 그룹간 수면증가시간의 분산 차이 검정
  t.test(extra~group, data=sleep2, paired=F, var.equal=T)  #독립 이표본, 등분산 평균 검정
  
  #3. 짝지은(paired) 이표본 평균 검정: t.test()
  with(sleep, t.test(extra[group==1], extra[group==2], paired = T))
  with(sleep, t.test(extra~group, paired = T, var.equal=T))
  
  #4. 이표본 분산
  with(iris, var.test(Sepal.Width, Sepal.Length)) 
  
  #5. 이표본 t-test (Parametric statistics)
  # using under the assumption that both samples are random, independent, and come from 
  # normally distributed population with unknow but equal variances
  a = c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)
  b = c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180)
  mean(a)
  mean(b)
  shapiro.test(a)
  shapiro.test(b)
  var.test(a,b)
  t.test(a,b, var.equal=TRUE, paired=FALSE)
  
  #6. 이표본 Mann-Whitney-Wilcoxon Test (Nonparametric statistics, 비모수 통계)
  # data: mtcars
  # the gas mileage data (mpg) for manual and automatic transmissions (am) are independent?
  head(mtcars)    # mpg is numeric, am is factor
  # Problem: decide at a=0.05 significance level if the gas mileage data of manual and 
  # automatic transmissions in mtcars have identical data distribution. 
  wilcox.test(mpg ~ am, data=mtcars) 
  # answer: At a=0.05 significance level, we conclude that the gas mileage data of manual 
  # and automatic transmissions in mtcar are nonidentical populations. 
  
  #7. 다중표본 Kruskal-Wallis Test
  # data: airquality
  head(airquality)
  # problem: Without assuming the data to have normal distribution, test at a=0.05 
  # if the monthly ozone density has identical distributions from May to September. 
  kruskal.test(Ozone ~ Month, data = airquality) 
  # Answer: At a=0.05 significance level, we conclude that the monthly ozone density 
  # from May to September 1973 are nonidentical populations. 
  airquality$Month<-as.factor(airquality$Month)
  str(airquality)
  res.aov <- aov(Ozone ~ Month, data = airquality)
  summary(res.aov)
  TukeyHSD(res.aov)
  plot(TukeyHSD(res.aov))
  
  #8. One-way ANOVA (ANalysis Of VAriance)
  ## Factor data로 그룹화 된 three-samples에 대한 t-test
  ## Null hypothesis: the means of the different groups are the same
  ## Alternative hypothesis: At least one sample mean is not equal to the others.
  my_data <- PlantGrowth
  # Show a random sample
  set.seed(1234)
  dplyr::sample_n(my_data, 10)
  # Show the levels
  levels(my_data$group)
  my_data$group <- ordered(my_data$group,
                           levels = c("ctrl", "trt1", "trt2"))
  library(dplyr)
  group_by(my_data, group) %>%
    summarise(
      count = n(),
      mean = mean(weight, na.rm = TRUE),
      sd = sd(weight, na.rm = TRUE)
    )
  library("ggpubr")
  ggboxplot(my_data, x = "group", y = "weight",
            color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
            order = c("ctrl", "trt1", "trt2"),
            ylab = "Weight", xlab = "Treatment")
  
  # Mean plots
  # ++++++++++++++++++++
  # Plot weight by group
  # Add error bars: mean_se
  # (other values include: mean_sd, mean_ci, median_iqr, ....)
  ggline(my_data, x = "group", y = "weight",
         add = c("mean_se", "jitter"),
         order = c("ctrl", "trt1", "trt2"),
         ylab = "Weight", xlab = "Treatment")
  
 # Compute the analysis of variance
  res.aov <- aov(weight ~ group, data = my_data)
  # Summary of the analysis
  summary(res.aov)
## As the p-value is less than the significance level 0.05, we can conclude that there are
## significant differences between the groups highlighted with “*" in the model summary.  
  
# Tukey multiple pairwise-comparisons
## Tukey HSD (Tukey Honest Significant Differences, R function: TukeyHSD()) for performing 
##  multiple pairwise-comparison between the means of groups.
  TukeyHSD(res.aov)  
# only the difference between trt2 and trt1 is significant  
  
# Multiple comparisons using multcomp package
#install.packages("multcomp")  
library(multcomp)
summary(glht(res.aov, linfct = mcp(group = "Tukey")))

# Pairewise t-test
pairwise.t.test(my_data$weight, 
                my_data$group,p.adjust.method = "BH") # Benjamini-Hochberg method

# Check ANOVA assumptions: test validity?
## The ANOVA test assumes that, the data are normally distributed and
## the variance across groups are homogeneous.
# Homogeneity of variances
## 1) residuals versus fits plot
plot(res.aov, 1)   # Points 17, 15, 4 are detected as outliers
## 2) use Bartlett’s test or Levene’s test to check the homogeneity of variances.
library(car)
leveneTest(weight ~ group, data = my_data)
## p > 0.05, we can assume the homogeneity of variances in the different groups

#9. Welch one-way test: ANOVA test with no assumption of equal variances
oneway.test(weight ~ group, data = my_data)
## Pairwise t-tests with no assumption of equal variances
pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH", pool.sd = FALSE)

## Check the normality assumption
## 1) Normality plot of residuals
plot(res.aov, 2)
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)
  
# 10. Kruskal-Wallis rank sum test
## use when ANOVA assumptions are not met
kruskal.test(weight ~ group, data = my_data)

# 11. Two-way ANOVA test
## evaluate simultaneously the effect of two grouping variables (A and B) on a response variable
## Two-way ANOVA test hypotheses
### 1) There is no difference in the means of factor A
### 2) There is no difference in means of factor B
### 3) There is no interaction between factors A and B
### The alternative hypothesis for cases 1) and 2) is: the means are not equal
### The alternative hypothesis for case 3) is: there is an interaction between A and B.

## (1) balanced designs - equal sample sizes within independent grouping levels
my_data <- ToothGrowth
# Show a random sample
set.seed(1234)
dplyr::sample_n(my_data, 10)
str(my_data)
# convert "dose" from numeric to factor
my_data$dose <- factor(my_data$dose,
                       levels = c(0.5, 1, 2),
                       labels = c("D0.5", "D1.0", "D2.0"))
str(my_data)
table(my_data$supp, my_data$dose)

library(dplyr)
group_by(my_data, supp, dose) %>%
  summarise(
    count = n(),
    mean = mean(len, na.rm = TRUE),
    sd = sd(len, na.rm = TRUE)
  )

## plot graphs
ggpubr::ggboxplot(my_data, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))

ggpubr::ggline(my_data, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))

# Compute two-way ANOVA test
## We want to know if tooth length depends on supp and dose.
res.aov2 <- aov(len ~ supp + dose, data = my_data)
summary(res.aov2)

# Two-way ANOVA with interaction effect (교호효과)
# These two calls are equivalent
res.aov3 <- aov(len ~ supp * dose, data = my_data)
summary(res.aov3)
res.aov4 <- aov(len ~ supp + dose + supp:dose, data = my_data)
summary(res.aov4)

TukeyHSD(res.aov3, which = "dose")
TukeyHSD(res.aov3, which = "supp")
library(multcomp)
summary(glht(res.aov2, linfct = mcp(dose = "Tukey")))

# Check the Homogeneity of variances
plot(res.aov3, 1)
leveneTest(len ~ supp*dose, data = my_data)

# Check the normality assumpttion
plot(res.aov3, 2)
# Extract the residuals
aov_residuals <- residuals(object = res.aov3)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

## (2) Unbalanced designs - unequal sample sizes within independent grouping levels
### Anova() [in car package] is used to compute two-way ANOVA test for unbalanced designs
library(car)
my_anova <- aov(len ~ supp * dose, data = my_data)
Anova(my_anova, type = "III")

# 12. MANOVA Test: Multivariate Analysis of Variance - multiple response variables
my_data <- iris
## want to know if there is any significant difference, in sepal and petal length, between the different species
res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(res.man)
# Look to see which differ
summary.aov(res.man)


res.man <- manova(cbind(Sepal.Length, Petal.Length, Sepal.Width, Petal.Width) ~ Species, data = iris)
summary(res.man)
# Look to see which differ
summary.aov(res.man)

