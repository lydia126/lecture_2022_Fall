# 2022 Fall Lecture 03. Descriptive Statistics Using R
## Objective: summarizing, describing and presenting data
## Example data: Paldang.csv, DC_2020.csv

# 1. Load data and view
dat <- read.csv("Paldang.csv")
head(dat) # first 6 observations
str(dat) # structure of dataset

# 2. Some statistics
min(dat$Chla)
max(dat$Chla)
range(dat$Chla)

## use function
range2 <- function(x) {
  range <- max(x) - min(x)
  return(range)
}
range2(dat$Chla)

mean(dat$Chla, na.rm = TRUE)  #compute the mean with the NA excluded
median(dat$Chla)
quantile(dat$Chla, 0.5)  # 50 percentile
IQR(dat$Chla) # the difference between the first and third quantile
temp <- quantile(dat$Chla, 0.75)-quantile(dat$Chla, 0.25)
temp
sd(dat$Chla) # standard deviation
var(dat$Chla) # variance

## use sapply, lapply libraries
sapply(dat[, 1:4], sd) # df 각 열의 함수(sd)를 벡터 또는 행렬 형식으로 출력 
lapply(dat[, 1:4], sd) # df 각 열의 함수(sd)를 list 형식으로 출력 

# 3. Summary statistics
summary(dat)

dc_dat <- read.csv("DC_2020.csv", na = "-", fileEncoding = "CP949", encoding = "UTF-8")
summary(dc_dat)
by(dc_dat[, 6:10], dc_dat$Depth, summary) # summary by group
by(dc_dat[, 6:10], dc_dat$Site, summary) # summary by group

## use package "pastecs" for stat.desc() library
#install.packages("pastecs")
library(pastecs)
stat.desc(dat, norm = TRUE)

# 4. mode
tab <- table(dat$Chla) # number of occurrences for each unique value
sort(tab, decreasing = TRUE) # sort highest to lowest

# 5. Plot
hist(dat$Chla)
boxplot(dat$Chla)
boxplot(dc_dat$Chl.a ~ dc_dat$Site)
boxplot(dc_dat$Chl.a ~ dc_dat$Depth)
plot(dat$Chla,type = "l") # "l" for line
plot(density(dat$Chla))

# Draw points on the qq-plot:
qqnorm(dat$Chla)
# Draw the reference line:
qqline(dat$Chla)

library(car) # package must be installed first
qqPlot(dat$Chla)

library(ggpubr)
ggqqplot(dat$Chla)

## use ggplot2

library(ggplot2)

ggplot(dat) +
  aes(x = Chla) +
  geom_histogram(bins = 30)

ggplot(dc_dat) +
  aes(x = Site, y = Chl.a) +
  geom_boxplot()

ggplot(dc_dat) +
  aes(x = Chl.a, y = T.P) +
  geom_point()

ggplot(dc_dat) +
  aes(x = Chl.a, y = T.P, colour = Site) +
  geom_point() +
  scale_color_hue()

ggplot(dat) +
  aes(x = Chla) +
  geom_density()
