# Lec08-1. Correlation Analysis using R

## 0) Correlation analysis
library(MASS)
str(cats)

windows(width = 10, height = 10)
plot(cats$Hwt ~ cats$Bwt,
     col="forestgreen", pch = 19,
     xlab = "Body Weight (kg)", ylab = "Heart Weight (g)",
     main = "Body Wight and Heart Weight of Cats")

cor(cats$Bwt, cats$Hwt)
with(cats, cor(Bwt, Hwt))
?cor

# 2개변수 상관관계 유의성 검정
cor.test(cats$Bwt, cats$Hwt)   # if p-value < 0.05 상관관계 유의

cor.test(cats$Bwt, cats$Hwt, alternative = "greater", conf.level = 0.99) #상관계수가 0보다 크다
#  formular form
cor.test(~ Bwt + Hwt, data = cats)   #2개 변수에 대해서만 적용가능
cor.test(~ Bwt + Hwt, data = cats, subset = (Sex=="F"))


## 1) Use of cor() function to produce correlation
# install.packages("corrgram")
cor(iris$Sepal.Width, iris$Sepal.Length)
cor(iris[,1:4])

# 3개 이상변수 상관관계 유의성 검정
library(psych)
corr.test(iris[-5])
symnum(cor(iris[,1:4]))  #Symbolic Number Coding
print(corr.test(iris[-5]), short = FALSE)
str(state.x77)
cor(state.x77)

pairs.panels(state.x77, pch = 21, bg = "red", hist.col = "gold",
             main = "Correlation plot for US State Data")

library(corrgram)
corrgram(state.x77)
corrgram(state.x77, lower.panel = panel.shade, 
         upper.panel = panel.pie, text.panel = panel.txt,
         order = TRUE, main = "Corrgram of US State Data")

cols <- colorRampPalette(c("red", "pink", "green", "blue"))
corrgram(state.x77, col.regions= cols,  
         lower.panel = panel.pie, 
         upper.panel = panel.conf, text.panel = panel.txt,
         order = FALSE, main = "Corrgram of US State Data")

# 2) 편상상관계수
str(mtcars)
mtcars2 <- mtcars[, c("mpg", "cyl", "hp", "wt")]
cor(mtcars2)   # mpg와 hp의 순수 상관관계 확인 필요

library(ggm)   #편상관계수 구하는 pcor()함수 사용
pcor(c(1,3,2, 4), cov(mtcars2))  #cyl, wt 통제후 편상관계수 산정
pcor(c("mpg","hp", "cyl", "wt"), cov(mtcars2))

# 편상관계수 유의성 검정
pcor.test(pcor(c(1,3,2, 4), cov(mtcars2)),
          q=2, n = nrow(mtcars2))    # if p-value > 0.05 상관관계 유의성 없음 
library(ppcor)
pcor(mtcars2)
pcor.test(mtcars2["mpg"], mtcars2["hp"], mtcars2[c("cyl", "wt")])

#------------------------------------------------------------------------    
corrgram(iris, upper.panel=panel.conf)
  
cor.test(c(1,2,3,4,5),c(1,0,3,4,5), method="pearson")
cor.test(c(1,2,3,4,5),c(1,0,3,4,5), method="spearman")
cor.test(c(1,2,3,4,5),c(1,0,3,4,5), method="kendall")

## 2) Correlations/covariances among numeric variables in mtcars. 
# Use listwise deletion of missing data. 
cor(mtcars, use="complete.obs", method="kendall") 
cor(mtcars, use="pairwise.complete.obs", method="kendall") 
cov(mtcars, use="complete.obs") 
    
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
   upper.panel=panel.pie, text.panel=panel.txt,
   main="Car Milage Data in PC2/PC1 Order") 
    
corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse,
   upper.panel=panel.pts, text.panel=panel.txt,
   diag.panel=panel.minmax, 
   main="Car Mileage Data in PC2/PC1 Order") 
    
corrgram(mtcars, order=NULL, lower.panel=panel.shade,
   upper.panel=NULL, text.panel=panel.txt,
   main="Car Milage Data (unsorted)")
    
## 3) Correlations with significance levels
library(Hmisc)
#mtcars is a data frame as.matrix(mtcars)
# for rcorr, input must be a matrix and pairwise deletion is used. 
rcorr(as.matrix(mtcars), type="pearson") # type can be pearson or spearman
rcorr(as.matrix(mtcars), type="spearman") 

## 4) Use of corrplot package
library(corrplot)
M<-cor(mtcars)
head(round(M,2))
### Visualizing the correlation matrix  
corrplot(M, method="circle")
### 7 methods: ??circle??, ??square??, ??ellipse??, ??number??, ??shade??, ??color??, ??pie??
corrplot(M, type="upper") 
### 3 types: full, upper, lower     
corrplot(M, type="upper", order="hclust")
### ??hclust??  hierarchical clustering order is used 
corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45)
### tl.col( text label color) and tl.srt (text label rotation)
### To compute the matrix of p-value, a custom R function is used :
### mat : is a matrix of data
### ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          tmp <- cor.test(mat[, i], mat[, j], ...)
          p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
      }
      colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
      p.mat
    }
### matrix of the p-value of the correlation
 p.mat <- cor.mtest(mtcars)
 head(p.mat[, 1:5])
    
### Specialized the insignificant value according to the significant level
corrplot(M, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.01)
    
### Leave blank on no significant coefficient
corrplot(M, type="upper", order="hclust", 
       p.mat = p.mat, sig.level = 0.05, insig = "blank")
    
## 5)  Customize the correlogram    
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
### Combine with significance
p.mat = p.mat, sig.level = 0.01, insig = "blank", 
# hide correlation coefficient on the principal diagonal
       diag=FALSE 
    )

## 6) use corrplot and RColorBrewer
library(RColorBrewer)
M <-cor(mtcars)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))