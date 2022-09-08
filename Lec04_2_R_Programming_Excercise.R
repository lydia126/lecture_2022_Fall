# Lecture 4. R programming

#1) 조건문
## if-else
x <- 15
if(x > 10) {
  y <- x*5
} else {
  y <- x/5
}
print(y)  

## ifelse 
X <- c(1,2,4,5,6)
ifelse(X%%2==0, "even", "odd")

## if-else if
x <-50 
if (x>=90){
  grade = 'A'
} else if (x>=80){
  grade='B'
} else if (x>=70){
  grade='c'
} else {
  grade='D'
print(grade)
}

for (x1 in c(50, 70, 100, 95, 85)) {
  if (x1>=90) {
    grade = 'A'
  } else if (x1>=80){
    grade='B'
  } else if (x1>=70){
    grade='c'
  } else {
    grade='D'
  }
  print(grade)
}

client<-"public"
net.price<-100
if(client=='private'){
  tot.price <- net.price * 1.12
} else if(client=='public'){
  tot.price <- net.price * 1.06
} else {
  tot.price <- net.price
}
print(tot.price)

#2) 반복문

## for
for (i in 1:7) {
  print(i^2)
}

x <-1:7
Looplees <- x^2        # R은 loop 사용없이 반복적 계산 용이
Looplees

Storage <- numeric(5)
for (i in 1:5){
  Storage[i] <- i^2
}
Storage
mean(Storage)

x <- c(-3,6,2,5,9)
storage1 <- numeric(5)
storage1
for (i in 1:5) {
  storage1[i]<-(x[i])^2
}
storage1

# vector에 대한 if 문 사용시 주의사항
Temp <- c(-4, 5, 10, -6, -40, 30)
if(Temp > 0) {
  print("warm")
} else {
  print("not so warm")
}

for(Temp in c(-4, 5, 10, -6, -40, 30)){
  if(Temp > 0) {
    print("warm")
  } else {
    print("not so warm")
  }  
}

# nested
for (i in 1:3){
  for (j in 1:2){
    print (i+j)
  }  #2
} #1

## while
i<-0
while(i<=9) {
  i<-i+1
  if(i%%2!=0) {
    next       #odd이면 프린터하지 않고 다음 숫자로 감
  }
  print(i)  
}

## repeat
i<-1
repeat {
  print(i)
  if(i>=10) {
    break       #i가 10보다 크거가 같으면 출력 중단
  }
  i<-i+1
}

## apply function in R
# Returns a vector or array or list of values obtained 
# by applying a function to margins of an array or matrix

Age<-c(56,34,67,33,25,28)
Weight<-c(78,67,56,44,56,89)
Height<-c(165, 171,167,167,166,181)
BMI_df<-data.frame(Age,Weight,Height)
BMI_df

# row wise sum up of dataframe using apply function in R
apply(BMI_df,1,sum)

# column wise sum up of dataframe using apply function in R
apply(BMI_df,2,sum)

# column wise mean of dataframe using apply function in R
apply(BMI_df,2,mean)

## lapply function in R
# takes list, vector or Data frame  as input and returns only list as output
lapply(BMI_df, function(BMI_df) BMI_df/2)
lapply(BMI_df, mean)

## sapply functionin 
# takes list, vector or Data frame  as input and returns only vector and matrix as output.
result<-sapply(BMI_df, function(BMI_df) BMI_df/2)
result
sapply(BMI_df, mean)
random <- c("This", "is", "random",  "vector")
sapply(random,nchar)

## tapply function in R
# apply a function to subsets of a vector
attach(iris)
# mean sepal length by species
tapply(iris$Sepal.Length, Species, mean)


#3) 연산
## NA의 처리
sum(c(1,2,3, NA))
sum(c(1,2,3,NA), na.rm=TRUE)
(x<-data.frame(a=c(1,2,3), b=c("a",NA,"c"), c=c("a","b",NA)))
#na.fail(x)   #NA가 포함되어 있으므로 실패
na.omit(x)   #NA가 포함된 행은 제외
na.exclude(x)   #NA가 포함된 행은 제외
na.pass(x)   #NA의 여부에 상관없이 통과

#4) 함수
## fibonacci number example
## 수학에서, 피보나치 수는 첫째 및 둘째 항이 1이며 그 뒤의 모든 항은 
## 바로 앞 두 항의 합인 수열이다. 처음 여섯 항은 각각 1, 1, 2, 3, 5, 8이다
fibo <- function(n){
  if(n==1 || n==2) {
    return(1)
  }
  return(fibo(n-1)+fibo(n-2))
}
fibo(1)
fibo(9)

# OConnor Dobbin Reaeration Coefficient
OConner <- function (vel, dep) 
{
  ka<-(3.93 * (vel^0.5))/(dep^1.5)
  return(data.frame(vel, dep, ka))
}
U <- seq(0.2, 1.0, 0.2)
OConner(vel=U,dep=1)

## 함수 내 가변 길이 인자
f <- function(...){
  args<-list(...)
  for (a in args) {
    print(a)
  }
}
f('I','love', 'r' )


#5) 스코프(Scope)
n <- 1
f <- function() {
  print (n)
}
f()

n<-1
f <- function() {
  n<-100
  print (n)
}
f()

f <- function() {
  a <- 1
  g <- function() {
    a <- 2
    print(a)
  }
  g()
  print(a)     #내부 블록 g함수에서 a를 2로 지정하려 했으나, 외부블록 f함수의 a=1로 유지
}
f()

f <- function() {
  a <- 1
  g <- function() {
    a <<- 2
    b <<- 2
    print(a)
    print(b)
  }
  g()
  print(a)     #내부 블록 g함수에서 <<-로 지정한 변수는 전역에 적용됨
  print(b)
}
f()
