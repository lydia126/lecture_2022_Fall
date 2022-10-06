# R Data Type 

#1) Vectors
# Create a vector
# When you want to create vector with more than one element, 
# you should use c() function which means to combine the elements into a vector

apple <- c('red','green',"yellow")  #벡터 생성은 C() 사용
print(apple)

# Get the class of the vector.
print(class(apple))

## 벡터 연산, 배열 할당
mean(x<-c(1,2,3))   # 값 할당은 <- 사용 할 것
x                   # 값 할당과 동시 연산 가능
mean(x=c(4,5,6))    # 연산은 되지만 값 할당 않됨
x

y<-c(1,3,4)
names(y)<-c("kim","seo","park")
y

matrix(1:9, nrow = 3, dimnames = list(c("r1","r2","r3"),c("c1","c2","c3")))

#2) Lists
# A list is an R-object which can contain many different types of elements inside it
# like vectors, functions and even another list inside it
# Create a list.
list1 <- list(c(2,5,3),21.3,sin)
# Print the list.
print(list1)

#3) Matrices
# A matrix is a two-dimensional rectangular data set. 
# It can be created using a vector input to the matrix function
library(matlib)
# Create a matrix.
M = matrix( c('a','a','b','c','b','a'), nrow = 2, ncol = 3, byrow = TRUE)
print(M)
M1 = matrix(1:9, nrow=3, dimnames=list(c("r1","r2","r3"),c("c1","c2","c3")))
M1
M1*2
M1%*%M1
M1%*%t(M1)

mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2", "C.3")))
mdat

A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
det(A)  # determinant 
inv(A)  # inverse matrix
A%*%inv(A)

#4) Arrays
# While matrices are confined to 2-D, arrays can be of any number of dimensions.
# Create an array.
a <- array(c('green','yellow'),dim = c(3,3,2))
print(a)
x<-array(1:12, dim = c(2,2,3))
x
x[1,1,1]
x[,,3]

#5) Factors
# Factors are the r-objects which are created using a vector. 
# It stores the vector along with the distinct values of the elements in the vector as labels. 
# The labels are always character irrespective of whether it is numeric or character or Boolean etc. in the input vector.
# Create a vector.
apple_colors <- c('green','green','yellow','red','red','red','green')

# Create a factor object.
factor_apple <- factor(apple_colors)

# Print the factor.
print(factor_apple)
print(nlevels(factor_apple))

#6) Data Frames
# Data frames are tabular data objects. 
# Unlike a matrix in data frame each column can contain different modes of data
# Data Frames are created using the data.frame() function
# Create the data frame.
BMI <- 	data.frame(
  gender = c("Male", "Male","Female"), 
  height = c(152, 171.5, 165), 
  weight = c(81,93, 78),
  Age = c(42,38,26)
)
print(BMI)

sub_BMI=BMI[c(2,3),c(1,2,3),drop=F]
print(sub_BMI)

#7) 변수 할당
# Assignment using equal operator.
var.1 = c(0,1,2,3)           

# Assignment using leftward operator.
var.2 <- c("learn","R")   

# Assignment using rightward operator.   
c(TRUE,1) -> var.3           

print(var.1)
cat ("var.1 is ", var.1 ,"\n")
cat ("var.2 is ", var.2 ,"\n")
cat ("var.3 is ", var.3 ,"\n")

var_x <- "Hello"
cat("The class of var_x is ",class(var_x),"\n")

var_x <- 34.5
cat("  Now the class of var_x is ",class(var_x),"\n")

var_x <- 27L
cat("   Next the class of var_x becomes ",class(var_x),"\n")

#8) Finding Variables
print(ls())
# List the variables starting with the pattern "var".
print(ls(pattern = "var"))   
print(ls(all.name = TRUE))

#9) Deleting Variables
rm(var.3)
print(var.3)
rm(list = ls()) # 모든 변수 삭제
print(ls())
