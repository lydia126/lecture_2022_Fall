# 1. Syntax - Helpful conventions for wrangling
## install.packages('dplyr',repos='http://cran.us.r-project.org')
library(dplyr)
library(tidyr)

## pipe operator %>%  (Ctl+Shit+m)
### Passes object on LHS as first argument of function on RHS.
iris_tbl <- as.tbl(iris)
iris_tbl %>%
  group_by(Species) %>%
  summarise(avg = mean(Sepal.Width)) %>%
  arrange(avg)

iris %>% 
  group_by(Species) %>% 
  summarise(avg = mean(Sepal.Width, na.rm = TRUE))

iris %>%
  group_by(Species) %>%
  summarise(avg = mean(Sepal.Width))

iris %>% select(1:4) %>% 
  filter(Sepal.Length >= 5) %>% 
  mutate(ratio = round(Sepal.Length/Sepal.Width, 2)) %>% 
  arrange(ratio)

# 2. Tidy Data
## Tidy data complements R’s vectorized operations. 
## R will automatically preserve observations as you manipulate variables

## 1) gather: Gather columns into rows
### Function: gather(data, key, value, ..., na.rm = FALSE, convert = FALSE)
#### Same as: data %>% gather(key, value, ..., na.rm = FALSE, convert = FALSE)
DF <-read.csv("tidy_data.csv")
long_DF <- DF %>% gather(Quarter, Revenue, Qtr.1:Qtr.4)
head(long_DF, 10)  # View only 10 data 

## 아래 명령도 동일한 결과 출력함
(DF %>% gather(Quarter, Revenue, -Group, -Year))
(DF %>% gather(Quarter, Revenue, 3:6))
(DF %>% gather(Quarter, Revenue, Qtr.1, Qtr.2, Qtr.3, Qtr.4))

## 2) separate() splits a single column into multiple columns
### Function: separate(data, col, into, sep = " ", remove = TRUE, convert = FALSE)
### Same as: data %>% separate(col, into, sep = " ", remove = TRUE, convert = FALSE)

separate_DF <- long_DF %>% separate(Quarter, c("Time_Interval", "Interval_ID"),sep = "\\.", remove = T)
head(separate_DF, 10)

## 3) unite() combines multiple columns into a single column
### Function: unite(data, col, ..., sep = " ", remove = TRUE)
### Same as: data %>% unite(col, ..., sep = " ", remove = TRUE)
unite_DF <- separate_DF %>% unite(Quarter, Time_Interval, Interval_ID, sep = ".")
head(unite_DF, 10)

## 4) spread() takes two columns (key & value) and spreads in to multiple columns, it makes “long” data wider
### Function: spread(data, key, value, fill = NA, convert = FALSE)
### Same as: data %>% spread(key, value, fill = NA, convert = FALSE)
wide_DF <- unite_DF %>% spread(Quarter, Revenue)
head(wide_DF, 10)

# 3. Subset Data
## Extract rows that meet logical criteria.
dplyr::filter(iris, Sepal.Length > 7)

## Remove duplicate rows.
dplyr::distinct(iris)

### Randomly select fraction of rows.
dplyr::sample_frac(iris, 0.5, replace = TRUE)

## Randomly select n rows.
dplyr::sample_n(iris, 10, replace = TRUE)

## Select rows by position.
dplyr::slice(iris, 10:15)

## Select columns by name or helper function
dplyr::select(iris, Sepal.Width, Petal.Length, Species)

# 4. Summarise Data
## Summarise data into single row of values.
dplyr::summarise(iris, avg = mean(Sepal.Length))
## Apply summary function to each column.
dplyr::summarise_each(iris, funs(mean))
## Count number of rows with each unique value of variable (with or without weights).
dplyr::count(iris, Species)

# 5. Make New Variables
## Compute and append one or more new columns.
dplyr::mutate(iris, sepal = Sepal.Length + Sepal.Width)
## Compute one or more new columns. Drop original columns
dplyr::transmute(iris, sepal = Sepal.Length + Sepal.Width)

