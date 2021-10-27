# Chapter 15 - Functions --------------------------------------------------

library(tidyverse)
# Introduction ------------------------------------------------------------


# Prerequisites -----------------------------------------------------------


# When Should You Write a Function? ---------------------------------------

df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

x <- df$a
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE,finite=TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01(c(0,5,10))
rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)
x <- c(1:10, Inf)
rescale01(x)




# Exercises ---------------------------------------------------------------

#1

rescale01 <- function(x) {
  rng <- range(x, na.rm = FALSE)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01(c(1:5,NA))

#2

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE,finite=TRUE)
  y<-(x - rng[1]) / (rng[2] - rng[1])
  y[y==-Inf]<-0
  y[y==Inf]<-1
  y
}

x <- c(1:10, Inf,-Inf)
rescale01(x)

#3
x<-c(1:4,NA)
mean(is.na(x))

prop_na<-function(x){
  mean(is.na(x))
}
prop_na(x)

x/sum(x,na.rm=TRUE)

sum_to_one<-function(x){
  x/sum(x,na.rm=TRUE)
}

sum_to_one(c(1,2,3,4))
sum_to_one(c(1,2,3,4,NA))
sum_to_one(c(1,2,3,4,Inf))

COV<-function(x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}
COV(x)
COV(c(1,2,3,4,NA))

sum_to_one_1 <- function(x, na.rm = FALSE) {
  x / sum(x, na.rm = na.rm)
}

sum_to_one_1(x,na.rm = TRUE)

COV_1<-function(x,na.rm=FALSE){
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}
COV_1(c(1,2,3,4))
COV_1(c(1,2,3,4,NA))
COV_1(c(1,2,3,4,NA),na.rm=TRUE)
COV_1(na.rm=TRUE,c(1,2,3,4,NA))

variance<-function(x,na.rm=TRUE){
  n<-length(x)
  mean<-mean(x,na.rm=TRUE)
  sq_error<-sum((x-mean)^2)
  (1/(n-1))*(sq_error)
}

variance(1:10)
var(1:10)

variance_1 <- function(x,na.rm=TRUE) {
  n <- length(x)
  m <- mean(x, na.rm = TRUE)
  sq_err <- (x - m)^2
  sum(sq_err) / (n - 1)
}
variance_1(c(1:10,NA))


x<-c(1,2,5,100)
skewness<-function(x){
  n<-length(x)
  m<-mean(x,na.rm=TRUE)
  v<-var(x,na.rm=TRUE)
  (sum((x-m)^3)/(n-2))/(v^(3/2))
}
skewness(x)
install.packages("e1071")
library(e1071)
e1071::skewness(x,type = 3)
e1071::skewness(x,type = 2)
e1071::skewness(x,type = 1)

both_na<-function(x,y){
  sum(is.na(x)& is.na(y))
}

x1<-c(1,2,3,NA,NA)
y1<-c(1,5,NA,NA,NA)

both_na(x1,y1)

is_directory <- function(x) 
file.info("C:\\\\Users\\\\badrk\\\\Documents\\\\Outlook Files")$isdir
file.access("C:\\\\Users\\\\badrk\\\\Documents\\\\Outlook Files")


# Functions Are for Human and Computers -----------------------------------


# Exercises ---------------------------------------------------------------

#1
f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

f2 <- function(x) {
  if (length(x) <= 1) {
    return(NULL)
  }
  x[-length(x)]
}

f3 <- function(x, y) {
  rep(y, length.out = length(x))
}
#2
#3
a<-rnorm(10)
mean(a)
sd(a)
?rnorm

#MASS::mvrnorm()


# Conditional Execution ---------------------------------------------------

# Conditions --------------------------------------------------------------

identical(0L, 0)
x <- sqrt(2) ^ 2
x
x==2
near(x,2)
x-2


# Multiple Conditions -----------------------------------------------------

a <- function(x, y, op) {
  switch(op,
    plus = x + y,
    minus = x - y,
    times = x * y,
    divide = x / y,
    stop("Unknown op!")
  )
}
a(1,2,"plus")
a(1,2,"times")
a(1,2,"lol")


# Code Style --------------------------------------------------------------

y <- 100
x <- if (y < 20) "Too low" else "Too high"

if (y < 20) {
  x <- "Too low" 
} else {
  x <- "Too high"
}


# Exercises ---------------------------------------------------------------

greet<-function(time=lubridate::now()){
  hr<-lubridate::hour(time)
  if (hr<12){
    print("good morning")
  } else if (hr<17) {
    print("good afternoon")
  } else {
    print("good evening")
  }
}

greet()
greet(lubridate::ydm_h("2021-04-05:09"))
greet(lubridate::ydm_h("2021-04-05:02"))
greet(lubridate::ydm_h("2021-04-05:12"))
greet(lubridate::ydm_h("2021-04-05:16"))
greet(lubridate::ydm_h("2021-04-05:17"))
greet(lubridate::ydm_h("2021-04-05:19"))
greet(lubridate::ydm_h("2021-04-05:23"))
greet(lubridate::ydm_h("2021-04-05:24"))
greet(lubridate::ydm_h("2021-04-05:01"))

3%%3
4%%3
5%%3
5%%5
15%%3
15%%5

fizzbuzz<-function(x){
  stopifnot(length(x) == 1)
  stopifnot(is.numeric(x))
  if (x%%3==0 && x%%5!=0){
    print("fizz")
  } else if (x%%3!=0 && x%%5==0){
    print("buzz")
  } else if (x%%3==0 && x%%5==0){
    print("fizzbuzz")
  } else {
    print(x)
  }
}

fizzbuzz(2)
fizzbuzz(3)
fizzbuzz(6)
fizzbuzz(5)
fizzbuzz(10)
fizzbuzz(15)
#fizzbuzz("two")
#fizzbuzz(two)

?cut

a<-function(temp) {
  if (temp <= 0) {
    "freezing"
  } else if (temp <= 10) {
    "cold"
  } else if (temp <= 20) {
    "cool"
  } else if (temp <= 30) {
    "warm"
  } else {
    "hot"
  }
}

b<-function(temp){
  cut(temp,breaks=c(-Inf,0,10,20,30,Inf),labels=c("freezing","cold","cool","warm","hot"))  
}
cut(b,breaks=c(-Inf,0,10,20,30,Inf),labels=c("freezing","cold","cool","warm","hot"))

b<-function(temp){
  cut(temp,breaks=c(-Inf,0,10,20,30,Inf),labels=c("freezing","cold","cool","warm","hot"),right=FALSE)  
}

k<-function(x){
  switch(x, 
         a = ,
         b = "123",
         c = ,
         d = "456"
  ) 
}

switch(1, "apple", "banana", "cantaloupe")
switch(1.8,"apple","banana","cantaloupe")



# Function Arguments ------------------------------------------------------

mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

mean_ci(c(100,200))

x <- runif(100)

mean_ci(x)
mean_ci(x, conf = 0.99)

wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}

wt_mean(c(1,2,3),c(1,2,3))

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = TRUE)
  }
  sum(w * x) / sum(w)
}

wt_mean(1:6, 1:3)

wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 6:1, na.rm = "foo")

commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

commas(letters[1:20])

letters[1:30]
pi
pi[1:100]
letters

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}

rule("a")
rule("a","b")

paste0("a","b")

getOption("width")

x <- c(1, 2)
sum(x, na.mr = TRUE)
sum(x)

commas(letters, collapse = "-")

rule("Title", pad = "-+")

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  padding <- str_dup(
    pad,
    ceiling(width / str_length(title))
  ) %>%
    str_trunc(width)
  cat(title, " ", padding, "\n", sep = "")
}

library(tidyverse)

str_dup("-",ceiling((getOption("width") - nchar("abc") - 5) / str_length("abc"))) %>% str_trunc(((getOption("width") - nchar("abc") - 5) / str_length("abc")))

?mean

x<-c(1,2,3,4,5,6,7,8,9,11)
mean(x)
mean(x,trim=0.1)
mean(c(2,3,4,5,6,7,8,9))
?cor()

return(0)

complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  
  # Complicated code here
}
x<-c()
complicated_function(x)

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}

show_missings(mtcars)

show_missings_1 <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
}

show_missings_1(mtcars)

x <- show_missings(mtcars) 
class(x)
y<-show_missings_1(mtcars)
class(y)

mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings() 
#> Missing values: 0
#> Missing values: 18
#

f <- function(x) {
x + y
} 

y<-10
f(100)


`+` <- function(x, y) {
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}

`+`(1,2)

table(replicate(10,1+2))
rm(`+`)
