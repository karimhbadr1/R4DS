# Chapter 16 - Vectors ----------------------------------------------------

library(tidyverse)

# Vector Basics -----------------------------------------------------------

typeof(letters)
typeof(1:10)
x<-list("a","b",1:10)
x
length(x)


# Important Types of Atomic Vector ----------------------------------------


# Logical -----------------------------------------------------------------

1:10%%3==0

c(TRUE,TRUE,FALSE,NA)


# Numeric -----------------------------------------------------------------

typeof(1)
typeof(1L)
1.5L

x<-sqrt(2)^2
x-2

c(-1,0,1)/0


# Character ---------------------------------------------------------------


x<-"This is a reasonably long string."
pryr::object_size(x)
y<-rep(x,1000)
pryr::object_size(y)


# Using Atomic Vectors ----------------------------------------------------

.Machine$double.eps
.Machine$integer.max
as.numeric(.Machine$integer.max) + 1


# Scalars and Recycling Rules ---------------------------------------------

sample(10) + 100
runif(10) > 0.5

1:10 + 1:2
1:10 + 1:3
tibble(x=1:4,y=1:2)
tibble(x = 1:4, y = rep(1:2, 2))
tibble(x = 1:4, y = rep(1:2, each = 2))


# Naming Vectors ----------------------------------------------------------

c(x=1,y=2,z=4)
a<-c(x=1,y=2,z=4)
typeof(a)
set_names(1:3, c("a", "b", "c"))


# Subsetting --------------------------------------------------------------

x <- c("one", "two", "three", "four", "five")
x[c(3,2,5)]
x[c(1, 1, 5, 5, 5, 2)]
x[c(-1, -3, -5)]
x[c(1, -1)]
x[c(0,-1)]
x[0]
x <- c(10, 3, NA, 5, 8, 1, NA)
x[c(1,3,5)]
x[!is.na(x)]
x[x %% 2 == 0]
x[!is.na(x)& x %% 2 == 0]
x <- c(abc = 1, def = 2, xyz = 5)
x
x[c("xyz","def")]
x[1]
x[]
x[[]]

x <- c(-Inf, -1, 0, 1, Inf, NA, NaN)
is.na(x)
mean(is.na(x))
!is.finite(x)
sum(!is.finite(x))
?is.vector()
x <- 1:10
attr(x, "something") <- TRUE
x
is.vector(x)
is.atomic(1:10)
x<-1:10
is.atomic(x)

last_value <- function(x) {
  # check for case with no length
  if (length(x)) {
    x[[length(x)]]
  } else {
    x
  }
}

last_value(numeric())
last_value(1)
last_value(1:10)
last_value(c(1:10,"abc"))
last_value(c(1:10,1:8))
length(c(1:10,1:8))

even_pos<-function(x){
  x[seq_along(x) %% 2 == 0]
}

seq_along(x) %% 2 == 0
even_pos(1)
even_pos(1:18)

even_pos(letters)

not_last<-function(x){
  x[-length(x)]
}

not_last(c(1,2,3))
not_last(c(1,2,3,TRUE,"abc"))
not_last(1)

x<-c(3,4,6,8,9)
x[x%%2==0]
x[seq_along(x)%%2==0]
seq_along(x)

x <- c(-1:1, Inf, -Inf, NaN, NA)
x
x[-which(x > 0)]
which(x>0)
x[-which(x>0)]

x<=0
x[NA]
x[1]
x[x<=0]

x[2]
x[12]
x[8]
x<-c(1,2,3)
x[4]


# Recursive Vectors (Lists) -----------------------------------------------

x<-list(1,2,3)
x
str(x)
x_named <- list(a = 1, b = 2, c = 3)
x_named
str(x)
y <- list("a", 1L, 1.5, c(TRUE,FALSE))
y
str(y)

z<-list(list(1,2),list(3,4))
z
str(z)

z<-list(list(list(1,2),3),list(4,5,8),list(list(1,2),list("x","y")))
str(z)
z


# Visualizing Lists -------------------------------------------------------

x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

x1

typeof(c(1,2))

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
a

a[1:2]
str(a[1:2])

a[4]
str(a[4])
a[[1]]
str(a[[1]])

a[[4]]
str(a[[4]])

a[[4]]
z<-a[[4]][1]
a[[4]][1][1]
a[[4]][[1]]

a$a
a["a"]
a[["a"]]

x <- tibble(a = 1:2, b = 3:4)
x
x[["a"]]
x["a"]
x[1]
x[1,]
x[,1]


# Attributes --------------------------------------------------------------

x<-1:10
attributes(x)

attr(x,"greetings")<-"Hi"
x
class(x)
typeof(x)
attr(x, "farewell") <- "Bye!"
attributes(x)


# Augmented Vectors -------------------------------------------------------


# Factors -----------------------------------------------------------------

x<-factor(c("ab","cd","ab"),levels=c("ab","cd","ef"))
typeof(x)
class(x)
attributes(x)


# Dates and Date-Times ----------------------------------------------------

x<-as.Date("1971-01-01")
x
unclass(x)
attributes(x)

x<-lubridate::ymd_hm("1971-01-01 1:00")
unclass(x)
typeof(x)
attributes(x)
attr(x,"tzone")<-"US/Pacific"
x
attr(x,"tzone")<-"US/Eastern"
x

y<-as.POSIXlt(x)
typeof(y)
attributes(y)


# Tibbles -----------------------------------------------------------------

tb<-tibble::tibble(x=1:5,y=5:1)
typeof(tb)
attributes(tb)
class(tb)

df<-data.frame(x=1:5,y=5:1)
typeof(df)
attributes(df)

typeof(hms::hms(3600))
class(hms::hms(3600))
attributes(hms::hms(3600))

t<-tibble(z=1:5,x=1)
t
t<-list(1:3)
b<-tibble(a=t,b=1:3)
b
View(b)

t
t<-tibble(z=2,x=1:5)
t

b<-tibble(a=t,c=1)
b
view(b)
alpha<-(tibble(x=1:3,y=list("a",1,list(1:3))))
