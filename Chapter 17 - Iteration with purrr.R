# Chapter 17 - Iteration with purrr ---------------------------------------


# Introduction ------------------------------------------------------------


# Prerequisites -----------------------------------------------------------

library(tidyverse)


# For loops ---------------------------------------------------------------

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

median(df$a)

output <- vector("double", ncol(df))
for (i in seq_along(df)){
  output[[i]]=median(df[[i]])
}

y <- vector("double", 0)
seq_along(y)
1:length(y)
# exercises ---------------------------------------------------------------

#1.1

view(mtcars)

output<-vector("double",ncol(mtcars))
for (i in seq_along(mtcars)){
  output[[i]]=mean(mtcars[[i]])
}
output

#1.2

output<-vector("character",ncol(nycflights13::flights))
for (i in seq_along(nycflights13::flights)){
  output[[i]]=typeof(nycflights13::flights[[i]])
}
output

#1.3

output<-vector("double",ncol(iris))
for (i in seq_along(iris)){
  output[[i]]=length(unique(iris[[i]]))
}
output

#1.4

means<-c(-10,0,10,100)
output<-vector(mode="list",length=length(means))
names(output)<-c("a","b","c","d")

for (i in seq_along(means)){
  output[[i]][[1]]=rnorm(10,mean=means[i])
  output[[i]][[2]]=mean(output[[i]][[1]])
  output[[i]][[3]]=hist(output[[i]][[1]])
}

#Exercise 2
#2.1

out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}

str_c(letters,sep="",collapse = "")
str_c(c(letters,letters),collapse="",sep="1")
str_c("Letter: ", letters)
str_c("Letter: ", letters)

#2.2

x <- sample(100)
sd2 <- 0
for (i in seq_along(x)) {
  sd2 <- sd2 + (x[i] - mean(x)) ^ 2
}
sd2 <- sqrt(sd2 / (length(x) - 1))

sd(x)

#2.3

x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}

cumsum(x)

#Exercise 3

humps<-c("five","four","three","two","two","one","no")

# for (i in seq_along(humps)){
#   if (humps[x]=="no") {
#     cat(str_c(rep(c("Alice the camel has",humps[x],"humps."),3),sep=" ", collapse = "\n"))
#     cat("Now Alice is a horse.")
#   } else {
#     cat(str_c(rep(c("Alice the camel has",humps[x],"humps."),3),sep=" ",collapse="\n"))
#     cat("So go, Alice, go.")
#   }
# }

# for (i in seq_along(humps)){
#   if (humps[x]=="no") {
#     cat(str_c(rep(c("Alice the camel has",humps[x],"humps."),3),sep=" ", collapse = "\n"))
#     cat("Now Alice is a horse.")
#   } else {
#     cat(str_c(rep(c("Alice the camel has",humps[x],"humps."),3),sep=" ",collapse="\n"))
#     cat("So go, Alice, go.")
#   }
# }

for (i in humps) {
  cat(str_c("Alice the camel has ", rep(i, 3), " humps.",
            collapse = "\n"
  ), "\n")
  if (i == "no") {
    cat("Now Alice is a horse.\n")
  } else {
    cat("So go, Alice, go.\n")
  }
  cat("\n")
}

#3.2

numbed<-c("ten","nine","eight","seven","six","five","four","three","two","one")

for (i in numbed){
 if (i=="one"){
    cat(str_c("There was one in a bed","\n"))
    cat(str_c("and the little one said"),"\n")
    cat(str_c("\"Good night!\""))
  } else { 
    cat(str_c("There were ",i, " in a bed"),"\n")
    cat(str_c("And the little one said"),"\n")
    cat(str_c("\"Roll over, roll over\""),"\n")
    cat(str_c("So they rolled over"),"\n")
    cat(str_c("And one fell out"),"\n")
  }
}

#3.3

bottles<-function(n){
  if (n>1) {
    str_c(n," bottles")
  }else if (n==1) {
    str_c("1 bottle")
  } else {
    str_c("No more bottles")
  }
}

beer_bottles<-function(total_bottles){
  for (current_bottles in seq(total_bottles,0)) {
    if (current_bottles>0){
      if (current_bottles==1){
        cat(str_c(bottles(current_bottles)," of beer on the wall, ", bottles(current_bottles), " of beer."),"\n")
        cat(str_c("Take one down and pass it around, ", "no more bottles", " of beer on the wall."),"\n")
        cat("\n")
      } else {
        cat(str_c(bottles(current_bottles)," of beer on the wall, ", bottles(current_bottles), " of beer."),"\n")
        cat(str_c("Take one down and pass it around, ", bottles(current_bottles-1), " of beer on the wall."),"\n")
        cat("\n")        
      }

    } else {
      cat(str_c(bottles(current_bottles)," of beer on the wall, ", "no more bottles of beer."),"\n")
      cat("Go to the store and buy some more, ", total_bottles ," bottles of beer on the wall...")
    }
  }
}

beer_bottles(3)



# For Loop Variations ----------------------------------------------------

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

df[1]
df[[1]]

for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}

means <- c(0, 1, 2)
output <- double()

for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}

str(output)

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)

str(unlist(out))

flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips

output1<-vector("integer")
for (i in 1:10000000) {
  output1[[i]]<-flip()
}

table(output1)

#Exercise 1

files <- dir("K://LENT//Research Department//CfS//Spring//Schools Output//DUMMY", pattern = "\\.xlsx$", full.names = TRUE)
df_list <- vector("list", length(files))
for (i in seq_along(files)){
  df_list[[i]]<-readxl::read_xlsx(files[[i]])
}

df<-bind_rows(df_list)

#Exercise 2

x <- c(11, 12, 13)
print(names(x))

for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}

x <- c(a = 11, 12, c = 13)
names(x)

# for (nm in names(x)) {
#   print(nm)
#   print(x[[nm]])
# }

x <- c(a = 11, a = 12, c = 13)

for (z in names(x)) {
  print(z)
  print(x[[z]])
}

#Exercise 3

show_mean<-function(df){
  maxstr<-max(str_length(names(df)))
  for (z in names(df)){
    if (typeof(df[[z]])=="double"){
      cat(str_pad(str_c(z,":"),maxstr+1L,side="right"),
          format(mean(df[[z]],na.rm=TRUE),digits=2,nsmall=2))
      cat("\n")
    }
  }
}
  
show_mean(iris)

print(c("1","abc"))

#Exercise 4

trans <- list( 
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)

for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}

names(trans)

mtcars[["disp"]]

trans[["disp"]](mtcars$disp)



# For loops vs functionals ------------------------------------------------

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output<-vector("double",length = length(df))
for (i in seq_along(df)){
  output[[i]]<-mean(df[[i]])
}
output


col_mean<-function(df){
  output<-vector("double",length = length(df))
  for (i in seq_along(df)){
    output[[i]]<-mean(df[[i]])
  }
  output
}

col_mean(iris)
col_mean(df)

col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- median(df[[i]])
  }
  output
}
col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- sd(df[[i]])
  }
  output
}

col_mean(df)
col_median(df)
col_sd(df)


col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}

col_summary(df,range)

x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
apply(x, 2, mean, trim = .2)
apply(x,1,mean)
apply(x,2,mean)
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)

rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))
stopifnot( apply(x, 2, is.vector))
apply(x, 2, is.vector)

col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    if (typeof(df[[i]])== "double"){
     out[i] <- fun(df[[i]]) 
    } else {
      out[i]<-"NA"
    }
  }
  out
}

col_summary(iris,fun=mean)

df <- tibble(
  X1 = c(1, 2, 3),
  X2 = c("A", "B", "C"),
  X3 = c(0, -1, 5),
  X4 = c(TRUE, FALSE, TRUE)
)
df

col_summary(df,fun=mean)

X <- matrix(rnorm(15), nrow = 5)
X
apply(X, 1, mean)
X_row_means <- vector("numeric", length = nrow(X))
for (i in seq_len(nrow(X))) {
  X_row_means[[i]] <- mean(X[i, ])
}
X_row_means

apply(X, 2, mean)

X_col_means <- vector("numeric", length = ncol(X))
for (i in seq_len(ncol(X))) {
  X_col_means[[i]] <- mean(X[, i])
}
X_col_means

col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    if (is.numeric(df[[i]])==TRUE){
      out[i] <- fun(df[[i]]) 
    } else {
      out[i]<-"NA"
    }
  }
  out
}

col_summary(iris,fun=mean)
col_summary(df,fun=mean)

col_summary2 <- function(df, fun) {
  numeric_cols <- vector("logical", length(df))
  for (i in seq_along(df)) {
    numeric_cols[[i]] <- is.numeric(df[[i]])
  }
  idxs <- which(numeric_cols)
  n <- sum(numeric_cols)
  out <- vector("double", n)
  for (i in seq_along(idxs)) {
    out[[i]] <- fun(df[[idxs[[i]]]])
  }
  names(out) <- names(df)[idxs]
  out
}
col_summary2(df,mean)
col_summary2(iris,mean)


# The Map Functions -------------------------------------------------------
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

df %>% map_dbl(mean)
map_dbl(df, mean, trim = 0.9)

z <- list(x = 1:3, y = 4:5)
map(z,length)
map_int(z, length)

models <- mtcars %>% 
  split(.$cyl)

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))


a<-map(split(mtcars,mtcars$cyl),function(df){
  lm(mpg~wt,data=df)
})



models<-mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg~wt,data=.))

summary(models)

map(models,summary) %>% map(~.$r.squared)

models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)

mtcars %>% 
  split(.$cyl)

models$`4`

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

models %>% summary() 

models %>% map(summary) %>% map_dbl()

mtcars %>% split(.$cyl)

models %>% map(summary) %>% map_dbl(~.$r.squared)

models %>% map(summary) %>% map_dbl("r.squared")

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x


apply(b,2,summary)
lapply(x1,summary)
sapply(x1,summary)

dim(b)

map_dbl(x,2)

x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]

x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()

x1 %>% lapply(threshold) %>% str()
x2 %>% lapply(threshold) %>% str()


# Exercises ---------------------------------------------------------------

#1

#a
a<-map(mtcars,mean)
a<-unlist(a)
map_dbl(mtcars,mean)

#b

map_chr(nycflights13::flights,typeof)

#c

map_int(iris,n_distinct)
map_dbl(iris,n_distinct)
map_dbl(iris,mean)

map_int(iris,function(x) length(unique(x)))
map_int(iris,~length(unique(.)))
#d

mu<-c(-10,0,10,100)

map(mu,~rnorm(n=10,mean = .))

#2

map(diamonds,is.factor) %>% str()
map_lgl(diamonds,is.factor)

#3

map(1:5,runif)


#4

map(-2:2,runif,n=5)

c(-2:2)

runif(2,n=5)

runif(-2,n=4)

runif(-20,n=2)

x<-split(mtcars,mtcars$cyl)
map(x, ~lm(mpg ~ wt, data = .))


#Exercises - Take 2
#
#1
#a
#
map(mtcars,mean)
map_dbl(mtcars,mean)
apply(mtcars,2,mean)
sapply(mtcars,mean)
unlist(map(mtcars,mean))
a<-as.data.frame(map(mtcars,mean))
b<-as.matrix(a)

#b
#
apply(nycflights13::flights,2,typeof)
map(nycflights13::flights,typeof)
a<-as_tibble(map_chr(nycflights13::flights,typeof),rownames="variables")

#c
#
map_dbl(iris,~length(unique(.)))
unique(iris$Sepal.Length)
length(unique(iris$Sepal.Length))

#d
#

mu<-c(-10,0,10,100)

map(mu,~rnorm(10,mean=.))

#2
#

map_lgl(mtcars,is.factor)

#3
#
class(1:5)

map(1:5,runif)

#4
#
map(-2:2, rnorm, n = 5)

map_dbl(-2:2, rnorm, n = 5)


map(-2:2,rnorm,n=5) %>% unlist()

map(-2:2,rnorm,n=5) %>% flatten_dbl()

#5
#

map(x, function(df) lm(mpg ~ wt, data = df))

x<-split(mtcars,mtcars$cyl)
map(x,~lm(mpg ~ wt, data=.))



# Dealing with Failure ----------------------------------------------------

try(1+1)
try(1/0)
try(0/-)
?try
try(log(a))
log(a)


safe_log<-safely(log)

safe_log(10)

safe_log(a)

x<-list(1,10,"a")

y<-x %>% map(safely(log))

str(y)

y<-transpose(y)

is_ok<-y$error %>% map_lgl(is_null)

x[!is_ok]

y$result[is_ok] %>% flatten_dbl()

x<-list(1,10,"a")

x %>% map_dbl(possibly(log,NA_real_))

z<-list(1,-1)
zx<-z %>% map(quietly(log))

transpose(zx)

# Mapping over multiple arguments -----------------------------------------

mu<-list(5,10,-3)

mu %>% 
  map(rnorm,n=5)

sigma<-list(1,5,10)

seq_along(mu) %>% 
  map(~rnorm(5,mu[[.]],sigma[[.]]))

map(seq_along(mu),~rnorm(5,mu[[.]],sigma[[.]]))

map(mu,rnorm,n=5)

?rnorm

rnorm(2,7,1)

mean(c(8.009322,6.524731))
sd(c(8.009322,6.524731))

map(seq_along(mu),~rnorm(5,mu[[.]],sigma[[.]]))

map2(mu,sigma,rnorm,n=5)

mu<-list(5,10,3)
sigma<-list(1,5,10)
n<-list(1,3,5)

args1<-list(mean=mu,sd=sigma,n=n)

pmap(args1,rnorm)

rnorm(n,mean,sd)

params<-tibble(args1)

rbind(unlist(args1))

a<-seq_along(args1)
transpose(args1)


cbind(args1)

b<-unlist(args1)

params<-tribble(
  ~mean,~sd,~n,
  5,1,1,
  10,5,3,
  -3,10,5
)

params

pmap(params,rnorm)

unlist(args1[1])

map(seq_along(args1),~unlist(args1[[.]]))


# Invoking different functions --------------------------------------------

f<-c("runif","rnorm","rpois")
param<-list(
  list(min=-1,max=1),
  list(sd=5),
  list(lambda=10)
)

invoke_map(f,param,n=5)

sim<-tribble(
  ~f,~params,
  "runif",list(min=-1,max=1),
  "rnorm",list(sd=5),
  "rpois",list(lambda=10)
)

view(sim)

sim<-mutate(sim,sim=invoke_map(f,params,n=5))

sim$sim[[1]]

a<-reduce(args1,cbind)

b<-as_tibble(a)

colnames(b)<-names(args1)

view(b)

# Walk --------------------------------------------------------------------

x<-list(1,"a",3)

walk(x,print)

library(ggplot2)

plots<-split(mtcars,mtcars$cyl) %>% 
  map(~ggplot(.,aes(mpg,wt))+geom_point())

paths<-str_c(names(plots),".pdf")

pwalk(list(plot=plots,filename=paths),ggsave,path=tempdir())

tempdir()



# Other patterns of for loop ----------------------------------------------



# Predicate functions -----------------------------------------------------

keep(iris,is.factor)

iris %>% discard(is.factor) %>% str()

s<-list(1:5,letters,list(10))

s

some(s,is.character)
every(s,is.character)
every(s,is_vector)

x<-sample(10)
x

detect(x,~.>5)
detect_index(x,~.>5)

head_while(x,~.>2)
tail_while(x,~.>2)


# Reduce and accumulate ---------------------------------------------------

dfs<-list(
  age=tibble(name="John",age=30),
  sex=tibble(name=c("John","Mary"),sex=c("M","F")),
  trt=tibble(name="Mary",treatment="A")
)

dfs

reduce(dfs,full_join)

vs<-list(
  c(1,3,5,6,10),
  c(1,2,3,7,8,10),
  c(1,2,3,48,9,10)
)

vs

reduce(vs,intersect)

x<-sample(10)
x

accumulate(x,`-`)


# Exercises ---------------------------------------------------------------

##1
s1<-list(1:5,c(letters,3),list(10))
s1
some(s,is.character)
every(s,is.character)

?every

every4loop<-function(x,check){
n=0
for (i in (1:length(x))){
 if(isTRUE(check(x[i]))){
  n=n+1 
 }else{
   n=n
 }
  }
if (n==length(x)){
  print(TRUE)
}else{
  print(FALSE)
}
}

every4loop(7:3,function(x){x>1})

b<-function(x){x>1}

b(2)


for (i in (1:length(a))){
  if((b(a[i]))){
    n=n+1 
  }else{
    n=n
  }
}


##2

a<-tibble(x=c(1,2,3),y=c("a","b","c"),z=c(8,19,1))

a1<-keep(a,is.numeric)

col_sum<-function(df,fun){
  df1<-keep(df,is.numeric)
  map(df1,fun)
}

col_sum(a,mean)


##3

col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  
  sapply(df_num, f)
}

col_sum3(a,sum)

df <- tibble(
  x = 1:3, 
  y = 3:1,
  z = c("a", "b", "c")
)

col_sum(df,sum)
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0],mean)
