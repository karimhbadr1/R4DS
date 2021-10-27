# Chapter 14: Pipes with magrittr -----------------------------------------


# Introduction ------------------------------------------------------------


# Prerequisites -----------------------------------------------------------

library(magrittr)


# Piping Alternatives -----------------------------------------------------

#foo_foo<-little_bunny


# Intermediate Steps ------------------------------------------------------

diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>% 
  dplyr::mutate(price_per_carat = price / carat)
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)
diamonds$carat[1] <- NA
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)


# Overwrite the Original --------------------------------------------------


# Function Composition ----------------------------------------------------


# Use the Pipe ------------------------------------------------------------

assign("x",10)
"x" %>% assign(100)
env<-environment()
"x" %>% assign(100,envir = env)

tryCatch(stop("!"), error = function(e) "An error")
stop("!") %>% 
  tryCatch(error = function(e) "An error")


# When Not to Use the Pipe ------------------------------------------------

rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()

rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()

mtcars %$%
  cor(disp, mpg)

mtcars %>% cor(disp,mpg)

mtcars <- mtcars %>% 
  transform(cyl = cyl * 2)

mtcars %<>% transform(cyl = cyl * 2)
