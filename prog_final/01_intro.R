# Chapter 1

# basic calculations -----------------------------------------------------------
2+5
3-4
5*6
7/8
2+3*2
(2+3)*2


4^2 ## 4²
sqrt(4) ## Square root
exp(1) ## Exponential function (Euler's number)
log(5) ## Natural logarithm
log(exp(5)) ## log and exp cancel each other out


# sequences --------------------------------------------------------------------
2:6
seq(2,11,3)


# creating the first object ----
x <- 4/2

x
# reusing objects ---------
y <- x * 5
y

# more than one entry -> vectors -------
x1 <- c(1,2,3)
x1
x1* 2


# length ----
length(x1)

# more object calc ------
y1 <- c(10,11,9)
y1
y1/x1


# error_test ---------
rm(x1)
x1

# comments ------
2+ 5 # this is a comment

2+ # a comment can also be placed here
  5

( 2 + # a 
    3) * # comment
  2 # across multiple lines



# Heading 1 ----

## Section 1.1 ----
3+2*4
3+2*3
## Section 1.2 ----
3+2*sqrt(3)

# Heading 2 ----
x <- c(2,6,8,2,35)
y <- seq(2,10,2)

y/x

