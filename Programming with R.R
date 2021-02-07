####################### Programming with R #####################
########### Conditionals ##################
a <- 4
if (a > 2) {
  print(1/a)
} else {
  print("No reciprocal for 2")
}

library(dslabs)
data("murders")

murder_rate <- murders$total/murders$population*100000
ind <- which.min(murder_rate)

if (murder_rate[ind] < 0.2) {
  print(murders$state[ind])
} else {print("Not found")
  }

#Function ifelse
a <-c(2,4,5,1,0,2)
ifelse(a>1,3/a,NA)

############### FUNCTIONS ###############
sum_squares <- function(x){
  sum(x^2)
}

sum_squares(c(1,2,3,4,5,6,7,8))

quadratic <- function(x){
  A <- 2 
  B <- 3
  C <- 10
  print(A*x^2 + B*x + C)
}

x <- seq(1,100)
plot(x)


polinomic <- function(x){
  A <- 2
  B <- -3
  C <- 4
  D <- 5
  print(A*x^3 + B*x^2 + C*x + D)
}

plot(polinomic(x))

################## LOOPS ##########################
compute_s_n <- function(n){
  x <- 1:n 
  sum(x)
}

m <- 25
s_n <- vector(length = m)
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

n <- 1:n
plot(n,s_n)


