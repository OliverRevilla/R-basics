# Function: dbinom: binomial density 
#Function: pbinom: binomial cumulative distribution function <= 0


#Random variables
beads <- rep(c("red","blue"), times = c(2,3))
x <- ifelse(sample(beads,1) == "blue", 1, 0)

#Sampling Models
color <- rep(c("Black","Red","Green"), c(18,18,2))

n <- 1000
X <- sample(ifelse(color == "Red", -1,1), n, replace = TRUE)

X <- sample(c(-1,1),n,replace= TRUE, prob = c(9/19,10/19))
X
sum(X)

# The probability distribution of a random variable

n <- 1000
B <- 10000
roulette_winnings <- function(n){
  X <- sample(c(-1,1),n,replace = TRUE,prob = c(9/19,10/19))
  sum(X)
}

S <- replicate(B, roulette_winnings(n))

qplot(S, geom = hist(S)) 

# pbinom and dbinom
n <- 1000
pbinom(n/2,size = n, prob = 10/19) # P(S <= 0)

pbinom(n/2-1, size = n, prob = 10/19) # P(S < 0)




