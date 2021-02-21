#Polls
library(tidyverse)
library(dslabs)
take_poll(25)




#Central Limit Theorem in practice

x_hat <- 0.48 
se <- sqrt(x_hat*(1-x_hat)/25)
se

pnorm(0.01/se)-pnorm(-0.01/se)

pnorm(1.96)-pnorm(-1.96)

#Montecarlo simulation with CTL.

B <- 10000
N <- 1000
p <- 0.48
x_hat <- replicate(B,{
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
  mean(x)
})
x_hat


p <- 0.45
N <-1000
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
x_hat <- mean(x)


B <- 10000
N <- 1000
p <- 0.45
x_hat <- replicate(B,{
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
  mean(x)
})
x_hat

mean(x_hat)
sd(x_hat)

#Histogram and qqplot to answer the questions about p
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(dplyr)
p1 <- data.frame(x_hat = x_hat) %>% ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat)%>% ggplot(aes(sample = x_hat))+
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat)))+
  geom_abline()+
  ylab("x_hat")+
  xlab("Theoretical normal")

grid.arrange(p1,p2,nrow = 1)

#Bias
N <- 100000
p <- seq(0.35,0.65, length = 100)
SE <- sapply(p,function(x)2*sqrt(x*(1-x)/N)) 
data.frame(p = p, SE = SE) %>% ggplot(aes(p, SE)) +
  geom_line()


#Exercises
# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.
take_sample <- function(p,N){
  sample <- sample(c(0,1),size = N, replace = TRUE, prob = c(1-p, p))
  mean(sample)
}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p,N)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B,{
p - take_sample(p,N)
})

# Calculate the mean of the errors. Print this value to the console.
mean(errors)
hist(x = errors, nclass = 10)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the mean of the absolute value of each simulated error. Print this value to the console.
mean(abs(errors))

#4.-
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the standard deviation of `errors`
errors^2
mean(errors^2)
sqrt(mean(errors^2))

#5-- Standard Error
# Define `p` as the expected value equal to 0.45
p <- 0.45

# Define `N` as the sample size
N <- 100

# Calculate the standard error
sqrt(p*(1-p)/N)

#6.-
# Define `p` as a proportion of Democratic voters to simulate
p <- 0.45

# Define `N` as the sample size
N <- 100

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X <- sample(c(0,1), size = N, prob = c(1-p,p), replace = TRUE)

# Define `X_bar` as the average sampled proportion
X_bar <- mean(X)

# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar*(1-X_bar)/N)

#8.-
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N,se)

#11.- 
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Generate a qq-plot of `errors` with a qq-line showing a normal distribution
qqnorm(errors)
qqline(errors)

#12.-
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
pnorm(0.5, mean = p, sd = sqrt(((1-p)*p)/N), lower.tail = FALSE)

#13.-
# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)

# Calculate the probability that the error is 0.01 or larger
1-(pnorm(0.01/se_hat) - pnorm(-0.01/se_hat))

#########################################################

############## CONFIDENCE INTERVALS ###################
library(dslabs)
library(ggplot2)
library(tidyverse)

data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Tmperatures in New Haven")

  
#We want to know the probability that the interval [¯X − 2 ˆ SE(¯X ), ¯X − 2 ˆ SE(¯X )] contains
#the true proportion p.
 
p <- 0.45
N <- 1000

#Interval
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
x_hat <- mean(x)
se_hat <- sqrt(x_hat * (1- x_hat)/N)
c(x_hat - 1.96 *se_hat,x_hat + 1.96 *se_hat )
 
# P(−1.96 ≤ Z ≤ 1.96)

pnorm(1.96) - pnorm(-1.96)

# Ir we want to have a larger probability, maybe 99.5%

qnorm(0.995)

B <- 10000

inside <- replicate (B, {
  x <- sample(c(0,1),size = N, replace = TRUE, prob = c(1-p,p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1- x_hat)/N)
  between(p, x_hat - 1.96* se_hat,x_hat + 1.96* se_hat)
  })

mean(inside)






