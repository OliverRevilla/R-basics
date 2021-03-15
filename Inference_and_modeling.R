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
#function: t.test(): mean and difference of mean
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

# Exercises

#Assume there are only two candidates and construct 
#a 95% confidence interval for the election night proportion .

# Load the data
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- polls_us_election_2016 %>% filter(state == "U.S." & enddate >="2016-10-31")

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls[1,"samplesize"]
print(N)

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- as.numeric(polls[1,] %>% select(rawpoll_clinton))/100
print(X_hat)
# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
print(se_hat)
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(X_hat - qnorm(0.975) *se_hat, X_hat + qnorm(0.975) *se_hat)

#Create a new object called pollster_results that contains 
#the pollster's name, the end date of the poll, the 
#proportion of voters who declared a vote for Clinton, 
#the standard error of this estimate, and the lower and 
#upper bounds of the confidence interval for the estimate

pollster_results <- polls %>% 
  mutate(X_hat = rawpoll_clinton/100,
  se_hat =sqrt(X_hat*(1- X_hat)/samplesize), 
  lower = X_hat - qnorm(0.975)*se_hat, 
  upper = X_hat + qnorm(0.975)*se_hat)

view(pollster_results)                                    

pollster_results <- pollster_results %>% mutate(hit = 0.482 >= lower & 0.482 <= upper)                              
                                     
avg_hit <- pollster_results %>% 
  mutate(hit = lower<=0.482 & upper>=0.482) %>% 
  summarize(mean(hit))


#
# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>% mutate(d_hat = rawpoll_clinton - rawpoll_trump)


# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls[1,"samplesize"]
print(N)

# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls[1,"d_hat"]/100
print(d_hat)

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- ((d_hat+1)/2)

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2*sqrt((X_hat*(1-X_hat))/N)


# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c((2*X_hat-1) - qnorm(0.975) *se_hat, (2*X_hat-1) + qnorm(0.975) * se_hat)


#

pollster_results <- polls %>% 
  mutate(X_hat = ((d_hat/100)+1)/2,
         se_hat =2*sqrt((X_hat*(1-X_hat))/samplesize), 
         lower = 2*X_hat-1 - qnorm(0.975)*se_hat, 
         upper = 2*X_hat-1 + qnorm(0.975)*se_hat) %>%
  select(pollster, enddate,d_hat, X_hat,se_hat,lower, upper)


pollster_results <-  polls %>% 
  mutate(X_hat = (d_hat+1)/2, 
         se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
         lower = d_hat - qnorm(0.975)*se_hat,
         upper = d_hat + qnorm(0.975)*se_hat) %>% 
  select(pollster, enddate, d_hat, lower, upper)

#
#calculate the difference between each poll's estimate 
#and the actual . 

polls %>% mutate(errors = d_hat/100 - 0.021) %>%
  ggplot(aes(x = d_hat, y = errors)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#


polls %>% mutate(errors = d_hat/100 - 0.021) %>%
  group_by(pollster) %>% filter(pollster >= 5) %>%
  ggplot(aes(x = d_hat, y = errors)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### Poll aggregators

library(tidyverse)
library(dslabs)

d <- 0.039
Ns <- c(1298,533,1342,897,774,254,812,324,1291,1056,2172,516)
p <- (d+1)/2

confidence_intervals <- sapply(Ns, function(N){
  x <-sample(c(0,1), size = N, replace = TRUE,prob = c(1-p,p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat*(1-x_hat)/N)
  2*c(x_hat, x_hat - 2*se_hat,x_hat + 2*se_hat) - 1
})


# dataframe
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals),
                    sample_size = Ns)
names(polls) <- c('poll','estimate','low','high','sample_size')

sum(polls$sample_size)

d_hat <- polls %>% summarise(avg = sum(estimate*sample_size)/sum(sample_size)) %>%
         pull(avg)

#estimation for proportion
p_hat <- (1 + d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
moe

#16.1.1. Poll data

data("polls_us_election_2016")
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= '2016-10-31' &
           (grade %in% c("A+","A","A-","B+")|is.na(grade)))
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

d_hat <- polls %>%
  summarize(d_hat = sum(spread*samplesize)/sum(samplesize)) %>%
  pull(d_hat)

p_hat <- (d_hat + 1)/2
noe <- 1.96*2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
noe

polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color = "black", binwidth = 0.01)


# Code: Investigating poll data and pollster bias
# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())

# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

#Data Driven Models
# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)

##### Exercises ###
# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Calculate the population average. Print this value to the console.
mean(x)

# Calculate the population standard deviation. Print this value to the console.
sd(x)

#2.-
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x,size = N, replace = TRUE)

# Calculate the sample average. Print this value to the console.
print(mean(X))

# Calculate the sample standard deviation. Print this value to the console.
print(sd(X))

#3.-
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Define `se` as the standard error of the estimate. Print this value to the console.
se <- sd(X)/sqrt(N)
print(se)


# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(mean(X)-qnorm(0.975)*se, mean(X)+qnorm(0.975)*se)

#5.-
# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu



res <- replicate(B,{
  X <- sample(x, N , replace = TRUE)
  se <- sd(X)/sqrt(N)
  interval <- c(mean(X)-qnorm(0.975)*se, mean(X)+qnorm(0.975)*se)
  between(mu, interval[1],interval[2])
})

#6.-
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

names(polls)

polls %>% ggplot(aes(pollster, spread)) + geom_boxplot() + geom_point()

#13.-
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma <- polls %>% group_by(pollster) %>% summarize(s = sd(spread))


# Print the contents of sigma to the console
print(sigma)

#15.-
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <-polls %>% group_by(pollster) %>% summarise(avg = mean(spread), sd = sd(spread),n())


# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <- abs(res[1,'avg']-res[2,'avg'])
print(estimate)

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt((res[1,'sd']^2)/res[1,'n()'] + (res[2,'sd']^2)/res[2,'n()'])

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(estimate - qnorm(0.975)*se_hat,estimate + qnorm(0.975)*se_hat  )

#16.-
# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 

# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# Calculate the p-value
2*(1 - pnorm(estimate/se_hat, 0, 1))

#17.-
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

names(polls)

polls$pollster



