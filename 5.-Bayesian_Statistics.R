# Bayesian Statistics

#In the urn model, it does not make sense to talk about the probability 
#of p being greater a certain value p is a fixed value.
#With Bayesian statistics we assume that p is in fact random, which allow
#us to calculate probabilities related to p.
#Hierarchical models describe variability at different levels and incorporate
#all these levels into a model for estimating p

#Bayes theorem simulation

prev <- 0.0025 #P(D = 1)
N <- 100000 # Number of people
outcome <- sample(c("Disease","Healthy"),N, replace = TRUE, prob = c(prev,1-prev))
N_D <- sum(outcome == 'Disease')
N_D
P_N_D <- N_D/N
P_N_D
N_H <- sum(outcome == 'Healthy')
N_H
P_N_H <- N_H/N
P_N_H

#Hierarchical Models
#Hierarchical models use multiple levels of variability to model results. They are
#hierarchical because values in the lower levels of the model are computed using values
#from higher levels of the model

#We model baseball player batting average using a hierarchical model with two levels of variability

#p ~ N(u,r) ---- describes player to player variability in natural ability
#Y|p ~ N(p, o) --- describes the varability due the luck

#First level _ prior distribution
#Second level _ sampling distribution

#Exercise 1
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

polls <- polls_us_election_2016%>%filter(state == 'Florida' & enddate >= "2016-11-04") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

head(polls, 200)


results <- polls %>% summarise(avg = mean(spread), se = sd(spread/sqrt(11)))
results


#exercises
mu <- 0
sigma <- results$se
Y <- results$avg

#Define a variable 'taus' as different values of tau
taus <- seq(0.005,0.05,len = 100)

#Create a function called 'p_calc' that generates 'B' 
#and calculates the probability of the spread being less than 0

p_calc <- function(x){
  B <- (sigma^2)/(sigma^2 + x^2)
  posterior_mean <- B*mu +(1-B)*Y
  posterior_se <- sqrt(1/(1/sigma^2 + 1/taus^2))
  prob <- pnorm(0,posterior_mean, posterior_se)
  prob
}

#Create a vector called 'ps' by applying the function 'p_calc' across
#value in 'taus'

ps <- p_calc(taus)
plot(taus, ps)

### Election Forecasting
library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
    filter(state == "U.S." & enddate >= "2016-10-31" &
                 (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
    filter(enddate == max(enddate)) %>%
    ungroup()

results <- one_poll_per_pollster %>%
    summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
    mutate(start = avg - 1.96*se, end = avg + 1.96*se)

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se)

#Mathematical Representations of Models

set.seed(3)
J <- 6
N <- 2000
d <- .021
p <- (d + 1)/2
X <- d + rnorm(J,0,2*sqrt(p*(1-p)/N))

#Definition of results object
library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

#Computing the posterior mean, standard error, credible interval and probability
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se)

#Simulated data with  Xj=d+ϵj
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))

#Simulated data with  Xi,j=d+ϵi,j
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

#Simulated data with  Xi,j=d+hi+ϵi,j
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

#Calculating probability of  d>0  with general bias
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

#Top 5 states ranked by electoral votes
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)

results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

#Computing the average and standard deviation for each state
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", "state") &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

#Monte Carlo simulation of Election Night results (no general bias)
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clintonEV) %>%
  ggplot(aes(clintonEV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

#Monte Carlo simulation including general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

#Variability across one pollster
# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")

#Trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

#Plotting raw percentages across time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))

#Exercise 1 - Confidence Intervals of Polling Data
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
cis <- polls %>% mutate(X_hat = (spread + 1)/2, 
                        se = 2*sqrt(X_hat*(1-X_hat)/samplesize),
                        lower = spread - qnorm(0.975)*se,
                        upper = spread+ qnorm(0.975)*se)

#Exercise 2 - Compare to Actual Results
# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.

p_hits <- ci_data %>% mutate(hit = (actual_spread > lower & actual_spread < upper))%>%
  summarise(mean = mean(hit))

#Exercise 3 - Stratify by Pollster and Grade
# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis<- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
p_hits <- ci_data %>% mutate(hit = (actual_spread >= lower & actual_spread <= upper))%>%
  group_by(pollster) %>% filter(n() >= 5) %>% summarise(proportions_hits = mean(hit), n = n(), grade = grade[1]) %>% arrange(desc(proportions_hits))

#Exercise 4 - Stratify by State
p_hits <- ci_data %>% mutate(hit = (actual_spread >= lower & actual_spread <= upper))%>%
  group_by(state) %>% filter(n() >= 5) %>% 
  summarise(proportion_hits = mean(hit), n = n()) %>% 
  arrange(desc(proportion_hits))

# The `p_hits` data have already been loaded for you. Use the `head` function to examine it.
head(p_hits)

# Make a barplot of the proportion of hits for each state
library(ggplot2)
p_hits %>% ggplot(aes(state, proportion_hits, fill = state)) +
  geom_bar( stat = "identity") + coord_flip()

#Exercise 6 - Predicting the Winner
# The `cis` data have already been loaded. Examine it using the `head` function.
head(cis)

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
names(cis)
errors <- cis %>%mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
# Examine the last 6 rows of `errors`
tail(errors, -6)

#Exercise 7 - Plotting Prediction Results
# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has 5 or more polls
p_hits <- errors %>% group_by(state) %>% filter( n() >= 5) %>% 
  summarise(proportion_hits = mean(hit), n = n())

# Make a barplot of the proportion of hits for each state
p_hits %>% arrange(desc(proportion_hits)) %>% 
  ggplot(aes(state,proportion_hits, fill = state)) +
  geom_bar( stat = "identity") + coord_flip()

#Exercise 8 - Plotting the Errors
# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate a histogram of the error
hist(errors$error)

# Calculate the median of the errors. Print this value to the console.
print(median(errors$error))

#Exercise 9- Plot Bias by State
# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c('A+','A','A-','B+')) %>% arrange(error) %>%
ggplot(aes(state, error)) +
  geom_boxplot() +
  geom_point()

#Exercise 10 - Filter Error Plot
errors %>% filter(grade %in% c('A+','A','A-','B+')) %>% group_by(state) %>%
  filter(n() >= 5) %>% ungroup() %>% arrange(error) %>% 
  ggplot(aes(state, error)) + geom_boxplot() + geom_point()

#The t-Distribution
#Calculating 95% confidence intervals with the t-distribution
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)


#Example
data("polls_us_election_2016")
polls_us_election_2016 %>% filter(state == 'Wisconsin' &
                                    enddate >= '2016-10-31' &
                                    (grade %in% c("A+","A","A-","B+")| is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  mutate(state = as.character(state)) %>%
  left_join(results_us_election_2016, by = 'state') %>%
  mutate(actual = clinton/100 - trump/100) %>%
  summarize(actual = first(actual), avg = mean(spread),
            sd = sd(spread), n = n()) %>%
  select(actual,avg,sd,n)

#Exercises
# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
1 - pt(2,3) + pt(-2,3)

#Plotting the t-distribution
# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- seq(3,50,0.5)

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(N){
  prob = 1 - pt(2,N) + pt(-2,N)
  prob
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply(df,pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df, probs)

#Sampling From the Normal Distribution
# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B,{muestra <- sample(x,N, replace = TRUE)
x_bar <- mean(muestra)
se_bar <- sd(muestra)/sqrt(N)
interval <- c(x_bar - qnorm(0.975)*se_bar, x_bar + qnorm(0.975)*se_bar)
cotenido <- between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
print(mean(res))

#Sampling from the t-Distribution
# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res <- replicate(B,{muestra <- sample(x,N, replace = TRUE)
x_bar <- mean(muestra)
se_bar <- sd(muestra)/sqrt(N)
interval <- c(x_bar - qt(0.975, N-1)*se_bar, x_bar + qt(0.975,N-1)*se_bar)
cotenido <- between(mu, interval[1], interval[2])
})


# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
print(mean(res))

# Research funding rates example
# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

#Two-by-two table and p-value for the Lady Tasting Tea problem
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

#Chi-squared test
# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate)) 

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
 chisq.test()
chisq_test$p.value

#Odds ratio
# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women

#p-value and odds ratio responses to increasing sample size
# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()

#Exercise 1 - Comparing Proportions of Hits
# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals <- as.data.frame(errors %>% filter(grade %in% c('A-','C-')) %>% group_by(grade, hit) %>% summarise(n()) %>% spread(grade, 'n()'))

# Print the proportion of hits for grade A- polls to the console
print(totals[2,3]/(totals[1,3]+totals[2,3]))

# Print the proportion of hits for grade C- polls to the console
print(totals[2,2]/(totals[1,2]+totals[2,2]))

#
# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test <- totals %>% select(-hit) %>% chisq.test()


# Print the p-value of the chi-squared test to the console
print(chisq_test$p.value)


# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)
names(totals) = c('hit','C','A')
# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- with(totals,(C[2]/sum(C))/(C[1]/sum(C)))

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- with(totals,(A[2]/sum(A))/(A[1]/sum(A)))

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C


