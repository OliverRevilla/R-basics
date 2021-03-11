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


























