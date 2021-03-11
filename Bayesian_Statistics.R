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










