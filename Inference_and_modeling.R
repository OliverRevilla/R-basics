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










