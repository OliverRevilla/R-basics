#### Probability ####
# Monte Carlo simulations for categorical data
# Function: sample(): to get one sample.
#Function: rep(): to generate one experiment. Its arguments
#are one vector that contains the list of the subjects and times
# one vector that contains the number of repetitions.
#Function: replicate (): permits us repeat the experiment a lot of times.
#Function: prop.table(): give us the probability.
#Function: paste():to create strings by joinning smaller things.
#Function: expand.grid(): gives us all the permutations of entries of two vectors.
#Function: permutation(): to get the permutation. First we should
# code library(gtools)
beads <- rep(c("red","blue"), times = c(2,3))
beads

sample(beads,1)

B <- 10000
events <- replicate(B, sample(beads,1))
results <- table(events)
prop.table(results)

set.seed(1986)

#With and without replacement
events <- sample(beads, B, replace = TRUE)
prop.table(table(events))

#Conditions and permutations

number <- "Three"
suit <- "Hearts"
paste(number,suit)

paste(letters[1:5], as.character(1:5))

expand.grid(pants = c("blue","black"), 
            shirt = c("white","grey","plaid"))
suits <- c("Diamonds","Clubs","Hearts","Spades")
numbers <- c("Ace","Deuce","Three","Four","Five","Six","Seven",
             "Eight","Nine","Ten","Jack","Queen","King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

kings <- paste("King", suits)
mean(deck %in% kings)

library(gtools)
permutations(3,2) #Permutation of 3 in 2.

all_phone_numbers <- permutations(10,7,v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n,5)
all_phone_numbers[index,]

hands <- permutations(52,2,v = deck)

first_card <- hands[,1]
second_card <- hands[,2]


kings <- paste("King", suits)
sum(first_card %in% kings)

#Probability that the second card contains one king is the first one got one

sum(first_card%in%kings & second_card%in%kings)/sum(first_card%in%kings)

#Combinations

combinations(3,2)

aces <- paste("Ace", suits)

facecard <- c("King","Queen","Jack","Ten")
facecard <- expand.grid(number = facecard,suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52,2,v = deck)
mean(hands[,1]%in%aces & hands[,2]%in%facecard)

#Monte Carlo example

hand <- sample(deck, 2)
hand

# Monty Hall Problem

B <- 10000 # number of replicates
monty_hall <- function(strategy){
  doors <- as.character(1:3) # doors the i could choose
  prize <- sample(c("car","goat","goat"))
  prize_door <- doors[prize == "car"] # the prize door
  my_pick <- sample(doors,1) # my first pick
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)# the door will show us
  stick <-my_pick
  stick == prize_door
  switch <- doors[!doors%in%c(my_pick, show)] # if we decide to change it
  choice <- ifelse(strategy == "stick", stick, switch)
  choice == prize_door
  }

stick <- replicate (B, monty_hall("stick"))
mean(stick)

stick <- replicate (B, monty_hall("switch"))
mean(stick)

#Birthday problem

n <- 50
bdays <- sample(1:365,n,replace = TRUE)
bdays

table(duplicated(bdays))

B <- 10000
same_birthday <- function(n){
  bdays <- sample(1:365,n,replace = TRUE)
  any(duplicated(bdays))
}

same_birthday(50)

  results <- replicate(B, same_birthday(25))
  mean(results)

compute_prob <- function(n, B= 10000){
  results <- replicate(B, same_birthday(n))
  mean(results)
}

# sapply function

n <- seq(1,60)
prob <- sapply(n,compute_prob)

library(tidyverse)
prob <- sapply(n, compute_prob)
qplot(n, prob)


#using the multiplication rule
exact_prob <- function(n){
  prob_unique <- seq(365,365-n+1)/365
  1 - prod(prob_unique)
  }

eprob <- sapply(n, exact_prob)
qplot(n,prob) + geom_line(aes(n,eprob), col = "red")

#Infinity in practice
B <- 10^seq(1,5,len = 100)
compute_prob <- function(B, n=25){
  same_day <- replicate(B, same_birthday(n))
  mean(same_day)
}

prob <- sapply(B,compute_prob)

qplot(log10(B),prob, geom = "line")

