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












