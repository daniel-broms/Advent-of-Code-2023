#Day 4 : Take a seat in the large pile of colorful cards. How many points are they worth in total?
library(tidyverse)

#Split into the two sets of numbers
input <- readLines("Day 4/test input.txt")   
input <- readLines("Day 4/input.txt")   

pos1 <- str_locate(input[1], ':')[1]
pos2 <- str_locate(input[1], '\\|')[1]

t <- tibble(card = str_sub(input, 5, pos1 - 1), winners =str_sub(input, pos1 + 2, pos2 - 2), numbers = str_sub(input, pos2 + 2, -1))


#Calculate the score for one card
score <- function(winners, numbers){
  winners <- str_split_1(str_trim(winners), ' +')
  numbers <- str_split_1(str_trim(numbers), ' +')
  common <- intersect(winners, numbers)
  x <- length(common)
  if (x == 0) return(0) else  return(2 ** (x-1))
}

scorev <- Vectorize(score)

#Get the final sum
sum(scorev(t$winners, t$numbers))  #26426

############################## Part 2 ##################################
#Process all of the original and copied scratchcards until no more scratchcards are won. Including the original set of scratchcards, how many total scratchcards do you end up with?

#Add an "instances" column to t, initialize to 1. 
#Process cards in order. For each card, increase the "instances" of the (nr of matches) cards below with 1 (per instnce of the current card).

t$instances <- 1

for(card in 1:nrow(t)){
  matchcount <- matches(t[[card,2]], t[[card, 3]])
  cardcount <- t[[card,4]]
  if(matchcount>0){
    for(i in 1:matchcount){
      t[[card + i, 4]] <-  t[[card + i, 4]] + cardcount
    }
  }
}
sum(t$instances)

#Calculate the score for one card - this time just the number of matches
matches <- function(winners, numbers){
  winners <- str_split_1(str_trim(winners), ' +')
  numbers <- str_split_1(str_trim(numbers), ' +')
  common <- intersect(winners, numbers)
  return(length(common))
}



