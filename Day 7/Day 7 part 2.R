#Day 7 part 2: Rank card hands - Jacks are now Jokers.
library(tidyverse)

input <- readLines("Day 7/test input.txt")   
input <- readLines("Day 7/input.txt") 

t <- tibble(input)
t <- separate_wider_delim(t, input, delim = ' ', names = c('hand', 'bid'))
t$bid     <- strtoi(t$bid)
t$type    <- handtypev(t$hand)
t$handval <- handvalv(t$hand)
t <- arrange(t, type, handval)
t <- rowid_to_column(t)
sum(t$rowid * t$bid)             #The final result


#Function to classify hand types into the below:
#five-of-a-kind = 7, four-of-a-kind = 6, full house=5, three-of-a-kind=4, two pair=3, one pair=2, high card=1
#Part 2 : J cards are jokers and can be used to create any type of hand.
handtype <- function(hand) {
  hand <- str_split_1(hand, '')  
  ctc  <- length(unique(hand))                   #nr  of distinct cards
  nj    <- length(hand[hand == "J"])             #nr of jokers
  if(nj < 5){
    m    <- max(table(hand[! hand == 'J']))      #how many of the most frequent card (excluding Jokers)
  }
  else m <- 0
  ctcej <- length(unique(hand[! hand == 'J']))  #nr  of distinct cards, excluding jokers

  if(nj ==5)     return(7)                      #special case : All jokers 
  if(ctcej == 1) return(7)                      #five of a kind
  if(ctcej == 2){                               #four of a kind or Full house
    if(m + nj == 4) return(6) else return(5)
  }  
  if(ctcej == 3){                               #Three of a kind or Two pair
    if(m + nj == 3) return(4) else return(3)
  }
  if(ctcej == 4)    return(2) else return(1)    #If we have four card types return"Pair", otherwise (5 types) return "high card"
}
handtypev <- Vectorize(handtype)

#calculate the hexadecimal card value of a hand. Convert each to a hex digit.
handval <- function(hand){
  hand <- str_split_1(hand, '') 
  hand <- cardval(hand)
  str_c(hand, collapse="") |> strtoi(base=16L)
}
handvalv <- Vectorize(handval)

#calculate the hexadecimal card value of one card.
cardval <- function(card){
  case_match(
    card,
    "T" ~ "A", 
    "J" ~ "1",     #In part 2, J cards have the lowest value of all.
    "Q" ~ "C",
    "K" ~ "D",
    "A" ~ "E",
    .default = card
  )
}


# 250023765 That's not the right answer; your answer is too low.
# 250026256 That's not the right answer; your answer is too low.
# 252137110 That's not the right answer; your answer is too HIGH.
# 249750334 Not the correct answer
# 250384185  #Correct answer!


