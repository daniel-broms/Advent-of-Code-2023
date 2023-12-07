#Day 7 : Rank card hands
library(tidyverse)

input <- readLines("Day 7/test input.txt")   
input <- readLines("Day 7/input.txt") 

t <- tibble(input)

t <- separate_wider_delim(t, input, delim = ' ', names = c('hand', 'bid'))
t$bid <- strtoi(t$bid)
t$type    <- handtypev(t$hand)
t$handval <- handvalv(t$hand)
t <- arrange(t, type, handval)
t <- rowid_to_column(t)
sum(t$rowid * t$bid)             #The final result

#Function to classify hand types into the below:
#five-of-a-kind = 7, four-of-a-kind = 6, full house=5, three-of-a-kind=4, two pair=3, one pair=2, high card=1
handtype <- function(hand) {
  hand <- str_split_1(hand, '')  
  ctc  <- length(unique(hand))  #nr  of distinct cards
  m    <- max(table(hand))      #how many of the most frequent card 
  
  if(ctc == 1) return(7)       #five of a kind

  if(ctc == 2){                #four of a kind or Full house
    if(m == 4) return(6) else return(5)
  }  
  if(ctc == 3){                #Three of a kind or Two pair
    if(m == 3) return(4) else return(3)
  }
  if(ctc == 4) return(2) else return(1)
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
    "J" ~ "B",
    "Q" ~ "C",
    "K" ~ "D",
    "A" ~ "E",
    .default = card
  )
}


