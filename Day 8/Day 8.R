#Day 8

library(tidyverse)

input <- readLines("Day 8/test input.txt")   
input <- readLines("Day 8/input.txt") 

#format instructions as a vector
steplist <- input[1] |> str_split_1('')

#Format map to a tibble
t <- tibble(input = input[3:length(input)])
t <-       separate(t,          col=input,                   into=c('from', 'value')     , sep= ' = '               )     
t <-       separate(t,          col=value,                   into=c('left', 'right')     , sep= ', '                )  
t$left  <- substr(t$left, 2,4)
t$right <- substr(t$right,1,3)

#Follow instruction until we reach zzz
steps  <- 0               #step counter
ix     <- 1               #where we are in instruction list
node   <- 'AAA'           #current node
dir_col <- c(R='right', L='left')

repeat{
  
  #Follow next R/L instruction, see if we have reached ZZZ.
  instruction <- dir_col[steplist[ix]]
  node <- t |> filter(from == node) |>pluck(instruction)
  ix <- ix + 1
  if(ix > length(steplist)) ix <- 1
  steps <- steps + 1
  
  if(node == "ZZZ") break
  print(c(steps, instruction, node))

}
steps  #18827



  

