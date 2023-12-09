#Day 8 Part 2 : navigate all paths at the same time, end when all paths end with "Z".

library(tidyverse)

input <- readLines("Day 8/test input.txt")   
input <- readLines("Day 8/input.txt") 

#format instructions as a vector
steplist <- input[1] |> str_split_1('')

#Format map to a tibble
t <- tibble(input = input[3:length(input)])
t <- separate(t, col=input, into=c('from', 'value'), sep= ' = ')     
t <- separate(t, col=value, into=c('left', 'right'), sep= ', ' )  
t$left  <- substr(t$left, 2,4)
t$right <- substr(t$right,1,3)

#Follow instruction until all nodes end with "Z"
steps  <- 0                                        #step counter
ix     <- 1                                        #where we are in instruction list
nodes  <- t |>filter(str_sub(from,3,3) == "A") |> pull(from)    #Begin with all nodes ending with "A"
dir_col <- c(R=3, L=2)                                          #Named vector to convert instruction to column indexes

#Brute force (as in part 1) : Does not finish with real input! (works fine for test input.)
repeat{
  
  #Follow next R/L instruction, see if we have reached ZZZ.
  instruction <- dir_col[steplist[ix]]
  nodes <- t |> filter(from %in% nodes) |> pull(instruction)
  ix <- ix + 1
  if(ix > length(steplist)) ix <- 1
  steps <- steps + 1
  
  if(all(substr(nodes, 3,3) == "Z")) break
  #print(c(steps, instruction, node))

}
steps 

##################################################################################################################
#We need an analytical approach!
#Each node will hit "..Z" with a regular cycle; analyze the cycles of each and find when they will co-incide.
# "KLA" 17141  
# "AAA" 18827
# "NDA" 20513
# "LBA" 12083
# "NNA" 22199
# "QVA" 19951
steps  <- 0                                        #step counter
ix     <- 1                                        #where we are in instruction list
node <- "LBA"
repeat{
  
  #Follow next R/L instruction, see if we have reached ZZZ.
  instruction <- dir_col[steplist[ix]]
  node <- t |> filter(from == node) |>pluck(instruction)
  ix <- ix + 1
  if(ix > length(steplist)) ix <- 1
  steps <- steps + 1
  
  if(substr(node, 3,3) == "Z") print(steps)
  #print(c(steps, instruction, node))
  
}
steps

#Find the least common multiple of all cycles:
cycles <- c(17141, 18827, 20513, 12083, 22199, 19951)
library(mpoly)
answer <- Reduce(LCM, cycles)
options(scipen = 999)  # 20220305520997  : Correct!

