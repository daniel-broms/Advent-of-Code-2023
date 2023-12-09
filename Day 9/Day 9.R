#Day 9 : Extrapolate values.
library(tidyverse)

input <- readLines("Day 9/test input.txt")   
input <- readLines("Day 9/input.txt") 

#Apply function "extrapolate" to all inputs, sum the results. 
map_int(input, extrapolate)|> sum()

#Function to extrapolate one input line. Make a numbered list of vectors.
extrapolate <- function(s){
  
  #Make a list of vectors beginning with input vector.
  #Add a "difference vector" until all entries are zero.
  s <- s |> str_split_1(' ') |> strtoi()
  l <- list()
  level <- 1
  l[[level]] <- s
  while( ! all(s == 0)){
    next_s <- vector(mode = "integer", length = length(s) - 1)
    for(i in seq_along(next_s)) {next_s[i] <- s[i + 1] - s[i]}
    s <- next_s
    level <- level + 1
    l[[level]] <- s
    }
  
  #Add new entries to the end of each vector
  add_val <- 0 #begin with a zero
  for(i in (level) : 2){
    #l[[i]] <- c( l[[i]], add_val)  #Do we really need to add this value to the vector - it works anyway??
    next_s <- l[[i - 1]]
    add_val <- next_s[length(next_s)] + add_val
  }
  
  return(add_val)
}




