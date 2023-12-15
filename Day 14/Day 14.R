#Day 14  Tilt the platform so that the rounded rocks all roll north. Afterward, what is the total load on the north support beams?
library(tidyverse)

input <- readLines("Day 14/test input.txt")   
input <- readLines("Day 14/input.txt") 

#Convert input to a matrix, one character in each position.
m <- map(input, str_split_1, pattern='') |>unlist() |> matrix(nrow=length(input), byrow=T)


#Roll north: Iterate all rocks, beginning from row 2, move each "O" as far up as possible.
for(row in 2:nrow(m)){
  for(col in 1:ncol(m)){
    y <- row
    repeat{
      if(y > 1 && m[y, col] == "O" && m[y - 1, col] == "."){
        m[y, col] <- "."
        m[y-1, col] <- "O"
        y <- y-1
      } else {
        break
      }
    }
  }
}

#Count total load
load <- 0
for(row in 1:nrow(m)){
  for(col in 1:ncol(m)){
   if(m[row,col] == "O") load <- load + nrow(m) - row + 1 
  }
}
load

#Part 2 : what is the load after 1000000000 cycles?
#Does the load stabilize after a while? Or does it "cycle" in some pattern?
