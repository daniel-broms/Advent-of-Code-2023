#Day 14  Tilt the platform so that the rounded rocks all roll north. Afterward, what is the total load on the north support beams?
library(tidyverse)

input <- readLines("Day 14/test input.txt")   
input <- readLines("Day 14/input.txt") 

#Convert input to a matrix, one character in each position.
m <- map(input, str_split_1, pattern='') |>unlist() |> matrix(nrow=length(input), byrow=T)

#Do 500 cycles, see if we have a pattern:
l <- vector(mode="integer", length=1000)
for(i in 1:500){
  m <- roll_north(m)
  m <- roll_west(m)
  m <- roll_south(m)
  m <- roll_east(m)
  l[i] <- calc_load(m)
  print(c(i,l[i]))
}

write.csv(l, file = 'Day 14/load.csv') #inspect the pattern in Excel
# TEST INPUT: After 7   initial cycles we see a pattern, beginning at cycle 8   and repeating every 7  cycles: (8, 15...)
# REAL INPUT: After 159 initial cycles we see a pattern, beginning at cycle 160 and repeating every 52 cycles: (160, 212, 264, 316, 368..)
cycle_test <- c(63,	68,	69,	69,	65,	64,	65)  #Test input
cycle_real <- l[160:211]                     #Real input   

getload2(n=1000000000, start_at=8,   cycles=cycle_test)  #64     : correct for test input!
getload2(n=1000000000, start_at=160, cycles=cycle_real)  #100310 : correct!


####################################### utility functions ###########################
#Calculate the load after n cycles, given a start_at point and a cycle pattern 
getload2 <- function(n, start_at, cycles){
  n <- (n-start_at) %% (length(cycles)) + 1
  return(cycles[n])
}

#Count total load for one pattern
calc_load <- function(m){
  load <- 0
  for(row in 1:nrow(m)){
    for(col in 1:ncol(m)){
      if(m[row,col] == "O") load <- load + nrow(m) - row + 1 
    }
  }
  load
}

#Roll north: Iterate all rocks, beginning from row 2, move each "O" as far up as possible.
roll_north <- function(m){
  for(row in 1:nrow(m)){
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
  return(m)
}

#Roll south: Iterate all rocks, beginning from row 2, move each "O" as far up as possible.
roll_south <- function(m){
  for(row in nrow(m) : 1){
    for(col in 1:ncol(m)){
      y <- row
      repeat{
        if(y < nrow(m) && m[y, col] == "O" && m[y + 1, col] == "."){
          m[y, col] <- "."
          m[y+1, col] <- "O"
          y <- y+1
        } else {
          break
        }
      }
    }
  }
  return(m)
}

#Roll west: Iterate all rocks, beginning from col 2, move each "O" as left as possible.
roll_west <- function(m){
  for(row in 1:nrow(m)){
    for(col in 1:ncol(m)){
      x <- col
      repeat{
        if(x > 1 && m[row, x] == "O" && m[row, x - 1] == "."){
          m[row, x] <- "."
          m[row, x-1] <- "O"
          x <- x-1
        } else {
          break
        }
      }
    }
  }
  return(m)
}

#Roll east: Iterate all rocks, beginning from col 2, move each "O" as left as possible.
roll_east <- function(m){
  for(row in 1:nrow(m)){
    for(col in ncol(m):1){
      x <- col
      repeat{
        if(x < ncol(m) && m[row, x] == "O" && m[row, x + 1] == "."){
          m[row, x] <- "."
          m[row, x+1] <- "O"
          x <- x+1
        } else {
          break
        }
      }
    }
  }
  return(m)
}
