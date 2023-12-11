# Day 11 : Part 2 : Now, instead of the expansion you did before, make each empty row or column one million times larger.

library(tidyverse)

input <- readLines("Day 11/test input.txt")   
input <- readLines("Day 11/input.txt") 


#Convert input to a matrix, one character in each position.
m <- map(input, str_split_1, pattern='') |>unlist() |> matrix(nrow=length(input), byrow=T)

#Log all galaxies with index (rownum), xpos, ypos in a tibble.
t <- which(m == "#", arr.ind=T) |> as_tibble()

#Find all empty rows and columns in the matrix.
emptycols <- vector(mode="integer")
for(i in 1:ncol(m)){
  if(all(m[,i] == ".")) emptycols <- c(emptycols, i)
}

emptyrows <- vector(mode="integer")
for(i in 1:nrow(m)){
  if(all(m[i,] == ".")) emptyrows <- c(emptyrows, i)
}

#Increase xpos/ypos with (one)one million minus 1) for each empty row between 0 and x/y.
for(i in 1:nrow(t)){
  t$row[i] <- t$row[i] + sum(emptyrows < t$row[i]) * 999999
  t$col[i] <- t$col[i] + sum(emptycols < t$col[i]) * 999999
}

#Iterate through all galaxies (except the last one), find the distance (xdif + ydiff) to all galaxies AFTR the current galaxy. Accumulate sum of distance.
acc_dist <- 0
for(i in 1:(nrow(t) - 1)){
  pos1 <- c(row=t[[i,1]], col=t[[i,2]])                     #get position of the "outer" galaxy 
  for(j in (i+1):nrow(t)){
    pos2 <- c(row=t[[j,1]], col=t[[j,2]])                   #get position of the "inner" galaxy 
    acc_dist <- acc_dist + sum(abs(pos1 - pos2))            #Find the distance, accumulate.
  }
}
acc_dist
#678728808158 - Correct