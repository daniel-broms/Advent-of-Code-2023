#Day 13 : Find reflection lines in patterns.
library(tidyverse)

input <- readLines("Day 13/test input.txt")   
input <- readLines("Day 13/input.txt") 

input <- c(input, "")  #Add an extra line to avoid edge case

#Transform block at a time to a matrix of chars.
ipos <- 1
acc <- 0
for(i in 1:length(input)){
  if(input[i] == ""){
    m <- input[ipos : (i-1)]
    m <- map(m, str_split_1, pattern='') |>unlist() |> matrix(nrow=length(m), byrow=T)
    ipos <- i + 1
    acc <- acc + calc_block(m)
  }
}
acc #27664

#Process one input block
calc_block <- function(m){
  
  #Find a vertical line
  for(x in 1:(ncol(m) - 1)){
    
    #check if all columns left and right of this line are identical.
    x1 <- x
    x2 <- x+ 1
    isline <- T
    while(x1 >= 1 & x2 <= ncol(m)){
      #If any column is not a reflection : break
      if(!all(m[,x1] == m[,x2])){
        isline <- F                 
        break
      }
      x1 <- x1 - 1
      x2 <- x2 + 1
    }
    if(isline) return(x)  #nr of columns to the left of the vertical line
  }
  
  #find a horiontal line
  for(y in 1:(nrow(m) - 1)){
    
    #check if all columns left and right of this line are identical.
    y1 <- y
    y2 <- y+ 1
    isline <- T
    while(y1 >= 1 & y2 <= nrow(m)){
      #If any column is not a reflection : break
      if(!all(m[y1,] == m[y2,])){
        isline <- F                 
        break
      }
      y1 <- y1 - 1
      y2 <- y2 + 1
    }
    if(isline) return(y * 100)  #nr of rows above the horizontal line * 100
  }
  stop("WE SHOULD NOT GET HERE")
}


