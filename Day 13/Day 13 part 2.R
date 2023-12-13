#Day 13 : Find reflection lines in patterns.
library(tidyverse)

input <- readLines("Day 13/test input.txt")   
input <- readLines("Day 13/input.txt") 

input <- c(input, "")  #Add an extra line to avoid edge case

#First log the part 1 lines in vector P1lines. Each matrix result is stored as H5 or V3 (Horizontal/Vertical + x/Y position).
P1lines <- vector(mode="character")
ipos <- 1
acc <- 0
for(i in 1:length(input)){
  if(input[i] == ""){
    m <- input[ipos : (i-1)]
    m <- map(m, str_split_1, pattern='') |>unlist() |> matrix(nrow=length(m), byrow=T)
    ipos <- i + 1
    acc <- acc + calc_block(m, T, 0)
  }
}

#then test modified matrices. Skip the lines detected in part 1.
ipos <- 1
acc <- 0
blockno <- 1
for(i in 1:length(input)){
  if(input[i] == ""){
    m <- input[ipos : (i-1)]
    m <- map(m, str_split_1, pattern='') |>unlist() |> matrix(nrow=length(m), byrow=T)
    ipos <- i + 1
    acc <- acc + calc_block_versions(m, blockno)
    blockno <- blockno + 1
  }
}
acc 

#Part 2 : This time one cell is wrong. Test flipping all cells in m until we we find a reflection.
#In each pattern, you'll need to locate and fix the smudge that causes a DIFFERENT reflection line.
calc_block_versions <- function(m, blockno){
  for(col in 1:ncol(m)){
    for(row in 1:nrow(m)){
      m2 <- m
      if(m2[row,col] == "#") m2[row,col] <- "." else m2[row,col] <- "#"
      r <- calc_block(m2, F, blockno)
      if(r > 0 ) return(r)
    }
  }
}

#Process one input block
calc_block <- function(m, loglines, blockno){
  
  #Find a vertical line
  for(x in 1:(ncol(m) - 1)){
    
    #Skip this line if it was detected in part 1.
    if(!loglines){
      if(P1lines[blockno] ==  paste("V", x, sep="")) next
    }
    
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
    if(isline) {
      print(c("V", x, x+1))
      if(loglines) P1lines <<- c(P1lines, paste("V", x, sep=""))
      return(x)  #nr of columns to the left of the vertical line
    }  
  }
  
  #find a horiontal line
  for(y in 1:(nrow(m) - 1)){
    
    #Skip this line if it was detected in part 1.
    if(!loglines){
      if(P1lines[blockno] ==  paste("H", y, sep="")) next
    }
    
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
    if(isline){
      print(c("H", y, y+1))
      if(loglines) P1lines <<- c(P1lines, paste("H", y, sep=""))
      return(y * 100)  #nr of rows above the horizontal line * 100
    }
  }
  return(-1)  #There is no horizontal or vertical reflection line in this block
}


