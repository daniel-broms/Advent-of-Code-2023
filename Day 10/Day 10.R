#Day 10 : navigate the pipes, find the point furthest away from S.

library(tidyverse)

input <- readLines("Day 10/test input.txt")   
#input <- readLines("Day 10/input.txt") 

#Convert input to a matrix
m <- map(input, str_split_1, pattern='') |>unlist() |> matrix(nrow=length(input), byrow=T)

#Define directions
N <- c(-1, 0)
S <- c(1,  0)
W <- c(0, -1)
E <- c(0,  1)

#find starting position as a (row, col) vector
pos <- which(m == "S", arr.ind=T) |> as.vector()
steps <- 0
 
#Choose a first step from S (there are two to choose from) : E or S in test input, N or S in real input. Go south.
laststep <- S
pos <- pos + laststep
steps <- 1

#Loop though map, find furthest point.
repeat{
  nextstep <- getnextstep(m[pos[1], pos[2]], laststep)
  pos <- pos + nextstep
  laststep <- nextstep
  steps <- steps + 1
  print(c(steps, m[pos[1], pos[2]]))
  if(m[pos[1], pos[2]] == "S") break
}
13572/2 #6786 : Correct!


#Function that returns the next step direction given the pipe type and the direction we came there.
getnextstep <- function(pipe, laststep){
  if(pipe == "|"){if(identical(laststep,N)) return(N) else return(S)}
  if(pipe == "-"){if(identical(laststep,W)) return(W) else return(E)}
  if(pipe == "L"){if(identical(laststep,S)) return(E) else return(N)}
  if(pipe == "J"){if(identical(laststep,S)) return(W) else return(N)}
  if(pipe == "7"){if(identical(laststep,N)) return(W) else return(S)}
  if(pipe == "F"){if(identical(laststep,N)) return(E) else return(S)}
  stop("We should not get here")
}

####################### Part 2 : How many tiles are enclosed by the loop? ##################


