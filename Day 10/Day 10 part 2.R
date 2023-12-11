#Day 10 : navigate the pipes, find the point furthest away from S.

library(tidyverse)

input <- readLines("Day 10/test input.txt")   
input <- readLines("Day 10/input.txt") 

#Convert input to a matrix
m <- map(input, str_split_1, pattern='') |>unlist() |> matrix(nrow=length(input), byrow=T)
m2 <- matrix(" ", nrow=nrow(m), ncol=ncol(m))     #Matrix to track the path with pipe symbols ()
m3 <- matrix(" ", nrow=nrow(m), ncol=ncol(m))     #Matrix to track the path with step nr.

#Define directions
N <- c(-1, 0)
S <- c(1,  0)
W <- c(0, -1)
E <- c(0,  1)

#find starting position as a (row, col) vector
pos <- which(m == "S", arr.ind=T) |> as.vector()
m2[pos[1], pos[2]] <- "S"
steps <- 0
 
#Choose a first step from S (there are two to choose from) : E or S in test input, N or S in real input. Go south.
laststep <- S
pos <- pos + laststep
steps <- 1
m2[pos[1], pos[2]] <- m[pos[1], pos[2]]

#Loop though map, find furthest point.
repeat{
  m2[pos[1], pos[2]] <- m[pos[1], pos[2]] #record our track
  m3[pos[1], pos[2]] <- steps
  nextstep <- getnextstep(m[pos[1], pos[2]], laststep)
  pos <- pos + nextstep
  laststep <- nextstep
  steps <- steps + 1
  #print(c(steps, m[pos[1], pos[2]]))
  if(m[pos[1], pos[2]] == "S") break
}

#13572/2 #6786 : Correct! The path is 13572 steps in total, halfway is the furthest point.

#### Utility functions 

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

lookdir <- function(nextstep){
  #Going S : Look E
  #Going N : Look W
  #Going W : Look S
  #Going E : look N
  if(identical(nextstep, S)) return(E)
  if(identical(nextstep, N)) return(W)
  if(identical(nextstep, W)) return(S)
  if(identical(nextstep, E)) return(N)
}


####################### Part 2 : How many tiles are enclosed by the loop? ##################  This solution works with ALL the test inputs, but not the real input!!!!
#Idea  : Follow the loop starting counter-clockwise = South (using m2), "look to the left" of our travelling direction at each step to see if there are any empty enclosed tiles. If there are, do a BFS to mark all other adjacent empty tiles, mark them as I. Alternatively: first mark all found I, then find & mark all empty tiles adjacent to all I. Change I to X when there are no mode adjacent tiles.

#find starting position (S), go south from there
pos <- which(m2 == "S", arr.ind=T) |> as.vector()
laststep <- S
#laststep <- W   #In this example go west instead!!!!
pos <- pos + laststep
steps <- 1

#Loop though path, look for enclosed tiles in m2
repeat{
  nextstep <- getnextstep(m2[pos[1], pos[2]], laststep)
  
  lookat <- pos + lookdir(nextstep)
  if(lookat[1] > 0 & lookat[1] <= nrow(m2) & lookat[2] >0 & lookat[2] <=ncol(m2)){
    if(m2[lookat[1], lookat[2]] == " "){
      m2[lookat[1], lookat[2]] <- "*"   #Look left, mark any free tiles
    } 
  }  

  pos <- pos + nextstep
  laststep <- nextstep
  steps <- steps + 1
  
  if(m2[pos[1], pos[2]] == "S") break
}

#Now find and adjacent empty tiles to "*", fill these also.  
marked <- which(m2 == "*", arr.ind=T) 

while(nrow(marked) > 0){
  for(i in 1: nrow(marked)){
    mark_all_adjacent(marked[i,])
    m2[marked[i,1], marked[i,2]] <- "@"   #Mark as search completed.
  }
  marked <- which(m2 == "*", arr.ind=T)
}
sum(m2 == "@")  #491 : That's not the right answer; your answer is too low.  492 is also wrong. 5526 : That's not the right answer; your answer is too high.
#495! Which 4 are we missing?



#find all empty tiles around this one
mark_all_adjacent <- function(pos){
  mark_one_adjacent(pos+N)
  mark_one_adjacent(pos+E)
  mark_one_adjacent(pos+S)
  mark_one_adjacent(pos+W)
}

#check one tile, mark is as * if it is empty
mark_one_adjacent <- function(pos){
  if(pos[1] > 0 & pos[1] <= nrow(m2) & pos[2] >0 & pos[2] <=ncol(m2)){
    if(m2[pos[1], pos[2]] == " ") m2[pos[1], pos[2]] <<- "*"
  }
}



###################### Part 2 alternative version ################################################################################
#Using m2, find all cells which are " ". Count the number of pipe crossings from the cell up to row 1. If this nr is odd, the cells is inside the loop.
#Identify horizontal crossings of the type "-", L "  or "F". Ignore 7 and J (the complementery pipes to L and F).

acc_odd <- 0
blank <- which(m2 == " ", arr.ind=T)              #Find all blank tiles.
m4 <- matrix(" ", nrow=nrow(m), ncol=ncol(m))     #Matrix to track internal tiles
for(i in 1:nrow(blank)){
  row <- blank[i,1]
  col <- blank[i,2]
  subset <- m2[1:row, col] 
  if(length(subset[subset == "-" | subset == "L" | subset == "F" ]) %% 2 == 1 ) {
    acc_odd <- acc_odd + 1
    m4[row, col] <- "@"
  }
}
acc_odd #495 

which(m4 == "@", arr.ind=T)

which(m2 == "@", arr.ind=T)


