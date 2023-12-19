#Day 18 : Form trench boundary. Count area of lake.
#Part 2 : Much larger areas, we cannot use a matrix of individual tiles any more. 

#Mabe make rectangles for all contained areas? Or apply "shoelace" formula?
library(tidyverse)
options(scipen=999) 

main() #63806916814817 (when we add "perimeter length/2 + 1" to the shoelace area) That's not the right answer; your answer is too high.  63806916814816 (one less) is also too high
       #63806916814817 (when we cont the types of each corner)
       #63806916814815 (if we reverse the direction of the loop): Also not correct! NOTE two less than the other direction - this supports that a loop always has net 4 extra "outside corners"; all other corners even out.

       #952408144115 for test input - correct! This algorithm works for all inputs except the final real input!!

main <- function(){
  #input <- readLines("Day 18/test input.txt")   
  input <- readLines("Day 18/input.txt") 
  
  t <- tibble(input)
  t <- separate(t, col=input, into=c('operation', 'value', 'color') , sep= ' ')     
  
  t$operation <- t$color |> str_sub(8,8)
  t$operation[t$operation == "0"] <- "R"
  t$operation[t$operation == "1"] <- "D"
  t$operation[t$operation == "2"] <- "L"
  t$operation[t$operation == "3"] <- "U"
  t$value     <- t$color |> str_sub(3,7) |> strtoi(16)
  
  
  #Follow instructions, make a list of vertexes (instead of a matrix)
  vertexes <- list()
  pos      <- c(row=0, col=0)
  vertexes[[1]] <- pos
  
  for(i in 1:nrow(t)){
    operation <- t[[i, "operation"]]
    value     <- t[[i, "value"]] 
    pos_diff  <- switch(operation,
                        "U"=c(-1, 0),
                        "D"=c( 1, 0), 
                        "R"=c( 0, 1), 
                        "L"=c( 0,-1))
    
    pos <- pos + value * pos_diff
    vertexes[[i + 1]] <- pos
  }
 
  #Apply the shoelace formula. Note that the first and last vertex is the same - is this correct??
  a1 <- 0
  a2 <- 0
  for(i in 1:(length(vertexes) - 1)){
    a1 <- a1 + vertexes[[i]]["col"] * vertexes[[i + 1]]["row"] 
    a2 <- a2 + vertexes[[i]]["row"] * vertexes[[i + 1]]["col"] 
  }
  
  area <- 0.5 * (a1 - a2)  #This is the area INSIDE the polygon, where to polygon runs in the middle of the 1-meter wide trench.. 
  
  #In addition, we have 1/2 are per straight run, 3/4 are per "right turn", 1/4 area per "left turn".
  #every vertex is a left or right turn. Find out which for each vertex by comparing the dierction with the previous direction:
  # R-D, D-L, L-U, U-R are right turns. The rest are left turns.
  # Also add a special case for the very first vertex : From Last to First vertex. 
  # The straight parts are simply sum of (length-1) for each vertex.
  
  straight <- sum(t$value - 1)/2
  corners <- 0
  for(i in 2:nrow(t)){
    corners <- corners + vertex_area(t$operation[[i-1]], t$operation[[i]]) 
  }
  #Add last corner (from last to first matrix : )
  corners <- corners + vertex_area(t$operation[[nrow(t)]], t$operation[[1]])
  
  return(area + straight + corners) 
}

#calculate the extra area of a vertex given previous direction and new direction. NOTE THEAT WE MIGHT NEED TO REVERSE LEFT/RIGHT IF THE LOOP DIRECTION CHANGES!
vertex_area <- function(pdir, dir){
  turn <- "Left"
  if(pdir == "R" & dir == "D") turn <- "Right"
  if(pdir == "D" & dir == "L") turn <- "Right"
  if(pdir == "L" & dir == "U") turn <- "Right"
  if(pdir == "U" & dir == "R") turn <- "Right"
  
  if(turn=="Right") return(0.75) else return(0.25)  #try reversing left/right if loop goes the other direction
  #if(turn=="Left") return(0.75) else return(0.25)
}

