#Day 16 : Part 2 : Find the initial beam configuration that energizes the largest number of tiles; how many tiles are energized in that configuration?
library(tidyverse)

options(error=NULL) #error=recover to see stack trace at the point of error.
options(show.error.locations=TRUE)

input <- readLines("Day 16/test input.txt")   
input <- readLines("Day 16/input.txt") 
m <- map(input, str_split_1, pattern='') |> unlist() |> matrix(nrow=length(input), byrow=T)

#Try all possible initial configurations, fix the max nr of energized tiles.
# main()
main <- function(){
  results <- 0
  for(i in 1:nrow(m)){
    results <- c(results,  calc_config(m, c(xpos=1,       ypos=i,       xdir= 1, ydir= 0)))   #from left
    results <- c(results,  calc_config(m, c(xpos=nrow(m), ypos=i,       xdir=-1, ydir= 0)))   #from right
    results <- c(results,  calc_config(m, c(xpos=i,       ypos=1,       xdir= 0, ydir= 1)))   #from top
    results <- c(results,  calc_config(m, c(xpos=i,       ypos=nrow(m), xdir= 0, ydir=-1)))   #from bottom
  }
  max(results)
}
main() #7896 : Correct!

#Calculate the number of energized tiles for any initial_config.
calc_config <- function(m, initial_config){

  #Convert input to a matrix, one character in each position. Note that "\" becomes "\\" (i.e. an escaped backslash)
  m <- map(input, str_split_1, pattern='') |> unlist() |> matrix(nrow=length(input), byrow=T)
  m2 <- matrix(data=0L,nrow=nrow(m), ncol=ncol(m))    #matrix to track where we have been - energized tiles
  
  #A beam is represented by an integer vector with xpos, ypos,xdir, ydir.
  beam <- initial_config  # c(xpos=1, ypos=1, xdir=1, ydir=0)
  l <- list()
  l[[1]] <- beam
  iteration <- 1
  
  #Keep track of states. Remove any light beams in a state which we have seen before. 
  seen_states <- new.env()
  

  #Process each beam one turn until no more beams are in the matrix. Record track in m2.
  repeat{
    for(b in seq_along(l)){
      
      #print(c(iteration, sum(m2), length(l)))
      #if(iteration == 764 )browser()
      iteration <- iteration + 1
      if(length(l) == 0 | b > length(l)) break

      #If this beam is outside the matrix : remove it from our beam list
      while(l[[b]]["xpos"] < 1 | l[[b]]["xpos"] > ncol(m) | l[[b]]["ypos"] < 1 | l[[b]]["ypos"] > nrow(m) ){
        l <- l[-b]
        if(length(l) == 0 | b > length(l)) break
      }
      if(length(l) == 0 | b > length(l)) break
      
      #Mark this tile as energized
      m2[l[[b]]["ypos"], l[[b]]["xpos"]] <- 1    
      tile <- m[l[[b]]["ypos"], l[[b]]["xpos"]]
      
      if(tile == "/"){
        if(     all(c(l[[b]]["xdir"], l[[b]]["ydir"]) == c(-1, 0)) )  {l[[b]]["xdir"] <-  0; l[[b]]["ydir"] <-  1}       # Left changes to down
        else if(all(c(l[[b]]["xdir"], l[[b]]["ydir"]) == c( 1, 0)) )  {l[[b]]["xdir"] <-  0; l[[b]]["ydir"] <- -1}       # Right changes to up
        else if(all(c(l[[b]]["xdir"], l[[b]]["ydir"]) == c( 0,-1)) )  {l[[b]]["xdir"] <-  1; l[[b]]["ydir"] <-  0}       # Up changes to Right
        else if(all(c(l[[b]]["xdir"], l[[b]]["ydir"]) == c( 0, 1)) )  {l[[b]]["xdir"] <- -1; l[[b]]["ydir"] <-  0}       # Down changes to Left
        l[[b]]["xpos"] <- l[[b]]["xpos"] + l[[b]]["xdir"]; l[[b]]["ypos"] <- l[[b]]["ypos"] + l[[b]]["ydir"];  # Move one step in this direction
      }
      if(tile == "\\"){
        if(     all(c(l[[b]]["xdir"], l[[b]]["ydir"]) == c(-1, 0)) )  {l[[b]]["xdir"] <-  0; l[[b]]["ydir"] <- -1}       # Left changes to up
        else if(all(c(l[[b]]["xdir"], l[[b]]["ydir"]) == c( 1, 0)) )  {l[[b]]["xdir"] <-  0; l[[b]]["ydir"] <-  1}       # Right changes to down
        else if(all(c(l[[b]]["xdir"], l[[b]]["ydir"]) == c( 0,-1)) )  {l[[b]]["xdir"] <- -1; l[[b]]["ydir"] <-  0}       # Up changes to Left
        else if(all(c(l[[b]]["xdir"], l[[b]]["ydir"]) == c( 0, 1)) )  {l[[b]]["xdir"] <-  1; l[[b]]["ydir"] <-  0}       # Down changes to Right
        l[[b]]["xpos"] <- l[[b]]["xpos"] + l[[b]]["xdir"]; l[[b]]["ypos"] <- l[[b]]["ypos"] + l[[b]]["ydir"];  # Move one step in this direction
      }
      if(tile == '.' | (tile == "-" & l[[b]]["ydir"] == 0) | (tile == "|" & l[[b]]["xdir"] == 0) ) {
        l[[b]]["xpos"] <- l[[b]]["xpos"] + l[[b]]["xdir"]; l[[b]]["ypos"] <- l[[b]]["ypos"] + l[[b]]["ydir"];  # Move one step in existing direction
      }
       
      if(tile == "-" & l[[b]]["xdir"] == 0){ #If we are going up or down and meet "-" : Change direction to Left, add a new beam with direction Right.
        l[[b]]["xdir"] <- -1; l[[b]]["ydir"] <-  0 #Change this beam to Left
        newbeam <- c(xpos=l[[b]][["xpos"]], ypos=l[[b]][["ypos"]], xdir=1, ydir=0)
        l[[length(l) + 1]] <- newbeam
      }
      
      if(tile == "|" & l[[b]]["ydir"] == 0){ #If we are going left or right and meet "-" : Change direction to Up, add a new beam with direction Down
        l[[b]]["xdir"] <- 0; l[[b]]["ydir"] <- -1 #Change this beam to Up
        newbeam <- c(xpos=l[[b]][["xpos"]], ypos=l[[b]][["ypos"]], xdir=0, ydir=1) 
        l[[length(l) + 1]] <- newbeam
      }
    }
    if(length(l) == 0) break

    #Remove beams in states we have seen before. Log states we have not seen before.
    for(b in seq_along(l)){
      if(length(l) == 0 | b > length(l)) break
      if(!is.null(seen_states[[toString(l[[b]])]])){
        l <- l[-b]
      }
      else {
        seen_states[[toString(l[[b]])]] <- 1
      }
      if(length(l) == 0 | b > length(l)) break
    }
    
  }
  print(sum(m2))
  sum(m2)
} 
