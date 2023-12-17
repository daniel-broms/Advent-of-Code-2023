#Day 17 : Find the shortest path from top left to bottom right. 
#Part 2 : 
# Once an ultra crucible starts moving in a direction, it needs to move a minimum of four blocks in that direction before it can turn (or even before it can stop at the end)
# an ultra crucible can move a maximum of ten consecutive blocks without turning
library(tidyverse)

input <- readLines("Day 17/test input.txt")   
input <- readLines("Day 17/input.txt") 

#Convert input to a matrix, one character in each position, converted to integer.
m <- map(input, str_split_1, pattern='') |>unlist() |> strtoi() |> matrix(nrow=length(input), byrow=T)
dist <- matrix(Inf, nrow=nrow(m), ncol=ncol(m))

#From each node we can go left, right or forward.
#We cannot reverse.
#We cannot go forward more than 3 steps in a row - after this we can only go left or right.
#State is Position (row,col), PreviousPosition (prow,pcol),  "how many consecutive steps we have taken forward" (steps), cost to get here (cost).

target <- c(nrow(m), ncol(m))
find_way(target)       #881 - correct! (after 760000 iterations)

find_way <- function(target) {
  seen_states   <- new.env()                                                     #keep track of seen states and the total cost getting there.
  queued_states <- new.env()                                                     #keep track of states added to the queue (we were adding too many!)
  distances     <- vector(mode="integer")                                        #Log final distances to the destination.  

  cur <- c(row=1, col=1, prow=1, pcol=1, steps=0, cost=0)                        #Initial state, start at 1,1, coming from 1, 1, 0 steps taken forward, 0 cost
  q <- collections::priority_queue(list(cur), priorities = 0L)                   #priority queue, initialized with starting node (1,1 in this case). (Note that vector cur needs to be embedded in a list before being pushed on the queue - otherwise each individual value in cur is added to q.)
  dist <- matrix(Inf, nrow=nrow(m), ncol=ncol(m))                                #distance matrix, initialized to Inf. (this is not used by the algorithm, only for illustration!)
  dist[1,1] <- 0L                                                                #change distance to starting node to 0
  iteration <- 1
  
  while (q$size() > 0) {                                                         #while queue (of tiles to visit) is not empty:
    cur <- q$pop()                                                               #Get the next tile to visit, in order of shortest distance   
    cur <- unlist(cur)
    if(iteration %% 10000 == 0) {print(c(iteration, min(distances))); print(sum(dist == Inf))}  #just to show progress
    iteration <- iteration + 1

    #if we are at the target after going forward at least 4 steps : add the total cost to get here this way. Continue looking for more ways to get here.
    if (all( c(cur[1], cur[2]) == target & cur[["steps"]] >= 4)) {                                     
      distances <- c(distances, cur[["cost"]])    
    }   

    #make list of neighbors reachable from current node  
    ne_list <- list()
    ne_list[[length(ne_list) + 1]] <- check_neighbor(cur, c(row=cur[["row"]] + 1, col=cur[["col"]] + 0) )
    ne_list[[length(ne_list) + 1]] <- check_neighbor(cur, c(row=cur[["row"]] - 1, col=cur[["col"]] + 0) )
    ne_list[[length(ne_list) + 1]] <- check_neighbor(cur, c(row=cur[["row"]] + 0, col=cur[["col"]] + 1) )
    ne_list[[length(ne_list) + 1]] <- check_neighbor(cur, c(row=cur[["row"]] + 0, col=cur[["col"]] - 1) )
    
    #log this state as seen with the current cost as value
    seen_states[[toString(cur[-6])]] <- cur[[6]]

    for (i in seq_along(ne_list)){                                                          #iterate all nodes reachable from current node : All neighbors with max value = (current node value + next node value)
      if (dist[ne_list[[i]][["row"]], ne_list[[i]][["col"]] ] >  ne_list[[i]][["cost"]]) {  #update the neighbor node distance if we have found a shorter path (i.e current node distance to start node + distance to next node)
          dist[ne_list[[i]][["row"]], ne_list[[i]][["col"]] ] <- ne_list[[i]][["cost"]]
      }
      
      #If we have not seen, or queued, this exact state with the same or lower cost before : Add this neighbor to the queue.
      x <-   seen_states[[toString(ne_list[[i]][-6])]]
      y <- queued_states[[toString(ne_list[[i]][-6])]]
      if( (is.null(x) || x > ne_list[[i]][6]) & (is.null(y)|| y > ne_list[[i]][6]) ){
        q$push(ne_list[i], priority = -(ne_list[[i]][6]))     
        queued_states[[toString(ne_list[[i]][-6])]] <- ne_list[[i]][[6]]
      }
    }
  }
  return(min(distances))
}


#check if tile(row,col) is reachable from state (row, col, prow, pcol, steps, cost). If it is, return the next state. If not, return NULL.
check_neighbor <- function(state, tile){
  next_steps <- 1

  if(tile["row"] < 1 | tile["row"] > nrow(m) | tile["col"] < 1 | tile["col"] > ncol(m)) return(NULL)    #We cannot move outside the map 
  if(all(tile == c(state["prow"], state["pcol"]))) return(NULL)                                         #We cannot go back to previous tile
  last_direction <- c(state["row"] - state["prow"], state["col"] - state["pcol"])                       #Find direction vector as (deltarow, deltacol)
  next_direction <- c(tile["row"]  - state["row"] , tile["col"]  - state["col"])                        #Find direction vector to Tile
  
  if(all(state == c(1,1,1,1,0,0))) last_direction <- next_direction                                     #special case for first step, count as a step forward.
  
  #PART 2 restrictions: at least 4 and at most 10 consecutive 
  if(all(next_direction == last_direction)){                                                            #If we are moving forward:  
    if(state["steps"] >= 10) return(NULL) else next_steps <- state[["steps"]] + 1                       #We cannot move in the same direction more then 10 times in a row 
  }
  
  if(!all(next_direction == last_direction)){                                                           #If we are NOT moving forward:  
    if(state["steps"] < 4) return(NULL) else next_steps <- 1                                            #We must have moved forward at least 4 steps before we can turn.
  }

  next_state <- c(row=tile[["row"]], col=tile[["col"]], prow=state[["row"]], pcol=state[["col"]], steps=next_steps, cost=state[["cost"]]+ m[tile["row"], tile["col"]])    #return this neighbor as a valid state. Set steps=0 if we change direction, otherwise increment steps by 1.
  return(next_state)
}
