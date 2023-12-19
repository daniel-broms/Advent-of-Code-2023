#Day 18 : Form trench boundary. Count area of lake.
library(tidyverse)

main() #106459 : correct!

main <- function(){
  #input <- readLines("Day 18/test input.txt")   
  input <- readLines("Day 18/input.txt") 
  
  t <- tibble(input)
  t <- separate(t, col=input, into=c('operation', 'value', 'color') , sep= ' ')     
  
  m <- matrix(".", ncol=600, nrow=350)  #real input
  #m <- matrix(".", ncol=20, nrow=20)  #real input
  
  
  #Follow instructions, dig trench in the matrix. Begin at (1,1).
  pos <- c(row=250, col=200)
  #pos <- c(row=1, col=1)
  m[pos["row"], pos["col"]] <- "#"
  
  for(i in 1:nrow(t)){
    operation <- t[[i, "operation"]]
    value     <- t[[i, "value"]]
    pos_diff  <- switch(operation,
                     "U"=c(-1, 0),
                     "D"=c( 1, 0), 
                     "R"=c( 0, 1), 
                     "L"=c( 0,-1))
    for(j in 1:value){
      pos <- pos + pos_diff
      
      if(pos["row"] > 1000 | pos["row"] < 1 | pos["col"] > 1000 | pos["col"] < 1) browser()
      m[pos["row"], pos["col"]] <- "#"  
      
      }
  }

  #Flood fill with BFS inside from R2C2. matrix m act as the "reached" set.
  frontier <- collections::queue()
  
  #pos <- c(row=2, col=2)   #test input
  pos <- c(row=248, col=200)   #real input : begin above the start point.
  frontier$push(list(pos))
  iteration <- 1
  
  while(frontier$size() > 0){
    pos <- unlist(frontier$pop())
    if(m[pos["row"], pos["col"]] == "."){
      m[pos["row"], pos["col"]] <- "#"
      if(iteration %% 100 == 0) print(c(iteration, sum(m=="#"), frontier$size() ))
      iteration <- iteration + 1
      
      #look for frontier neighbors
      cpos <- pos + c(0, 1);  if(check_tile(cpos,m)) frontier$push(list(cpos))
      cpos <- pos + c(0,-1);  if(check_tile(cpos,m)) frontier$push(list(cpos))
      cpos <- pos + c( 1,0);  if(check_tile(cpos,m)) frontier$push(list(cpos))
      cpos <- pos + c(-1,0);  if(check_tile(cpos,m)) frontier$push(list(cpos))
    }
  }
  sum(m=="#")
}

#check is a given tile is valid and empty, if so return True, otherwise False.
check_tile <- function(pos,m){
  if(pos["row"] < 1 | pos["row"] > ncol(m) | pos["col"] < 1 | pos["col"] > ncol(m)) return(F)
  if(m[pos["row"], pos["col"]] == ".") return(T) else return(F)
}


################################################### Labs ################################################### 
#Find the min/max row and col of # in m.  Cols : -180 to +390 Rows: -230 to +86.    Create matrix with 600 cols, 350 rows Start at row 250, col 200, 
maxcol <- 1000  #1390
mincol <- 1000  #821
maxrow <- 1000  #1086
minrow <- 1000  #770

for(r in 1:nrow(m)){
  for(c in 1:ncol(m)){
    if(m[r,c] == "#"){
      if(r < minrow) minrow <- r
      if(r > maxrow) maxrow <- r
      if(c < mincol) mincol <- c
      if(c > maxcol) maxcol <- c
      
    }
  }
}
