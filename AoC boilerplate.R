############################################################################################################
#Data import boilerplate code
############################################################################################################
library(tidyverse)

#Import text file lines to a character vector using readlines:
input <- readLines("Day 8/input.txt")                        #Simply imports text lines, separated by newline.

#Import text file values using scan(). 
input <- scan(file="Day 1/input.txt", what=integer() )       #All values must be of the same type, specified by "what" (numeric() is default). Good for vectors or matrixes.
m<-matrix(scan(file="Day 1/input.txt"), ncol=2, byrow=T)     #Transform scanned values to a 2-column matrix.

#import a table using read.table. Good for tabular data separated by white spaces.
t<-read.table("Day 8/input.txt", col.names = c('operation', 'value'))                     #base R version
t<-read.delim("Day 8/input.txt", col.names = c('operation', 'value'), sep=' '  )          #similar, other defaults.

#Tidyverse readr: read_csv(), read_delim(), read_fwf(), read_table(). It will guess data types,delimiters etc!
#Import a fixed-width table using read_fwf():
t <- read_fwf("Day 3/input.txt",col_positions = fwf_widths(rep(1,32)) )                   #Read input as 32 fixed-length text columns into a tibble. Each column has width 1.       

#Convert input to a matrix, one character in each position.
m <- map(input, str_split_1, pattern='') |>unlist() |> matrix(nrow=length(input), byrow=T)

##################  Sample data ##############################################################################
#AoC 2020 Day 3 has 32-character lines containg "#" or "."
#AoC 2020 Day 4 has multiline messages, and varying number of fields.
#AoC 2020 Day 7 has "bag specifications" (ragged list)
#AoC 2020 Day 8 has two-column operation/value list.


##################  separate  (for data frame) ##############################################################################
#Use tidyr "separate" to split a string column in a data frame into two (or more) columns:
input <- readLines("Day 8/input.txt", )    #sample input with two columns separated by space
t <- tibble(input) #First create a tibble with just one column (the string input)

# syntax : separate(data frame, col=target column in tibble, into=names of new columns        , sep= separator expression : Can be a regular expression!)
t <-       separate(t,          col=input,                   into=c('operation', 'value')     , sep= ' '                 )     

##################  unite  ##############################################################################
#Use tidyr "unite" to paste together columns
t <- unite(t, col="NewCol",'operation', 'value' )

################## unnest ###################################################################################
#Use str_split and then unnest to convert columns with list data of varying length into separate rows.

input <- readLines("Day 7/input.txt")                    #594 rules are imported.
input <- str_replace_all(input, 'bags', 'bag')           #we have mixed bag/bags, make all singular.

#Create a tibble with the parent bag in one column and the child list in another column.
t <- tibble(parent  =str_sub(input, 1, str_locate(input, " contain")[,1]-1) , 
            children=str_sub(input,    str_locate(input, " contain")[,1]+8) )

t$child <- str_split(t$children, ',')                    #Split the children to a vector with each child as one element
t <- unnest(t, child)                                    #unnest the child vector to cr eate one row per child


####################################### Algorithms #####################################
#BFS : Breadth-first search
#DFS : Depth-first search

#Djikistra shortest path: Example from AoC 2022 day 12
#  target is the vertex we are looking for
#  gr is the vertex list (just a height list in this case)
#  lookup is a list of nodes reachable from each other node (i.e. th edges)

find_way <- function(target) {
  q <- collections::priority_queue(which(data12 == "E"), priorities = 0L)        #priority queue, initialized with starting node (E in this case)
  dist <- c(rep.int(10000L, length(gr)))                                         #distance matrix, initialized to Inf
  dist[which(data12 == "E")] <- 0L                                               #change distance to starting node to 0
  
  while (q$size() > 0) {                                                         #while queue (of tiles to visit) is not empty:
    cur <- q$pop()                                                               #Get the next tile to visit, on order of shortest distance   
    if (any(cur == target)) return(dist[cur])                                    #Return if we have reached the destination node                                   
    cur_dist <- dist[cur]                                                        #Get the distance of the current node to the starting node
    for (ne in lookup[[cur]][gr[lookup[[cur]]] + 1L >= gr[cur]]) {               #iterate all nodes reachable from current node : All neighbors with max value = (current node value + 1)
      if (dist[ne] > cur_dist + 1L) {                                            #update the neighbor node distance if we have found a shorter path (i.e current node distance to start node + distance to next node)
        dist[ne] <- cur_dist + 1L
        q$push(ne, priority = -cur_dist - 1L)                                    #Add the neighbor node to the queue of nodes to visit. Set the priority = (negative) total distance to this node.
      }
    }
  }
}



#a* : optimized version of the above, only check certain paths.