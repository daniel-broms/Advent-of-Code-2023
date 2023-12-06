#Day 5 : Find lowest location.
library(tidyverse)

############################################### Part 2 #################################################
#Seed input is actually pairs of values with start/range. This gives lots of seeds to calculate - 524 336 848 only in the first par! 
#It is no longer possible to call the function for all individual seed values. 

input <- readLines("Day 5/test input.txt")   
input <- readLines("Day 5/input.txt") 

#Store input seed ranges a a list with a list entry for each range.
seeds <- input[1] |> str_sub(8,-1) |> str_split_1(' ') |> as.numeric()
seed_from <- vector(mode = "numeric", length=length(seeds)/2)
seed_to   <- vector(mode = "numeric", length=length(seeds)/2)
for(i in 1:(length(seeds)/2)){
  seed_from[i] <- seeds[(i-1)*2 + 1]
  seed_to[i]   <- seeds[(i-1)*2 + 1] + seeds[(i-1)*2 + 2] -1
}

#Extract maps to t
rowix <- 1
mapix <- 0
row_list <- vector("list")   #List of collected rows. Each row is a tibble.

#Extract all the rest to a table with MapType, destination, source, range.
for(i in 3:length(input)){
  if(str_detect(input[i], 'map')){
    maptype <- input[i] |> str_sub(1, str_length(input[i])-5)  
    mapix <- mapix + 1
    next
  }
  if(input[i] == "") next
  maprow <- input[i] |> str_trim() |> str_split_1(' ') |> as.numeric()
  row_list[[rowix]] <- tibble(level = mapix, maptype=maptype, destination = as.numeric(maprow[1]), source = as.numeric(maprow[2]), range = as.numeric(maprow[3]))
  rowix <- rowix + 1
}

t <- bind_rows(row_list) #Combine all individual rows into a single tibble. (Better performance than adding rows iteratively.)
t$source_to <- t$source + t$range - 1
t$increment <- t$destination - t$source 

#make function to map backwards from Location to Seed.
#check if this Seed is in a valid range.
#call this function from 0 up to fins the lowest location with a valid seed.

map_one_rev<- function(input_val, level_in){
  maprow <- t |> filter(level == level_in & destination <= input_val & (destination + range - 1) >= input_val)
  if(nrow(maprow) == 0) return(input_val)
  if(nrow(maprow) > 1) print("ERROR : multiple map values found")
  return( input_val + ( maprow[[1,'source']] -  maprow[[1,'destination']] ))
}  

for(i in seq(6472000, 6473001, by = 1)){
  seed <- i |> map_one_rev(7) |> map_one_rev(6) |> map_one_rev(5) |> map_one_rev(4) |> map_one_rev(3) |> map_one_rev(2) |> map_one_rev(1)
  for(j in 1:length(seed_from)){
    if(seed >= seed_from[j] & seed <= seed_to[j]){
      print(i)
      stop()
    }
  }
}

#Stepping by 1000 : 6 473 001
#Check 6472000 to 6473001 => 6472060 CORRECT!
