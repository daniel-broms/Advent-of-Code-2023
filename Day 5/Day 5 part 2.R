#Day 5 : Find lowest location.
library(tidyverse)

#Split into the two sets of numbers
input <- readLines("Day 5/test input.txt")   
#input <- readLines("Day 5/input.txt") 

#Extract first line (seeds) to a vector.
seeds <- input[1] |> str_sub(8,-1) |> str_split_1(' ') |> as.numeric()
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

#Make a function to map an input_val to an output for a given maptype using the table t.
map_one <- function(input_val, maptype_in){
  #Find the maprow for the matching maptype and input value
  
  maprow <- t |> filter(maptype == maptype_in & source <= input_val & (source + range - 1) >= input_val)
  if(nrow(maprow) == 0) return(input_val)
  if(nrow(maprow) > 1) print("ERROR : multiple map values found")
  
  #If one map exists : use it to return a value
  return( input_val + ( maprow[[1,'destination']] -  maprow[[1,'source']] ))

}
map_one_v <- Vectorize(map_one)

#Chain together all maps from soil to location.
map_all <- function(s){
  s |> map_one_v('seed-to-soil') |> map_one_v('soil-to-fertilizer') |> map_one_v('fertilizer-to-water') |> map_one_v('water-to-light') |> map_one_v('light-to-temperature') |> map_one_v('temperature-to-humidity') |> map_one_v('humidity-to-location') 
}
  

location <- seeds |> map_one_v('seed-to-soil') |> map_one_v('soil-to-fertilizer') |> map_one_v('fertilizer-to-water') |> map_one_v('water-to-light') |> map_one_v('light-to-temperature') |> map_one_v('temperature-to-humidity') |> map_one_v('humidity-to-location') 
min(location) #331445006

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


#make function to map backwards from Location to Seed.
#check if this Seed is in a valid range.
#call this function from 0 up to fins the lowest location with a valid seed.

map_one_rev<- function(input_val, level_in){
  maprow <- t |> filter(level == level_in & destination <= input_val & (destination + range - 1) >= input_val)
  if(nrow(maprow) == 0) return(input_val)
  if(nrow(maprow) > 1) print("ERROR : multiple map values found")
  return( input_val + ( maprow[[1,'source']] -  maprow[[1,'destination']] ))
}  

for(i in seq(1, 115202033, by = 1)){
  seed <- i |> map_one_rev(7) |> map_one_rev(6) |> map_one_rev(5) |> map_one_rev(4) |> map_one_rev(3) |> map_one_rev(2) |> map_one_rev(1)
  for(j in 1:length(seed_from)){
    if(seed >= seed_from[j] & seed <= seed_to[j]){
      print(i)
      stop()
    }
  }
}

#115 202 033 That's not the right answer; your answer is too high.

1.16e+08 (with step 1 000 000)
115300001  (with step 100 000) : I is between 115300001 and 115200001

115202101

115202033
