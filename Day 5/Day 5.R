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

#Strategy: Find the "breakpoints" for seed values so that we can create "seed bins" : Rages of seed values which map to the same location.
#Begin with the input seed ranges (55-67, 79-92).
#make a function which maps input rages to output ranges for one maptype. (one input range may result in multiple output ranges.)
#Chain these functions together to map initial seed ranges to final location ranges. Find the location range with the lowest start value => the final answer.

input <- readLines("Day 5/test input.txt")   
#input <- readLines("Day 5/input.txt") 

#Store input seed ranges a a list with a list entry for each range.
seeds <- input[1] |> str_sub(8,-1) |> str_split_1(' ') |> as.numeric()
seed_ranges <- list()
for(i in 1:(length(seeds)/2)){
  print(i)
  seed_ranges[[i]] <- c(from=seeds[(i-1)*2 + 1], to=seeds[(i-1)*2 + 1] + seeds[(i-1)*2 + 2] -1)
}


#Function to map an input list of ranges to an output list of ranges for a given level.
map_ranges <- function(rangelist, level){
  
  #Begin by slitting input ranges where required. If an input range contains a start or end point of any mapping at this level : Split the input range at that point into two ranges.
  # from_split_points . Split if > Start and <= End.
  # to_split_points :   Split if >= Start and < End.
  from_split_points <- t |> filter(level == level) |> pull(source) 
  to_split_points   <- t |> filter(level == level) |> pull(source_to)
  
  #Then transform these (split) input ranges to output ranges simply by mapping the start/end points.
  
}

