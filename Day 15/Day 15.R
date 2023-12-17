#Day 15 : Find sum of initiation sequence
library(tidyverse)

input <- readLines("Day 15/test input.txt")   
#input <- readLines("Day 15/input.txt") 

input <- str_split_1(input, ',')

acc <- 0
for(i in 1:length(input)){
  h <- hash(input[i])
  print(h)
  acc <- acc + h
}
acc

#Calculate the hash
hash <- function(string){
  h <- 0
  s <- str_split_1(string, '')
  for(i in 1:length(s)){
    h <- h + utf8ToInt(s[i]) 
    h <- h * 17
    h <- h  %% 256
  }
  return(h)
}


######################### Part 2 ###########################
#What is the focusing power of the resulting lens configuration?

# operation - : 
input <- readLines("Day 15/test input.txt")  
input <- readLines("Day 15/input.txt") 
input <- str_split_1(input, ',')

boxes <- list()
for(i in 1:257) boxes[[i]] <- vector(mode="integer")

#Process instructions : Add/replace/remove lenses from boxes.
for(i in 1:length(input)){
  
  #get components
  s     <- input[i]
  pos   <- str_locate(s, "-|=")[1,1]
  op    <- str_sub(s, pos, pos)
  label <- str_sub(s, 1, pos-1)
  boxno <- hash(label) + 1
  box   <- boxes[[boxno]]

  if(op == "=") {
    #operation is "=" : replace the existing lens with the given label with the new focal_length. If one does not exist, add the lens at the end of the box.
    focal_length <- strtoi(str_sub(s, pos+1, -1))
    lens <- c(focal_length)
    names(lens) <- label

    if(is.na(box[label]))  boxes[[boxno]] <- c(boxes[[boxno]], lens) else boxes[[boxno]][label] <- focal_length
  } else {
    
    #operation is minus : remove the lens with the given label from the box if it exists)
    boxes[[boxno]] <- boxes[[boxno]][!names(boxes[[boxno]]) == label]
  }
}

#Add up the focusing power of all of the lenses.
acc_fp <- 0
for(i in 1:length(boxes)){
  for(j in seq_along(boxes[[i]])){
    acc_fp <- acc_fp + i * j * boxes[[i]][j]
  }
}
acc_fp  #271384  : Correct!
