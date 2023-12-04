#Day 3 : find parts, sum the part numbers.
library(tidyverse)

#Read input to a character matrix
input <- scan(file="Day 3/test input.txt", what=character()) 
#input <- scan(file="Day 3/input.txt", what=character()) 

m <- str_split_fixed(input, "", str_length(input[1]))

#Find the positions of all numbers in the input as a list of position matrixes
numbers <- str_locate_all(input, '\\d+')
parts <- vector(mode="integer")

#Check each number - does it have any adjacent symbol?
for(line in 1:length(numbers)){
  if(nrow(numbers[[line]]) > 0){
    for(n in 1 : nrow(numbers[[line]])){
      startpos <- numbers[[line]][n,1]
      endpos   <- numbers[[line]][n,2]
      number <- str_sub(input[line], startpos, endpos )
      if(is_part(line, startpos, endpos)){parts <- c(parts, number)}  #Log the number as a valid part it passes part check
    }
  }
}
sum(strtoi(parts))  #27095 : Incorrect, too low!!

#Return true if this is a valid part, i.e. there exists an adjacent symbol.
#Scan all positions around the input, look for symbols. 
is_part <- function(line, startpos, endpos) {
  
  #left and right
  if(is_symbol(line, startpos-1) | is_symbol(line, endpos+1)) return(TRUE)
  
  #above and below
  for(xpos in (startpos - 1) : (endpos + 1) ){
    if(is_symbol(line-1, xpos) | is_symbol(line+1, xpos)) return(TRUE)
  }
  
  return(FALSE) #no symbol was found
}


#Return true if this position has a symbol. False if not (or if outside the matrix).
is_symbol <- function(row, col){
  #check if we ar inside the matrix.
  if(row < 1 | row > nrow(m) | col < 1 | col > ncol(m))
    return(FALSE)
  else {
    if(str_detect(m[row,col], "\\.|\\d"))
      return(FALSE)
    else
      return(TRUE)
  }
}
