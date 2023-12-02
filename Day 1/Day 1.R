###########################################################################################
# Day 1 :Find the calibration code by extraction the first and last digit from strings.
###########################################################################################
library(tidyverse)
library(stringi)

#Import text file lines to a character vector using readlines:
input <- readLines("Day 1/test input.txt")                          
input <- readLines("Day 1/input.txt")    

#for each string, find the position of the first and last character.
acc <- 0
for(i in 1:length(input)) {
  
  first <- str_extract(input[i], '\\d')
  last  <- str_extract(stri_reverse(input[i]), '\\d')
  number <- strtoi(str_c(first, last))
  print(number)
  acc <- acc + number
}
print(acc)  #53194


###################################################################################################################
#part 2: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".
#complication: Letters can overlap such as in "eightwo", so we cannot replace "two" with "2". Eight and Two need to be processed separately.
#Another complication: digits and "letter digits" can appar multiple times.

#Strategy: Find the min and max starting position of any of the searches : //d, 'one', 'two' etc.
#Also keep track of the ending position related to each start.
#Extract the min and max for each line. 
#convert from text to digit if len > 1.

#for each line: Create a tibble with columns StartPos (int), StopPos (int), sValue (string), iValue(int).

acc <- 0
for(i in 1:length(input)) {
  m <- matrix(ncol=2, nrow=1)           #Initialize an one-row two-column matrix. (a dummy row was needed to avoid a bug with single-row maxtrix.)
  
  test <- input[i]
  m <- checkpattern(m, test, '\\d')
  m <- checkpattern(m, test, 'one')
  m <- checkpattern(m, test, 'two')
  m <- checkpattern(m, test, 'three')
  m <- checkpattern(m, test, 'four')
  m <- checkpattern(m, test, 'five')
  m <- checkpattern(m, test, 'six')
  m <- checkpattern(m, test, 'seven')
  m <- checkpattern(m, test, 'eight')
  m <- checkpattern(m, test, 'nine')
  m <- m[order(m[,1],decreasing=FALSE),]  #Sort all found digits
  

  first <- parse_digit(str_sub(test, m[1,1], m[1,2]))                                     #gives error when matrix m has only one row
  last  <- parse_digit(str_sub(test, m[dim(m)[1]-1,1], m[dim(m)[1]-1,2]))
  
  cal_val <- strtoi(str_c(first, last ))
  acc <- acc + cal_val
}
acc  #54249


#Add matches found in s to matrix m
checkpattern <- function(m, s, pat){
  r <- str_locate_all(s, pat)
  r <- r[[1]]
  m <- rbind(m,r)
  m
}


parse_digit <- function(s){
  case_when(
    str_length(s) == 1 ~ s,
    s == "one"   ~ "1",
    s == "two"   ~ "2",
    s == "three" ~ "3",
    s == "four"  ~ "4",
    s == "five"  ~ "5",
    s == "six"   ~ "6",
    s == "seven" ~ "7",
    s == "eight" ~ "8",
    s == "nine"  ~ "9"
  )
}
