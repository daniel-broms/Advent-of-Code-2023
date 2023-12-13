#Day 12 : figure out how many different arrangements of operational and broken springs fit the given criteria in each row. 
# Sum the number of possible arrangements. Spings length=max 20. (between 10 and 20). Groups length is 2-6. 1000 groups in real input.
library(tidyverse)
library(memoise)

input <- readLines("Day 12/test input.txt")   
input <- readLines("Day 12/input.txt") 

t <- tibble(input)
t <- separate(t, col=input, into=c('springs', 'groups') , sep= ' ')    

#Part 2 : Unfold the input : 
unfold <- function(x, times, s){
  r <- x
  for(i in 1:(times-1)){
    r <- paste(r, s, x, sep="")
  }
  r
}
t$springs <- unfold(t$springs, 5, "?")
t$groups  <- unfold(t$groups,  5, ",")


#Idea 2 : Go group by group : find all possible positions of group one. For each of these: recursively find the possible positions of the rest. Only include the results where all groups can fit in.
#remove the matched part of the string (except the last ".") The remaining groups need to fit into the remaining string, after the previous group match.

#Note: 1 actually means ".#."  since it needs to be isolated! 
#      2 actually means ".##." since it needs to be isolated! Etc.

options(scipen=999)
main()  # 525152 for test input (Correct!) but it took very long time. many repetitive calls with the same arguments : Memoize the function!

#main function : calculate all rows in input.
main <- function(){
  acc <- 0
  for(i in 1:nrow(t)){
    cr <- paste(".", t[[i,1]], ".", sep='')                            #Add a point before and after the string to avoid edge cases.
    groups <- t[[i,2]] |> str_split_1(',') |> strtoi()                 #convert groups to a vector.
    arr <-  get_arrangements_m(cr, groups)
    print(c(i,arr))
    acc <- acc + arr
  }
  acc
}

#Function to calculate the number of possible arrangements of "groups" in condition record cr
get_arrangements <- function(cr, groups){
  
  #print(c(cr, paste(groups, collapse=",")))
  
  #TODO : find the mot compact way to arrange all groups after the current group into cr. This tells us which portion of cr is available for the current group. 
  #Only check the remaining cr for matches. (This optimization was not needed after meoization was applied.)
  
  #Get the possible arrangements for the first group, test each of these. 
  matches <- str_locate_all(cr, get_pattern(groups[1]))
  matches <- matches[[1]][,"start"]                           #Extract starting positions only
  
  #FIX : delete any matches which start after the first #! We cannot leave unused "#" behind.
  firsthashpos <- str_locate(cr, "#")[1]
  if(!is.na(firsthashpos)) matches <- matches[matches < firsthashpos]
  
  #If no matches : Stop here, return 0 = no arrangements are possible.
  if(length(matches) == 0) return(0) 
  
  #If this is the last group : return the number of matches which do not leave any remaning #.
  if(length(groups) == 1){
    acc <- 0
    for(i in 1:length(matches)){
      remaining_string <- paste(str_sub(cr, matches[i] + groups[1] + 2, -1))
      if(!str_detect(remaining_string, "#")) acc <- acc + 1
    }
    return(acc)
  } 
  
  #otherwise go though each match and call get_arrangements() with the remaining string and groups

  acc <- 0
  for(i in 1:length(matches)){
    remaining_string <- paste(".", str_sub(cr, matches[i] + groups[1] + 2, -1), sep="")
    s <- str_split_1(remaining_string, "")
    
    #Shortcut : Do not consider paths where sum of # and ? is less than sum of remaining broken springs, we know this will not work.
    #           Do not consider paths where sum (#) is greater than sum of remaining broken springs, we know this will not work.
    if(sum(s == "#") <= sum(groups[-1]) & sum(s == "#" | s=="?") >= sum(groups[-1]))  acc <- acc + get_arrangements_m(remaining_string, groups[-1])
  }
  return(acc)
}
#memoize get_arrangements:
get_arrangements_m <- memoise(get_arrangements)

#Get the regex to identify all possible matches for n broken springs (#), including overlapping matches
get_pattern <- function(n){
  paste("(?=(",                       #look-ahead
        "(\\.|\\?)",                  #initial space
        str_dup("(#|\\?)", n),        #given nr of broken springs
        "(\\.|\\?)",                  #trailing space
        "))",                         #close look-ahead
        sep="")   
}

