
########################## Part 2 #######################################
#This time, you need to find the gear ratio of every gear and add them all up so that the engineer can figure out which gear needs to be replaced.
#A gear is any * symbol adjacent to exactly two part numbers.

#Detect all numbers in the surrounding eight points. 
#for each point, if is is a digit find the whole digit and return it, together with xpos and ypos.
#Log all found number/xpos/ypos in a tibble.
#Elimiate duplicates. If we have two numbers (rows) left, log their product in the result vector.

#Read input to a character matrix
input <- scan(file="Day 3/test input.txt", what=character()) 
input <- scan(file="Day 3/input.txt", what=character()) 

m <- str_split_fixed(input, "", str_length(input[1]))

gears <- str_locate_all(input, '\\*')
ratios <- vector(mode="integer")


#Check each number - does it have any adjacent symbol?
for(line in 1:length(gears)){
  if(nrow(gears[[line]]) > 0){
    for(n in 1 : nrow(gears[[line]])){
      xpos <- gears[[line]][n,1]

      #find all unique numbers adjacent to this gear at line, xpos.
      t <- tibble(number =numeric(), row=numeric(), col=numeric())  
      t <- rbind(t, findnumber(line, xpos-1))  #look left
      t <- rbind(t, findnumber(line, xpos+1))  #look right
      for(x in (xpos - 1) : (xpos + 1)) {
        t <- rbind(t, findnumber(line - 1, x))  #look above
        t <- rbind(t, findnumber(line + 1, x))  #look below
      }
      names(t) <- c("number", "row", "col")     #for some reason names are lost
      t <- t %>% group_by(number, row, col) %>% summarise()   #remove duplicates
      
      #if we have exactly two numbers : Add product to ratios
      if(nrow(t) == 2){ 
        ratios <- c(ratios, (t[[1,"number"]] * t[[2, "number"]]))
      }
    }
  }
}
sum(ratios) #show the final answer

#Look for a complete number at the given row/col. Return c() (an empty vector)if no number was found.
findnumber <- function(row, col){
  if(row < 1 | row > nrow(m) | col < 1 | col > ncol(m)) return(c())
  
  if(str_detect(m[row,col], "\\d") ){
    startpos <- col
    while(startpos > 1     && str_detect(m[row,startpos - 1], "\\d")) {startpos <- startpos - 1} #go left while there are digits to find startpos
    
    endpos <- col
    while(endpos < ncol(m) && str_detect(m[row,endpos + 1], "\\d")) {endpos <- endpos + 1} #go left while there are digits to find startpos
    
    return(c(number = strtoi( str_sub(input[row], startpos, endpos)), row = row, col = startpos))  #return the complete number and its row and startcol
    
  }
  else return(c())
}


