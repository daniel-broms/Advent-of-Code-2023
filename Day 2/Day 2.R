#Day 2 : Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. 
#What is the sum of the IDs of those games?

#Parse input to a table with GameID, Red, Green, Blue.
#Find all GameId which violate any of the rules (max 12 red, max 13 green, max 14 blue)
#Find the sum of GameId for the remaining games.

library(tidyverse)

#Import text file lines to a character vector using readlines:
input <- readLines("Day 2/test input.txt")                          
input <- readLines("Day 2/input.txt")    

#Parse each game to a tibble.
t <- tibble(GameId = numeric(), RoundId= numeric(), red= numeric(), green= numeric(), blue= numeric())
roundid <- 0

#Extract the game id from position 6 to position of #-1.
for(game in 1:length(input)){
  s <- input[game]
  pos <- str_locate(s, ':')[1]
  game_id <- strtoi(str_sub(s,6,pos - 1))
  s <- str_sub(s, pos + 1 , -1)
  
  #Split into rounds, process each round:
  rounds <- str_split(s, ';')[[1]]
  for(i in 1:length(rounds)){
    roundid <- roundid + 1
    round <- rounds[i]
    t[roundid, 'RoundId'] <- roundid
    t[roundid, 'GameId'] <- game_id

    #Split each round into draws (each draw is for one color in the current round).
    draws <- str_split(round,',')[[1]]
    for(j in 1:length(draws)){
      draw <- draws[j]                                #extract one draw, ex. " 12 blue"
      color <- str_extract(draw, 'red|blue|green')    #Find the color of this draw.
      value <- strtoi(str_trim(str_sub(draw,1,3)))    #Find the value of this draw.
      t[roundid, color] <- value                      #Log this draw to our results tibble.
    }
  }
}

#find games violating rules, find the remaining good games, sum the ID
bad_games <- t %>% filter(red > 12 | green > 13 | blue > 14) %>% select(GameId) %>% distinct(GameId)
goodgames <- setdiff(t %>% select(GameId) %>% distinct(GameId), bad_games)
sum(goodgames)

#######################################################################################################################
#Part 2 : For each game, find the minimum set of cubes that must have been present. What is the sum of the power (red*blue*green) of these sets?
#Summarize per game : What is the max of each color? Multiply these together and sum the results.

#Find max red/green/blue per game.
t2 <- t %>% group_by(GameId)  %>% summarize(maxred = max(red, na.rm = TRUE), maxgreen = max(green, na.rm = TRUE ), maxblue = max(blue, na.rm = TRUE))
sum(t2['maxred'] * t2['maxgreen'] * t2['maxblue'])
