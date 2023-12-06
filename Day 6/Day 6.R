#Find the ways to win the race. 
#We can use a quadratic equation in the form "x*x - Time*x + Distance = 0" to find the minimum/maximum time limits.
#The solution is the range of integers which fit inside this range.


quad <- function(a, b, c)
{
  a <- as.complex(a)
  answer <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
              (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
  if(all(Im(answer) == 0)) answer <- Re(answer)
  if(answer[1] == answer[2]) return(answer[1])
  answer
}

#Test
quad(1, -7, 9) #2 to 5
5-2+1 #4
quad(1, -15, 40) #4 to 11
11-4+1 #8
quad(1, -30, 200) #11 to 19
19-11+1 #9

4*8*9 = 288


#Real
quad(1, -61, 430) #9-52 => 
52-9+1 #44 ways
quad(1, -67, 1036) #25 to 42
42-25+1 #18
quad(1, -75, 1307) #28 to 47
47-28+1 #20
quad(1, -71, 1150) #26 to 45
45-26+1 #20

44*18*20*20 = 316800  #Correct 

###########################################################################
#Part 2:
#test input
quad(1, -71530, 940200) # 14 to 71516
71516-14 + 1 = 71503

#Real : 
time = 61677571
distance = 430103613071150
quad(1, -61677571, 430103613071150) # 8014960 to 53662611 (excluding borders)

53662611 - 8014960 + 1  = 45647652 #That's not the right answer; your answer is too low. (I actually think this is the correct answer!)
53662612 - 8014959 + 1  = 45647654 #I get correct for this but it is not correct : We are including the borders which we should not!!



