#Exercise 8 Quinton Hayre 
setwd("/Users/Quintonhayre/Desktop/R_Biocomp/Biocomp-Exercise08/")
library(ggplot2)
##1
#Using the score-by-score information from this game summarized in “UWvMSU_1-22-13.txt” generate /
# a graph similar to the one I show above. Don’t worry about how pretty your graph is. /
#Focus more on the control structures required in your script used to generate the plot.

#You’ll want to generate a matrix or dataframe with a cumulative score for each team /
#whenever either team scores.
df = read.table(file = "UWvMSU_1-22-13.txt", sep = "\t", header= TRUE, stringsAsFactors = FALSE)
  #Initials
df$UWscore = 0
df$MSUscore = 0
totalUW = 0 
totalMSU = 0 

for (i in 1:nrow(df)){
  if (df$team[i] == "UW"){
    if (i == 1){
      totalUW = df$score[i]
      df$UWscore[i] = totalUW
      df$MSUscore[i] = totalMSU
    }else{
      totalUW = df$score[i] + totalUW
      df$UWscore[i] = totalUW
      df$MSUscore[i] = totalMSU
    }
  }#End of if1 
  else{
    if (i == 1){
      totalMSU = df$score[i]
      df$MSUscore[i] = totalMSU
      df$UWscore[i] = totalUW
    }else{
      totalMSU = totalMSU + df$score[i]
      df$MSUscore[i] = totalMSU
      df$UWscore[i] = totalUW
    }
  }#End of if2 
}#end of i 

#Plotting 
ggplot(data=df, aes(x= time)) + geom_line(aes(y = UWscore), color = "red") + 
  geom_line(aes(y = MSUscore), color = "green") + xlab("Time (minutes)") + ylab("Score") + 
  labs(title = "MSU (green) vs UW (red)", subtitle = "1/22/13") + geom_vline(xintercept = 20)
  #Question: Can I use GGplot and is it okay if I don't have the time be by Quarter 



###2
#Write a game called “guess my number”. The computer will generate a random number between 1 and 100. 
#The user types in a number and the computer replies “lower” if the random number is lower than the 
#guess, “higher” if the random number is higher, and “correct!” if the guess is correct. 
#The player can continue guessing up to 10 times.

#Random number 

print("Guess an integer 1 to 100 by running: guess_my_number(guess #)")

guess_my_number = function (x){
  computer = sample(1:100, 1)
  attempts = 1
  while (x != computer){
    if (attempts > 9){
      print("You have used all of your guesses. The number is: ")
      print(computer)
      break
    }
    else if (x > computer){
      print("Lower")
      attempts = attempts + 1 
      x = as.integer(readline(prompt="Enter a new integer: "))
    } else if (x < computer){
      print("Higher")
      attempts = attempts + 1
      x = as.integer(readline(prompt="Enter a new integer: "))
    } 
  }
  while (x == computer){
    print("You have guessed correctly!")
    break
  }
}


#Example of running the code:
guess_my_number(50)





















