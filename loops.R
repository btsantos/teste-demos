die <- c(1, 2, 3, 4, 5, 6)
rolls <- expand.grid(die, die) #You can use expand.grid with more than two vectors if you like.
rolls$value <- rolls$Var1 + rolls$Var2 #determine the value of each roll

prob <- c("1"=1/8,"2"=1/8,"3"=1/8,"4"=1/8,"5"=1/8,"6"=3/8)
rolls$prob1 <- prob[rolls$Var1] #probabilities of rolling the values in Var1
rolls$prob2 <- prob[rolls$Var2] #probabilities of rolling the values in Var2
rolls$prob <- rolls$prob1 * rolls$prob2 #alculate the probability of rolling each combination by multiplying prob1 by prob2
sum(rolls$value * rolls$prob) #calculate the expected - expected value will be the summation of the dice values multiplied by the dice probabilities
##the expected value of rolling two loaded dice is 8.25. If you rolled a pair of loaded
##dice an infinite number of times, the average sum would be 8.25


############################################################################
#testing THE SLOT 3 wheels .... using loops
############################################################################
wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
##add the argument stringsAsFactors = FALSE to your expand.grid call; otherwise, expand.grid will save the combinations as factors, an unfortunate choice that will disrupt the score function
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)
prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3
sum(combos$prob) #sum of the probabilities is one, which suggests that our math is correct

##For example, we can calculate the prize for the first row of combos like this:
#symbols <- c(combos[1, 1], combos[1, 2], combos[1, 3]) ## "DD" "DD" "DD"
#score(symbols) ## 800

##for loop to calculate the prize for each row in combos - for score_no_wdd
combos$prize <- NA
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score_no_wdd(symbols)
}

#The expected value is the sum of combos$prize weighted by combos$prob. This is also the payout rate of the slot machine:
sum(combos$prize * combos$prob) ## 0.538014

##for loop to calculate the prize for each row in combos - for score_no_wdd
combos$prize <- NA
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}

#The expected value is the sum of combos$prize weighted by combos$prob. This is also the payout rate of the slot machine:
sum(combos$prize * combos$prob) ## 0.934356

############################################################################
#playing  SLOT 3 wheels .... 
###################   While - condition remains TRUE.    ###################
plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  while (cash > 0) {
    cash <- cash - 1 + play()
    n <- n + 1
  }
  n
}
plays_till_broke(100)
###################  repeat - until hitting Escape or encounter break.  ###################
plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  repeat {
    cash <- cash - 1 + play()
    n <- n + 1
    if (cash <= 0) {
      break
    }
  }
  n
}
plays_till_broke(100)
