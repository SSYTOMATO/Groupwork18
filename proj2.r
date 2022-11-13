# Group 18
# Member: Shangyun Sun(S2330859); Ziyi Gu(S2307232); Mengyao Zhang(S2049018).

# Github repo: https://github.com/SSYTOMATO/Groupwork18.git

# Shangyun Sun: wrote the function Unit, Pone and corresponding comments;
#               visualized the probability.
# Ziyi Gu: wrote part of the function dloop and corresponding comments;
#          provided all examples' codes and analysis.
# Mengyao Zhang: wrote the function Pall and part of the function dloop;
#                added the overview comment and comments for function Pall.
# Each of us double-checked and optimized others' codes.
# Everyone undertook roughly 1/3 of the work.

#-------------------------------------------------------------------------------

# This code tries to solve a particular problem by three different strategies,
# estimate the probability of success of each strategy by stochastic simulation
# and do some investigation of one strategy which performs surprisingly better.

# The problem: A room contains 2n boxes numbered from 1 to 2n. There are 2n
#              cards, each printed with a unique number from 1 to 2n and
#              randomly placed one in each box. 2n prisoners numbered from 1 to
#              2n need to find the card with their number on it by opening a 
#              maximum on n boxes. After each prisoner’s go, the room is set to
#              its original state and communication is not allowed. If all
#              prisoners succeed in finding their number, then they all go free.

# Strategy 1: The prisoner starts at the box with their number on it, opens it
#             and find the card with number i. If i is not the prisoner number,
#             they go to box number i, open it and repeat the process until they
#             have either found the card with their number on it, or opened n 
#             boxes without finding it.
# Strategy 2: As strategy 1, but starting from a randomly selected box.
# Strategy 3: They open n boxes at random, checking each card for their number.

#-------------------------------------------------------------------------------

# a pre function

Unit <- function(n,k,first,box){
  
# Function Unit is a pre-function, which is used to determine whether one
# prisoner can succeed within n times.

# Input: n (the maximum number of boxes the prisoner can open)
#        k (the index of the prisoner)
#        first (the index of the first box the prisoner chooses)
#        box (the integer vector simulated randomly, representing the number on
#             the card contained in boxes numbered from 1 to 2n)
# Output: an integer which is marked as 1 when successful, otherwise marked as 0
  
  box_index <- first  # the index of the box to be opened
  success <- 0  # 1 represents success, 0 represents failure
  
  for (i in 1:n){  # loop for n times, since maximum number of opened box is n
    if (k != box[box_index]){     # if card number is not the prisoner's index k
      box_index <- box[box_index] # goes to the box numbered same as card number
      
    } else {
      success <- 1  # when the card number equals to prisoner's index, the
                    # experiment is successful; set success as 1, and stop loop
      break
    }
  }
  success
}

#-------------------------------------------------------------------------------

# function Pone

Pone <- function(n,k,strategy,nreps=10000){
  
# Function pone is used to estimate probability of single prisoner's success in  
# each strategy by simulating nreps experiments and calculating the proportion
# of the results that the prisoner succeeds.
  
# Input: n (the maximum number of boxes one prisoner can open, also, 2n is the
#           number of prisoners, boxes and cards)
#        k (the index of the prisoner)
#        strategy (take values of 1, 2, 3,
#                corresponding to strategies stated above)
#        nreps (the number of simulations, with default value 10000)
# Output: a float which is the probability that 1 prisoner can find his or her
#         number within maximum n boxes opened
  
  success <- 0
    
    # strategy 1
  if (strategy == 1){         # decide to use which strategy
    for (rep in 1:nreps){     # loop for nreps experiments
      box <- sample(2*n,2*n)  # random simulate the cards' number in boxes
      success <- success + Unit(n,k,k,box)  # sum the number of success
    }
  }
    
    # strategy 2
  else if (strategy == 2){
    for (rep in 1:nreps){
      box <- sample(2*n,2*n)
      first <- sample(2*n,1)  # start the experiment with a random selected box
      success <- success + Unit(n,k,first,box)
    }
  }
  
    # strategy 3
  else if(strategy == 3){
    for (rep in 1:nreps){
      choice <-sample(2*n,n)  # open n boxes at random
      success <- success + k %in% choice  # if k is in those opened boxes' card
                                          # numbers, the experiment succeeds
    }
  } 
  success/nreps  # success probability = (the number of simulations with the
                 # result of success) / (the total number of simulations)
}

#-------------------------------------------------------------------------------

# function Pall

Pall <- function(n,strategy,nreps=10000){

# Function Pall is used to estimate the probability of all prisoners finding
# their number by simulating nreps experiments and calculating the proportion
# of the results that all prisoners succeed.
  
# Input: n (the maximum number of boxes one prisoner can open, also, 2n is
#           the number of prisoners, boxes and cards)
#        strategy (take values of 1, 2, 3,
#                  corresponding to strategies stated above)
#        nreps (the number of simulations, with default value 10000)
# Output: a float which is the probability of all prisoners finding their number
  
  success_num <- 0  # the times of simulations with result of success
  
  # strategy 1
  if (strategy == 1){
    for (rep in 1:nreps){     # loop for every simulation
      success <- 1            # 1 represents success, 0 represents failure
      box <- sample(2*n,2*n)  # place 2n card randomly
      for (k in 1:(2*n)){     # loop for every prisoner
        success <- success * Unit(n,k,k,box)  # variable success will remain 1
                                              # if every prisoner successes, but
                                              # will be 0 if any one fails
        if (success == 0){break}  # fail, no need to continue
      }
    success_num <- success_num + success  # if success, success_num will add 1
    }
  }
  
  # strategy 2
  else if (strategy == 2){      # similar as the strategy 1
    for (rep in 1:nreps){
      success <- 1
      box <- sample(2*n,2*n)
      for (k in 1:(2*n)){
        first <- sample(2*n,1)  # randomly choose the first box
        success <- success * Unit(n,k,first,box)
        if (success == 0){break}
      }
      success_num <- success_num + success  
    }
  }
  
  # strategy 3
  else {
    for (rep in 1:nreps){
      success <- 1
      for (k in 1:(2*n)){
        choice <-sample(2*n,n)  # randomly choose half of boxes 
        success <- success * k %in% choice # whether the prisoner's number is in
                                           # the boxes he chooses
        if (success == 0){break}
      }
      success_num <- success_num + success
    }
  } 
  
  success_num/nreps  # success probability = (the number of simulations with
                     # result of success) / (the total number of simulations) 
}

#-------------------------------------------------------------------------------

# example codes for n = 5 and 50 under three different strategies
# results for each n is shown in a matrix
# row i is the result of strategy i
# column 1 is individual probability and column 2 is joint success probability

# n = 5
matrix_n5 <- matrix(0,3,2)
for (strategy in 1:3){
  matrix_n5[strategy,1] <- Pone(5,1,strategy)
  matrix_n5[strategy,2] <- Pall(5,strategy)
}
matrix_n5

# n = 50
matrix_n50 <- matrix(0,3,2)
for (strategy in 1:3){
  matrix_n50[strategy,1] <- Pone(50,1,strategy)
  matrix_n50[strategy,2] <- Pall(50,strategy)
}
matrix_n50

# Remarks on the result:
# The difference among individual success probabilities is not large.
# However, there is huge difference in joint probability. 
# Joint success probability of strategy 1 is surprisingly high, more than 0.3,
# but Joint success probabilities of strategy 2&3 are close to 0.
# The difference is more obvious when n becomes larger.

#-------------------------------------------------------------------------------

# function dloop

dloop <- function(n,nreps=10000){
  
# Function dloop is used to estimate probability of each loop length from 1
# to 2n occurring at least once in a random shuffling of cards to boxes
  
# Input: n (2n is the number of boxes and cards)
#        nreps (the number of simulations, with default value 10000)
# Output: a 2n vector of probability of each loop length occurring at least once
  
  freq <- rep(0,2*n)    # to store the frequency of each loop length occurring 
  for (reps in 1:nreps){
    u <- sample(2*n,2*n)  # simulate cards number in boxes
    u_index <- 1:(2*n)    # a vector of box number
    len_list <- c()       # a vector to store the loop length
    while (length(u_index) != 0){  # while there are still boxes to be opened
      first <- u_index[1] # the target number needs to be found to end the loop
      k <- first          # the number of the box to be opened
      ring <- k           # to store all numbers in one loop, starting with k
      length <- 1         # loop length with initial value 1
      while (u[k] != first){  # if card number in box k != target number
        k <- u[k]             # set box number equal to the card number 
        length <- length + 1  # loop length increases 1
        ring <- c(ring,k)     # add the new box number to the ring
      }
      u_index <- u_index[-which(u_index %in% ring)] # remove the box number used
      len_list <- c(len_list,length)                # store this loop length
    }
    # add 1 to the frequency of corresponding loop length
    freq[which(1:(2*n) %in% len_list)] <- freq[which(1:(2*n) %in% len_list)] + 1
  }
  freq/nreps  # calculate the probability
}

#-------------------------------------------------------------------------------

# example code for n = 50
trial <- dloop(50)

# calculate the probability that no loop is longer than 50 in one experiment
# Loop with length greater than 50 only occurs at most once in each simulation,
# so trial[51:100] are exactly the probability of each loop length occurring. 
# And the occurrence of each loop length from 51 to 100 is mutually exclusive.
prob_success <- 1 - sum(trial[51:100])
prob_success  
# this probability is just the joint success probability of strategy 1

# visualization
x<-seq(1,100)
barplot(trial,names=x,xlab = 'Loop length',ylab = 'Probability',ylim = c(0,0.7))
lines(x,trial,type="l",col="blue")
# Analysis:
# Intuitively, the probability of loop length exceeding 50 is very small,
# which allows us to get a relatively high probability of success.
