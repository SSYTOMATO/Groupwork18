# Group 18
# Member: Shangyun Sun(S2330859); Ziyi Gu(S2307232); Mengyao Zhang(S2049018).

# Github repo: https://github.com/SSYTOMATO/Groupwork18.git

# Shangyun Sun: wrote the function Unit, Pone and corresponding comments;
#               visualized the probability.
# Ziyi Gu: wrote part of the function dloop and corresponding comments;
#          provided all examples' codes and analysis.
# Mengyao Zhang: wrote the function Pall and part of the function dloop;
#                wrote the overview comment and comments for function Pall.
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
#             and find the card with number k. If k is not their prisoner number,
#             they go to box number k, open it and repeat the process until they
#             have either found the card with their number on it, or opened n 
#             boxes without finding it.
# Strategy 2: As strategy 1, but starting from a randomly selected box.
# Strategy 3: They open n boxes at random, checking each card for their number.

#-------------------------------------------------------------------------------

# 
# a pre function
Unit <- function(n,k,first,box){
# Function unit is a pre-function for other functions later, which is used to 
# determine whether prisoners can successful within n times.
# 
# Input: n (the maximum number that boxes can be opened)
#        k (the index of the first prisoner)
#        first (the index of the first box we chosen)
#        box (the integer simulated randomly, which represents the number on the
#        card contained in each box)
# Output: an integer which is marked as 1 when it success, otherwise marked as 0
  
  box_index <- first
  success <- 0 # 1 represents success, 0 represents failure
  for (i in 1:n){ # loop for n times, because maximum number of opened box is n
    if (k != box[box_index]){ # if k is not the prisoner's number,
      box_index <- box[box_index]  # prisoner goes to box number k, 
                                   # open it and repeat the process.
      
    } else {
      success <- 1 # when the card number equals to prisoner's number, 
      # the experiment is successful, we set success as 1, and stop loop.
      break
    }
  }
  success
}

#-------------------------------------------------------------------------------


# function Pone
Pone <- function(n,k,strategy,nreps=10000){
# Function pone is used to estimate probability of single prisoners success in  
# each strategy within nreps experiments. 
# Input: n (used to calculate the number of prisoners (2*n) and box (2*n))
#        k (the index of the first prisoner)
#        strategy (can equal to 1,2,3, represents 3 strategies respectively)
#        nreps (the amount of experiments, which is defaulted to 10000)
# Output: a float which is the probability 1 prisoner can find his or her number
  success <- 0
    
    # strategy 1
  if (strategy == 1){ # decide to use which stategy
    for (rep in 1:nreps){ # loop for nreps experiments
      box <- sample(2*n,2*n) # random simulate the card's number in boxes
      success <- success + Unit(n,k,k,box) # calculate the number of success
    }
  }
    
    # strategy 2
  else if (strategy == 2){
    for (rep in 1:nreps){
      box <- sample(2*n,2*n)
      first <- sample(2*n,1) # start the experiment with random selected box
      success <- success + Unit(n,k,first,box)
    }
  }
  
    # strategy 3
  else if(strategy == 3){
    for (rep in 1:nreps){
      choice <-sample(2*n,n) # open n boxes at random
      success <- success + k %in% choice # if k is in those opened box's 
      # card number, we think experiment success.
    }
  } 
  success/nreps # success probability = (the number of simulations with
                # result of success) / (the total number of simulations)
}

#-------------------------------------------------------------------------------

Pall <- function(n,strategy,nreps=10000){
  
  # Estimate the probability of all prisoners finding their number
  # By stochastic simulation
  
  # Input: n (the maximum number of boxes one prisoner can open, also, 2n is
  #           the number of prisoners, boxes and cards)
  #        strategy (take values of 1, 2, 3,
  #                  corresponding to strategies stated above)
  #        nreps (the number of replicate simulations to run in order to
  #               estimate the probability, with default value 10000)
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

# Example codes for n = 5 and 50 with three different strategies
# results for each n is shown in a matrix
# row i is the result of strategy i
# column 1 is individual probability and column 2 is joint success probability

matrix_n5 <- matrix(0,3,2)
for (strategy in 1:3){
  matrix_n5[strategy,1] <- Pone(5,1,strategy)
  matrix_n5[strategy,2] <- Pall(5,strategy)
}
matrix_n5

# n = 50 matrix, row-strategy, column-indi/joint
matrix_n50 <- matrix(0,3,2)
for (strategy in 1:3){
  matrix_n50[strategy,1] <- Pone(50,1,strategy)
  matrix_n50[strategy,2] <- Pall(50,strategy)
}
matrix_n50

# Remarks on the result
# Individual success probability of each strategy range from about 0.4 to 0.5.
# The difference among each probability is not large.
# However, there is huge difference in joint probability. 
# Joint success probability of strategy 1 is surprisingly high, with about 0.3,
# but Joint success probability of strategy 2&3 are close to 0.
# The result is more obvious when n becomes larger.


#-------------------------------------------------------------------------------

# a new dloop function from zmy

dloop <- function(n,nreps=10000){
  
# Function dloop is used to estimate probability of each loop length from 1
# to 2n occurring at least once in a random shuffling of cards to boxes
  
# Input: n (2n is used to calculate the number of boxes, loop length and 
#           number of frequency of each loop length)
#        nreps (the number of replicate simulations to run in order to 
#               estimate the probability, with default value 10000)
# Output: a 2n vector of probabilities of each loop length occurring at
#         least once in a random shuffling of cards to boxes
  
    freq <- rep(0,2*n)
  for (reps in 1:nreps){
    u <- sample(2*n,2*n)
    u_index <- 1:(2*n)
    len_list <- c()
    while (length(u_index) != 0){
      first <- u_index[1]
      k <- first
      ring <- k
      length <- 1
      while (u[k] != first){
        k <- u[k]
        length <- length + 1
        ring <- c(ring,k)
      }
      u_index <- u_index[-which(u_index %in% ring)]
      len_list <- c(len_list,length)
    }
    freq[which(1:(2*n) %in% len_list)] <- freq[which(1:(2*n) %in% len_list)] + 1
  }
  freq/nreps
}

test <- dloop(50)
test
prob <- 1 - sum(test[51:100])
prob



# Q6
x<-seq(1,100)

barplot(dloop(50),names=x,xlab = 'Loop length',ylab = 'Probability',ylim = c(0,0.7))
lines(x,dloop(50),type="l",col="blue")
# Analysis:
# When we perform the experiment with n=50. We can see from the graph 
# that a loop of length 1 has a higher probability compared to the other lengths.
# And as the length increases, the probability decreases abruptly. 
# When the length of the loop exceeds 50, the probability tends to zero.
# This is keeping up with our rules that the maximum number of boxes 
# that can be opened is n=50, otherwise it is considered a failure. 
# Therefore the probability of success in a loop of these lengths should be 0.
