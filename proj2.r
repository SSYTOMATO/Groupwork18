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
# Overview






#-------------------------------------------------------------------------------
# Function unit is a pre-function for other functions later. 
# Input of this function are n,k,first and box.
# 'n' is the maximum number that boxes can be opened. 'k' is the index of the 
# first prisoner. 'first' is the index of the first box we chosen.
# 'box' is the integer simulated randomly, which represents the number on the
# card contained in each box.
# This function is used to determine whether prisoners can successful within n
# times. If they success we define success as 1, otherwise we mark it as 0.
# 
# a pre function
Unit <- function(n,k,first,box){
  box_index <- first
  success <- 0
  for (i in 1:n){
    if (k != box[box_index]){
      box_index <- box[box_index]
    } else {
      success <- 1
      break
    }
  }
  success
}

# Function pone is used to estimate probability of single prisoners success in  
# each strategy within nreps experiments. 
# 'n' is used to calculate the number of prisoners (2*n) and box (2*n).
# 'k' is similar to the function Unit, which is the index of the first prisoner.
# 'strategy' can equal to 1,2,3, represents 3 strategies respectively.
# 'nreps' is the amount of experiments, which is defaulted to 10000.
# After we run this function we can get estimate success probability of
# those 3 strategies for individual prisoner.
# function Pone
Pone <- function(n,k,strategy,nreps=10000){
  
  success <- 0
    
    # strategy 1
  if (strategy == 1){
    for (rep in 1:nreps){
      box <- sample(2*n,2*n)
      success <- success + Unit(n,k,k,box)
    }
  }
    
    # strategy 2
  else if (strategy == 2){
    for (rep in 1:nreps){
      box <- sample(2*n,2*n)
      first <- sample(2*n,1)
      success <- success + Unit(n,k,first,box)
    }
  }
  
    # strategy 3
  else if(strategy == 3){
    for (rep in 1:nreps){
      choice <-sample(2*n,n)
      success <- success + k %in% choice
    }
  } 
  success/nreps
}


# function Pall
# Similar as function Pone, but consider joint probability, that is,
# in each simulation, find whether all prisoners can success.
# Simulate nreps times, calculate the probability.

Pall <- function(n,strategy,nreps=10000){
  
  success_num <- 0
  
  # strategy 1
  if (strategy == 1){
    
    for (rep in 1:nreps){
      success <- 1
      box <- sample(2*n,2*n)
      for (k in 1:(2*n)){
        success <- success * Unit(n,k,k,box)
        if (success == 0){break}
      }
    success_num <- success_num + success  
    }
  }
  
  # strategy 2
  else if (strategy == 2){
    
    for (rep in 1:nreps){
      success <- 1
      box <- sample(2*n,2*n)
      for (k in 1:(2*n)){
        first <- sample(2*n,1)
        success <- success * Unit(n,k,first,box)
        if (success == 0){break}
      }
      success_num <- success_num + success  
    }
  }
  
  # strategy 3
  #if(strategy == 3)
  else {
    for (rep in 1:nreps){
      success <- 1
      for (k in 1:(2*n)){
        choice <-sample(2*n,n)
        success <- success * k %in% choice
        if (success == 0){break}
      }
      success_num <- success_num + success
    }
  } 
  success_num/nreps
}


# n = 5 matrix, row-strategy, column-indi/joint
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

# Joint success probability of strategy 2&3 are close to 0 but
# joint success probability of strategy 1 is surprisingly high
# The result is more obvious when n becomes larger.


# dloop from gzy

dloop <- function(n,nreps){
  freq <- c(rep(0,2*n))
  for (rep in 1:nreps){
    u <- sample(2*n,2*n)
    u_index <- sample(2*n,1)
    for (i in 1:(2*n)){
      if (u_index != u[u_index]){
        u_index <- u[u_index]
      } else {
        freq[i] <- freq[i] + 1
        break
      }   
    }
  }
  prob <- freq/nreps
  print(prob)
}



# Q5 from ssy

dloop <- function(n,nreps){
  
  freq <- c(rep(0,2*n))
  u_index <- seq(1,2*n)
  for (rep in 1:nreps){
    u <- sample(2*n,2*n)
    k <- sample(u_index,1)
    start_value <- k
    length <- 1
    while (k != u[k]) {
      k <- u[k]
      if(start_value == u[k]){break}
      length <- length+1}
    freq[length] <- freq[length]+1
    rep <- rep+1
  }
  prob <- freq/nreps
  print(prob)
}

dloop(50,10000)

n<-5 
u <- sample(2*n,2*n)
u_index <- seq(1,2*n)
i<-1

k <- 1
length <-1
while (k != u[k]) {
  k <- u[k]
  if(u_index[1] == u[k]){break}
  length <- length+1
  }
length
i <- i+1
freq[length] <- freq[length]+1




# a new dloop function from zmy

dloop <- function(n,nreps=10000){
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
