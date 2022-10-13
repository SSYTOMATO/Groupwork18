# Group 18
# Member: Shangyun Sun(S2330859); Ziyi Gu(S2307232); Mengyao Zhang(S2049018).

# Github repo: https://github.com/SSYTOMATO/Groupwork18.git

# Shangyun Sun (some contributions).
# Ziyi Gu (some contributions).
# Mengyao Zhang (some contributions).
# Each of us double-checked and optimized others' codes.  
# Everyone undertook roughly 1/3 of the work.

#-------------------------------------------------------------------------------
# Overview






#-------------------------------------------------------------------------------
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
Pall <- function(n,strategy,nreps=10000){
  random_number <- sample(1:10000,1)
  prob <- 1
  list1 <- rep(0,2*n)
  for (k in 1:2*n){
    set.seed(random_number)
    prob <- prob * Pone(n,k,strategy,nreps)
    list1[k] <- prob
  }
  list(list1, prob)
}


# n = 5 matrix, row-strategy, column-indi/joint
matrix_n5 <- matrix(0,3,2)
for (strategy in 1:3){
  matrix_n5[strategy,1] <- Pone(5,1,strategy)
  matrix_n5[strategy,2] <- Pall(5,strategy)
}

# n = 50 matrix, row-strategy, column-indi/joint
matrix_n50 <- matrix(0,3,2)
for (strategy in 1:3){
  matrix_n50[strategy,1] <- Pone(50,1,strategy)
  matrix_n50[strategy,2] <- Pall(50,strategy)
}

Pone(50,1,3)
Pall(50,2)

random_number <- sample(1:10000,1)
prob <- 1
for (k in 1:100){
  set.seed(random_number)
  prob <- prob * Pone(50,k,3)
  print(prob)
}



