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

Pone <- function(n,k,strategy,nreps){
  for (rep in 1:nreps){
    box <- sample(1:2*n,2*n)
    success <- 0
    # strategy 1
    success <- success + Unit(n,k,k,box)
    # strategy 2
    first <- sample(1:2*n,1)
    success <- success + Unit(n,k,first,box)
    # strategy 3
    choice <-sample(1:2*n,n)
    success <- success + k %in% choice
  }
  success/nreps
}
  

