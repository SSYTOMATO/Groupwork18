# Group 18
# Member: Shangyun Sun(S2330859); Ziyi Gu(S2307232); Mengyao Zhang(S2049018).

# Github repo: https://github.com/SSYTOMATO/Groupwork18.git


#-------------------------------------------------------------------------------
# find hessain matrix:
'''
Hfd <- matrix(0,length(theta),length(theta))
gll0 <- grad(theta)
for(i in i:length(theta)){
  th1 <- theta
  th1[i] <- th1[i] + eps
  gll1 <- func(th1)
  Hfd[i,] <- (gll1 - gll0)/eps
}

'''


newt <- function(theta,func,grad,hess=NULL,...,tol=1e-8,fscale=1,
                 maxit=100,max.half=20,eps=1e-6){
  function_value <- func(theta,...)#function value at initial parameter values
  theta <- c(theta) # a vector of parameter value
  gll <- c(0)
  number_not_ruduce = 0
  
  for (i in 1:maxit+1) {
    Hfd <- matrix(0,length(theta[1]),length(theta[1])) # empty hessain matrix
    
    gll[i] <- grad(theta[i]) #find the gradient of theta[i]
    for(i in 1:length(theta[1])){
      # use gll[i] to find the second derivative
      th1 <- theta
      th1[i] <- th1[i] + eps
      gll1 <- func(th1)
      Hfd[i,] <- (gll1 - gll0)/eps
    }
    theta[i+1] <- theta[i] - gll[i]/Hfd #find a new parameter matrix
    if(theta[i+1] >= theta[i]){
      number_not_ruduce = number_not_ruduce +1
    }
    i = i+1
    
    
   }
  
  
    
}
  

