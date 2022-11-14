# Group 18
# Member: Shangyun Sun(S2330859); Ziyi Gu(S2307232); Mengyao Zhang(S2049018).

# Github repo: https://github.com/SSYTOMATO/Groupwork18.git


#-------------------------------------------------------------------------------

# find hessain matrix

hessain <- function(theta,grad,...,eps=1e-6){
  Hfd <- matrix(0,length(theta),length(theta))
  gll0 <- grad(theta,...)
  for(i in i:length(theta)){
    th1 <- theta
    th1[i] <- th1[i] + eps
    gll1 <- grad(th1,...)
    Hfd[i,] <- (gll1 - gll0)/eps
  }
  Hfd
}



newt <- function(theta,func,grad,hess=NULL,...,tol=1e-8,fscale=1,
                 maxit=100,max.half=20,eps=1e-6){
  k <- 0
  
  while(k <= maxit){

    func_k <- func(theta,...)
    grad_k <- grad(theta,...)
    if (is.NULL(hess)){
      hess_k <- hessian(theta,grad,...)
    }else{hess_k <- hess(theta,...)}
    
    if (k == 0){
      ## warnings 1 needs adding
    }
    
    ## judge whether theta is just the parameter we want to find
    ## if it is, break
    ## if it is not and k = maxit, warning 3
    
    
    ## whether hess_k is positive definite,if not, perturb it.
    
    ## find the step(delta) by delta= -(hess_k)^(-1) %*% grad_k
    
    if (func(theta+delta) >= func_k){
      for (i in 1:max.half){
        delta <- delta/2
        if (func(theta+delta) < func_k){break}
      }
      if (func(theta+delta) >= func_k){
        # warning 2 needs adding
      }
    }
    
    theta <- theta + delta
    k <- k+1
    
  }
  
  # for final theta, check whether hess is positive definite, warning 4
  # the inverse of the Hessian matrix?
  
  
 c(f=func_k, theta=theta, iter=k, g=grad_k, Hi) 
}  
  
  
  
  
  
'''
  
  function_value <- func(theta,...)#function value at initial parameter values
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
  
'''  
    
  

