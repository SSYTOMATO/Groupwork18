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
  func_k <- func(theta,...)
  grad_k <- grad(theta,...)
  if (is.NULL(hess)){
    hess_k <- hessian(theta,grad,...)
  }else{hess_k <- hess(theta,...)}
  
  if (sum(is.finite(func_k))+sum(is.finite(grad_k)) != 
      length(func_k)+length(grad_k)){
    stop('the objective or derivatives are not finite at the initial theta')
  }

    
  while(k <= maxit){
    
    # judge whether theta is just the parameter we want to find
    if (abs(grad_k) < tol*abs(func_k)+fscale){
      break
    } else if (k==maxit){
      stop('maxit is reached without convergence')
    }
    
    
    ## remaining1: whether hess_k is positive definite,if not, perturb it.
    
    ## remaining2: find the step(delta) by delta= -(hess_k)^(-1) %*% grad_k
    
    if (func(theta+delta) >= func_k){
      for (i in 1:max.half){
        delta <- delta/2
        if (func(theta+delta) < func_k){break}
      }
      if (func(theta+delta) >= func_k){
        stop('the step fails to reduce the objective with max.half halvings')
      }
    }
    
    theta <- theta + delta
    k <- k+1
    func_k <- func(theta,...)
    grad_k <- grad(theta,...)
    if (is.NULL(hess)){
      hess_k <- hessian(theta,grad,...)
    }else{hess_k <- hess(theta,...)}
    
  }
  
  # remaining3: check whether final hess is positive definite, warning 4
  # the inverse of the Hessian matrix?
  
  
 c(f=func_k, theta=theta, iter=k, g=grad_k, Hi) 
}  
  
  

