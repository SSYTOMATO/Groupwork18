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
  
  if (sum(is.infinite(func_k))+sum(is.infinite(grad_k)) != 0 ){
    stop('the objective or derivatives are not finite at the initial theta')
  }

    
  while(k <= maxit){
    
    # judge whether theta is just the parameter we want to find
    if (abs(grad_k) < tol*abs(func_k)+fscale){
      break
    } else if (k==maxit){
      stop('maxit is reached without convergence')
    }
    
    
    multiple <- 1e-6
    hess_ori <- hess_k
    norm_matrix <- norm(hess_ori,type = 'I')
    check <- try(chol(hess_k))
    while (inherits(check,'try-error')){
      hess_k <- hess_ori + multiple*norm_matrix
      check <- try(chol(hess_k))
      multiple <- multiple*10
    } 
  
    delta <- -chol2inv(chol(hess_k))%*%grad_k
    
    if (func(theta+delta) >= func_k){
      for (i in 1:max.half){
        delta <- delta/2
        if (sum(is.infinite(func_k))!=0){next}
        if (func(theta+delta)<func_k){break}
      }
      if (func(theta+delta)>=func_k | sum(is.infinite(func_k))!=0){
        stop('the step fails to reduce the objective with max.half halvings')
      }
    }
    
    theta <- theta + delta
    k <- k+1
    func_k <- func(theta,...)
    grad_k <- grad(theta,...)
    if (sum(is.finite(func_k))!= length(func_k)){
      
    }
        
    
    if (is.NULL(hess)){
      hess_k <- hessian(theta,grad,...)
    }else{hess_k <- hess(theta,...)}
    
  }
  

  check <- try(chol(hess_k))
  if (inherits(check,'try-error')){
    stop('the Hessian is not positive definite at convergence')
  }
  Hi <- chol2inv(chol(hess_k))
  
  
  
 c(f=func_k, theta=theta, iter=k, g=grad_k, Hi=Hi) 
}  
  
