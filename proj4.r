# Group 18
# Member: Shangyun Sun(S2330859); Ziyi Gu(S2307232); Mengyao Zhang(S2049018).

# Github repo: https://github.com/SSYTOMATO/Groupwork18.git


#-------------------------------------------------------------------------------

# find hessain matrix

hessian <- function(theta,grad,...,eps=1e-6){
  Hfd <- matrix(0,length(theta),length(theta))
  gll0 <- grad(theta,...)
  for(i in 1:length(theta)){
    th1 <- theta
    th1[i] <- th1[i] + eps
    gll1 <- grad(th1,...)
    Hfd[i,] <- (gll1 - gll0)/eps
  }
  (t(Hfd)+Hfd)/2
}



newt <- function(theta,func,grad,hess=NULL,...,tol=1e-8,fscale=1,
                 maxit=100,max.half=20,eps=1e-6){
  k <- 0
  func_k <- func(theta,...)
  grad_k <- grad(theta,...)
  if (is.null(hess)){
    hess_k <- hessian(theta,grad,...)
  }else{hess_k <- hess(theta,...)}
  
  if (sum(is.infinite(func_k))+sum(is.infinite(grad_k)) != 0 ){
    stop('the objective or derivatives are not finite at the initial theta')
  }

    
  while(k <= maxit){
    
    # judge whether theta is just the parameter we want to find
    if (sum(abs(grad_k) < tol*(abs(func_k)+fscale))==length(grad_k)){
      break
    } else if (k==maxit){
      stop('maxit is reached without convergence')
    }
    
    
    hess_ori <- hess_k
    multiple <- 1e-6
    norm <- norm(hess_ori,type = 'I')
    check <- try(chol(hess_k), silent=TRUE)
    while(inherits(check,'try-error')){
      hess_k <- hess_ori + multiple*norm
      check <- try(chol(hess_k), silent=TRUE)
      multiple <- multiple*10
    } 
  
    delta <- -chol2inv(chol(hess_k))%*%grad_k
    
    if (func(theta+delta) >= func_k){
      for (i in 1:max.half){
        delta <- delta/2
        if (sum(is.infinite(func(theta+delta)))!=0){next}
        if (func(theta+delta)<func_k){break}
      }
      if (func(theta+delta)>=func_k | sum(is.infinite(func(theta+delta)))!=0){
        stop('the step fails to reduce the objective with max.half halvings')
      }
    }
    
    theta <- theta + delta
    k <- k+1
    func_k <- func(theta,...)
    grad_k <- grad(theta,...)

    if (is.null(hess)){
      hess_k <- hessian(theta,grad,...)
    }else{hess_k <- hess(theta,...)}
    
  }
  

  check <- try(chol(hess_k), silent=TRUE)
  if (inherits(check,'try-error')){
    result <- list(f=func_k, theta=theta, iter=k, g=grad_k)
    warning('the Hessian is not positive definite at convergence')
  }else{
    Hi <- chol2inv(chol(hess_k))
    result <- list(f=func_k, theta=theta, iter=k, g=grad_k, Hi=Hi)
  }
  
 result
}  

th <- c(-2, -4)
rb <- function(th,k=2) {
  k*(th[2]-th[1]^2)^2 + (1-th[1])^2
}
gb <- function(th,k=2) {
  c(-2*(1-th[1])-k*4*th[1]*(th[2]-th[1]^2),k*2*(th[2]-th[1]^2))
}
hb <- function(th,k=2) {
  h <- matrix(0,2,2)
  h[1,1] <- 2-k*2*(2*(th[2]-th[1]^2) - 4*th[1]^2)
  h[2,2] <- 2*k
  h[1,2] <- h[2,1] <- -4*k*th[1]
  h
}
th0 <- c(10,.1)
newt(th,rb,gb)
newt(th,rb,gb,hess=hb)
