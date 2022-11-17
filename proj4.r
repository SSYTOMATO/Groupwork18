# Group 18
# Member: Shangyun Sun(S2330859); Ziyi Gu(S2307232); Mengyao Zhang(S2049018).

# Github repo: https://github.com/SSYTOMATO/Groupwork18.git

# Shangyun Sun: wrote function hessian and codes related to positive definite
#               and judgment about infinity.
# Ziyi Gu: wrote our own functions to test the validity and efficiency of codes 
#          and added comments.
# Mengyao Zhang: wrote the framework of the code, including loops and
#                some if statements.
# Everyone undertook roughly 1/3 of the work.

#-------------------------------------------------------------------------------

# This code is to create an R function, newt, to implement Newton's method for 
# minimization of functions. 


# find hessian matrix

hessian <- function(theta,grad,...,eps=1e-6){
  Hfd <- matrix(0,length(theta),length(theta)) # create an empty matrix
  gll0 <- grad(theta,...)# gradient of the objective function at theta
  for(i in 1:length(theta)){
    th1 <- theta # initial values for the optimization parameters
    th1[i] <- th1[i] + eps # increase th1[i] by eps
    gll1 <- grad(th1,...) # compute resulting gradient
    Hfd[i,] <- (gll1 - gll0)/eps # approximate second derives
  }
  (t(Hfd)+Hfd)/2 # symmetric Hfd
}



newt <- function(theta,func,grad,hess=NULL,...,tol=1e-8,fscale=1,
                 maxit=100,max.half=20,eps=1e-6){
# Function newt is used to implement the optimization process and find the 
# minimum value of the objective function and corresponding parameters.

# Input: theta (a vector of initial values for the optimization parameters)
#        func (the objective function to minimize)
#        grad (the gradient function with the same arguments as func returning  
#              the gradient vector of the objective function w.r.t. the elements 
#              of parameter vector,)
#        hess (the Hessian matrix function with the same arguments as func 
#              returning the Hessian matrix of the objective function w.r.t. the 
#              elements of parameter vector. If not provided, it is obtained by
#              finite difference of the gradient vector)
#        . . . (used to pass any arguments of func, grad and hess after the
#               parameter vector)
#        tol (convergence tolerance)
#        fscale (estimate of magnitude of func near the optimum)
#        maxit (maximum number of Newton iterations)
#        max.half (maximum number of times of halving process before concluding 
#                  that it fails to improve the objective function)
#        eps (the finite difference interval)
#
  
# Output: a list containing the minimum value of objective function, the
#         corresponding parameter vector, number of iterations, the gradient
#         vector and the inverse of Hessian matrix if it is positive definite.
  
  k <- 0                          # number of iterations with initial value 0
  func_k <- func(theta,...)       # objective function at theta
  grad_k <- grad(theta,...)       # gradient function at theta
  if (is.null(hess)){             # judge whether hess function is provided 
    hess_k <- hessian(theta,grad,...) # let Hessian matrix be the function we defined
  }else{hess_k <- hess(theta,...)}  # let Hessian matrix be the function provided
  
  # issue error
  if (sum(is.finite(func_k))+sum(is.finite(grad_k)) != length(func_k)+length(grad_k)){
    stop('the objective or derivatives are not finite at the initial theta')
  }  

    
  while(k <= maxit){ # loop for every iteration
    
    # judge whether theta is just the parameter we want to find
    if (sum(abs(grad_k) < tol*(abs(func_k)+fscale))==length(grad_k)){
      break
    } else if (k==maxit){ # maximum number of iteration is achieved 
      stop('maxit is reached without convergence') # issue error
    }
    
    
    hess_ori <- hess_k # store the original hessian matrix 
    multiple <- 1e-6   # set the multiplier 
    norm <- norm(hess_ori,type = 'I') # 
    check <- try(chol(hess_k), silent=TRUE) # check if hessian matrix is + def
    while(inherits(check,'try-error')){ # if hessian matrix is not + def
      hess_k <- hess_ori + multiple*norm # perturb the hessian matrix
      check <- try(chol(hess_k), silent=TRUE) 
      multiple <- multiple*10 # multiply the multiplier by 10
    } 
  
    delta <- -chol2inv(chol(hess_k))%*%grad_k # compute delta value
    
    if (func(theta+delta) >= func_k){ # if the function is not improved 
      for (i in 1:max.half){ # loop to improve the function
        delta <- delta/2 # halve the dalta value
        if (sum(is.infinite(func(theta+delta)))!=0){next} # if function is not
                                                       # finite, go to next loop 
        if (func(theta+delta)<func_k){break} # if function is improved, no need 
                                             # to continue
      }
      if (func(theta+delta)>=func_k){ 
        # if function not improved after max.half steps, issue error
        stop('the step fails to reduce the objective with max.half halvings')
      }else if(sum(is.finite(func(theta+delta)))!=length(func(theta+delta))){
        # if function not finite after max.half steps, issue error
        stop('the step fails to find the finite objective with max.half halvings')
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
