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

# This code is to find the parameter(theta)
# which can minimize the objective function(func) by Newton's method. 
# Starting from a guess at the parameter vector and finding the quadratic
# function matching the objective function and its first and second derivatives
# (grad and hess respectively),
# we then minimize the quadratic to find an improved guess 
# and repeat the process until the first derivative is sufficiently close to 0.
# Additionally, we want to ensure that each new theta has actually reduced func,
# and that the approximating quadratic has a proper minimum.

#-------------------------------------------------------------------------------

# Firstly, create a function to approximate the Hessian 
# by finite differencing of the gradient vector

hessian <- function(theta,grad,...,eps=1e-6){
  
# Function hessian is used to approximate the hessian matrix
# Input: theta (the optimization parameters of the function)
#        grad (the gradient function, which will be used to find hessian matrix)
# Output: Hfd (the hessian matrix)
  
  Hfd <- matrix(0,length(theta),length(theta))  # create an empty matrix
  gll0 <- grad(theta,...)  # the value of gradient function at theta
  for(i in 1:length(theta)){
    th1 <- theta  # initial values of the optimization parameters
    th1[i] <- th1[i] + eps  # increase th1[i] by eps
    gll1 <- grad(th1,...)  # compute resulting gradient
    Hfd[i,] <- (gll1 - gll0)/eps  # approximate second derives
  }
  (t(Hfd)+Hfd)/2  # symmetric Hfd
}

#-------------------------------------------------------------------------------

newt <- function(theta,func,grad,hess=NULL,...,tol=1e-8,fscale=1,
                 maxit=100,max.half=20,eps=1e-6){
  
# Function newt is used to implement optimization process by Newton's method,
# finding the theta which minimizes the value of the objective function
# and other useful information.

# Input: theta (a vector of initial values for the optimization parameters)
#        func (the objective function to minimize)
#        grad (the gradient function with the same arguments as func returning  
#              the gradient vector of the objective function w.r.t. the elements 
#              of parameter vector,)
#        hess (the Hessian matrix function with the same arguments as func 
#              returning the Hessian matrix of the objective function w.r.t. the 
#              elements of parameter vector. If not provided, it is obtained by
#              finite difference of the gradient vector)
#        ... (used to pass any arguments of func, grad and hess after the
#             parameter vector)
#        tol (convergence tolerance)
#        fscale (estimate of magnitude of func near the optimum)
#        maxit (maximum number of Newton iterations)
#        max.half (maximum number of times of halving process before concluding 
#                  that it fails to improve the objective function)
#        eps (the finite difference interval)
#
  
# Output: a list containing the minimum value of objective function(f),
#         the corresponding parameter vector(theta),
#         number of iterations(iter), 
#         the gradient vector(g) and
#         the inverse of Hessian matrix(Hi) if it is positive definite.
  
  k <- 0                          # number of iterations with initial value 0
  func_k <- func(theta,...)       # objective function at theta
  grad_k <- grad(theta,...)       # gradient function at theta
  
  if (is.null(hess)){             # judge whether hess function is provided 
    hess_k <- hessian(theta,grad,...) # (if not) obtain one by hessian function;
  }else{hess_k <- hess(theta,...)}  # (or) using the given one
  
  # issue error: the objective or derivatives are not finite at initial theta
  if (sum(is.finite(func_k))+sum(is.finite(grad_k)) != 
      length(func_k)+length(grad_k)){
    stop('the objective or derivatives are not finite at the initial theta')
  }  

  
  while(k <= maxit){  # iterate at most maxit times
    
    # whether it is convergence at this theta. If it is, break the while loop
    if (sum(abs(grad_k) < tol*(abs(func_k)+fscale))==length(grad_k)){
      break
    } else if (k==maxit){  # maximum number of iteration is achieved 
      stop('maxit is reached without convergence') # issue error
    }
    
    
    # compute delta using + def hessian(if not, perturb it)
    
    hess_ori <- hess_k  # store the last hessian matrix 
    multiple <- 1e-6    # set the multiple
    norm <- norm(hess_ori,type = 'I')  # the norm of hessian
    check <- try(chol(hess_k), silent=TRUE)  # check if hessian matrix is + def
    while(inherits(check,'try-error')){  # if hessian matrix is not + def
      hess_k <- hess_ori + multiple*norm  # perturb the hessian matrix
      check <- try(chol(hess_k), silent=TRUE) # check the new hessian
      multiple <- multiple*10  # multiply the multiplier by 10
    } 
    delta <- -chol2inv(chol(hess_k))%*%grad_k  # compute delta value
    
    
    # determine delta which can be used to improve func
    
    if (func(theta+delta) >= func_k){  # if the function is not improved 
      for (i in 1:max.half){           # loop to find the proper delta
        delta <- delta/2               # halve the delta value
        # if function is not finite, go to next loop
        if (sum(is.finite(func(theta+delta)))!=length(func(theta+delta))){next}
        # if function has been improved, then break the loop
        if (func(theta+delta)<func_k){break}
      }
      
      # if function is not improved after max.half steps, issue error
      if (func(theta+delta)>=func_k){ 
        stop('the step fails to reduce the objective with max.half halvings')
      }
      # if function is not finite after max.half steps, issue error
      else if(sum(is.finite(func(theta+delta)))!=length(func(theta+delta))){
        stop('the step fails to find the finite objective with max.half halvings')
      }
    }
    
    theta <- theta + delta  # update theta using delta
    k <- k+1                # the number of iterations added
    
    # for new theta, calculate the objective function and its derivatives
    func_k <- func(theta,...)
    grad_k <- grad(theta,...)
    if (is.null(hess)){
      hess_k <- hessian(theta,grad,...)
    }else{hess_k <- hess(theta,...)}
    
  }
  
  # after while loop (only those reaching convergence,i.e. break at line102, can
  # continue with the following codes)
  # find whether hessian is positive definite at convergence
  # if it not, give warning message
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

