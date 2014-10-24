
##
## makeCacheMatrix(x) 
##    accepts a matrix 'x', and returns a list with the following elements:
##    set(y) ## set a matrix 'y' (this will overwrite the exisiting x)
##    get()  ## returns 'x'
##    compute_inv(...) ## computes inverse of 'x' using the function solve(x, ...)
##    getinv() ## returns the inverse
##
## cacheSolve(x) 
##    accepts a cached matrix 'x', the list object returned by the function makeCacheMatrix
##    and returns the inverse of the matrix obtained by x$get() either by computing it, 
##    or from the cache if it is already computed.

## Example usage:
##   d <- matrix(sample(1:16), ncol=4)
##   dmat <- makeCacheMatrix(d)
##   cacheSolve(dmat)  #### This prints the inverse of d
##   cacheSolve(dmat)  #### This prints the inverse of d from the cache (because it is found above)
##   d %*% cacheSolve(dmat) ### results in unit matrix.
##
##   y <- matrix(sample(17:32), ncol=4)
##   dmat$set(y)
##   cacheSolve(dmat)  ### This should print the inverse of y (NOT from cache)
##   cacheSolve(dmat)  ### This should print the inverse of y from the cache.
##   y %*% cacheSolve(dmat)  ### results in unit matrix
##

makeCacheMatrix <- function(x = matrix()) {
  
  #### the matrix inverse is set to null, when 'x' is assigned for the first time.
  inv <- NULL
  
  #### function resets the matrix 'x', and then its inverse is set to NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  #### calculate inverse, and then update 'inv'
  compute_inv <- function(...){
    inv <<- solve(x, ...)
  }
  
  #### get the matrix:
  get <- function() {
    x
  }
  
  ### get matrix inverse:
  getinv <- function(){
    inv
  }

  list(set = set, get = get, compute_inv = compute_inv, getinv = getinv)
}


## Write a short comment describing this functi

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #### simply get the inverse from the makeCacheMatrix object `x'
    inv <- x$getinv()
    
    #### return the cached inverse, if exists
    if(!is.null(inv)){
      message("getting cached inverse")
      return(inv)
    }
    
    ### If not, find the inverse
    my_dat <- x$get()
    inv <- x$compute_inv(...)
    
    inv
}


