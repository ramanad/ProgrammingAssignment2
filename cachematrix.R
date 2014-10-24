## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


