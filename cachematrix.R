## This couple of function create an special matrix and cache its inverse
## in order to decrease computational cost
## functions do :

## 1)makeCacheMatrix 2)cacheSolve

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  INVMat=NULL # Initial value of inverse matrix is set as N
  
  set <- function(y) {
    x <<- y   # Assign to X the value of the matrix on parent ENV
    INVMat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) INVMat <<- inverse # Assign INVMat -->Parent environment
  getInverse <- function() INVMat  # Get the current existing value of INVMat if called
  list(set = set, get = get, #Define functions as list and the use $ to make reference
       setInverse = setInverse,
       getInverse = getInverse)
  
  
  }


## This function retrieve cached matrix if exist. If it doesn´t then calculates inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  INVMat <- x$getInverse() # Assign to INVMat the current cached value
  if(!is.null(INVMat)) {
    message("getting cached data")
    return(INVMat)
  }
  data <- x$get()
  INVMat <- solve(data, ...)
  x$setInverse(INVMat)
  INVMat
  
  
}