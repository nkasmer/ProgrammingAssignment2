## In this assignment I write a pair of functions that cache the inverse 
## of a matrix


## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  cm  <- NULL
  set  <- function(y){
    x <<- y
    cm <<- NULL 
  }
  get  <- function() x
  setinv  <- function(inv) cm  <<- inv
  getinv  <- function() cm
  list(set= set, get = get, 
       setinv = setinv, 
       getinv = getinv)
  
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  cm  <- x$getinv()
  if (!is.null(cm)){
    message("getting cached data")
    return(cm)
  }
  data  <- x$get()
  cm  <- solve(data, ...)
  x$setinv(cm)
  cm
}
