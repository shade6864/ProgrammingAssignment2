## Those 2 functions are used to create and use a matrix like object which is 
## able to cache its inverse


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- matrix(data=NA)
  
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  
  get <- function() x
  
  setinv <- function(invert) inv <<- invert
  
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves the inverse
## from the cache.

## If the matrix is not square then a warning message is displayed and 
## a null value is returned. 

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  if(!is.na(inv)[1,1]) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  if(dim(data)[1] != dim(data)[2] ) {
    message("Warning : this is not a square matrix - null value is returned")
    return()
  }
  
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}