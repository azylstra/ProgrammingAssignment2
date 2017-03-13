## Utilities for working with matrices
## Create and use a list wrapper for a special matrix that caches its inverse

## Generate a new CacheMatrix, return list has five elements
## data     numeric matrix
## inv      inverse of data, or NULL
## get      function that returns data
## setinv   set the inverse of data
## getinv   get the inverse of data (can be NULL)
makeCacheMatrix <- function(x = matrix()) {
  data <- x
  inv <- NULL
  set <- function(y) {
    data <<- y
    inv <<- NULL
  }
  get <- function() data
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(data=data, inv=inv, get=get, setinv=setinv, getinv=getinv)
}


## Get the inverse of x, which is returned.
## Prints a message if cached data is used
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Using cached inverse matrix")
    return(inv)
  }
  temp <- x$get()
  inv <- solve(temp)
  x$setinv(inv)
  inv
}
