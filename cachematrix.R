## This pair of two functions are to create a special object that stores a matrix and its inverse.

## The CacheMatrix is to create a matrix object which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(invrsfun) invrs <<- invrsfun
  getinverse <- function() invrs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve is to compute the inverse matrix of the one created by the CacheMatrix function.
## If the inverse has already been calculated, it will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting the cached inverse matrix")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinverse(invrs)
  invrs
}
