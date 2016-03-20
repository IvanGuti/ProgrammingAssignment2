## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Function cacheSolve  calculate the inverse of matrix makeCacheMatrix function. 
##If the inverse have been calculated the cachesolve retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

