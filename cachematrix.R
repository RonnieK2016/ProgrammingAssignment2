## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setInverse <- function(invm) cache <<- invm
  getInverse <- function() cache
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getInverse()
  if(!is.null(invm)) {
    message("cached data returned")
    return(invm)
  }
  mat <- x$get()
  invm <- solve(mat,...)
  x$setInverse(invm)
  invm
}