## Put comments here that give an overall description of what your
## functions do

## function will create matrix with specific functions to get and set data
## as well as setting and getting cached inversions

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  
  ##funciton to set data
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  ##returns current matrix data
  get <- function() x
  
  ##setting inversion
  setInverse <- function(invm) cache <<- invm
  
  ##getting cached inversion
  getInverse <- function() cache
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function return cached inversion of 'x' if available
## if cache is not available, inversion will be computed and stored to cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getInverse()
  if(!is.null(invm)) {
    message("cached data returned")
    return(invm)
  }
  
  ##compute inversion and store to cache
  mat <- x$get()
  invm <- solve(mat,...)
  x$setInverse(invm)
  invm
}