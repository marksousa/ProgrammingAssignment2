## The two functions below can save a lot of processing time and 
## computational resource. The caching is very useful in a loop because 
## can saves a lot of time in the calculation. The solution of this 
## ProgrammingAssignment2, is based on the example of Caching The Mean of
## a vector, with some adjust for matrices and their inverse.

## The function makeCacheMatrix returns a list containing four functions to
## get and set matrices and their inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## x must to be a square and invertible matrix.
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setInverse <- function (inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## The function cacheSolve returns the inverse of the matrix x using 
## the caching.
cacheSolve <- function(x, ...) {

  i <- x$getInverse()
  if(!is.null(i)){
    message("Getting cached data!")
    return (i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
