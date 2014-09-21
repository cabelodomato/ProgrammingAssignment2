## The pair functions, makeCacheMatrix and cacheSolve, are used together to make computations 'on the fly' and store(cache) it results;
## as well it retrieve saved values from previous computations, ainming to avoid redundance in the computation.

## makeCacheMatrix returns a list of functions (get,set,getmean,setmean) for the value given as argument,
## while caching the matrix(argument) and its inverse
makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(y) {
    m <<- y
    inverse <<- NULL
  }
  get <- function() m
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve makes use of the list created by makeCacheMatrix.
## It first gets the inverse of matrix given at makeCacheMatrix, and display the result,
## or calculates the inverse matrix given at makeCacheMatrix and stores, given the first request for the inverse matrix were null.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
