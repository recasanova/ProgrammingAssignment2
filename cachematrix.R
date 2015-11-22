## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(b) {
    x <<- b
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(e, ...) {
        ## Return a matrix that is the inverse of 'x'
  d <- e$getInverse()
  if(!is.null(d)) {
    message("getting cached data")
    return(d)
  }
  data <- e$get()
  d <- solve(data)
  e$setInverse(d)
  return(d)
}
