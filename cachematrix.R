## The functions here check if the inverse of a certain Matrix had been calculate with the function
## solve and return it from cache if it has; if it hasn't, it calculates the inverse

## makeCacheMatrix is a function that stores a list of functions to retrieve and change the 
## original data, and to set and retrieve the inverse Matrix of the input

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


## cacheSolve search in the cache if the inverse of a matrix has been calculated, if it hasn't
## it calculates it and return it

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
