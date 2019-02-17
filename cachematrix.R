## The group of functions below creates a system that allows creation and use of a cached
## matrix object for inverse computation. The implementation 'binds' certain function calls 
## to the wrapped matrix. All manipulations around setting and getting the wrapped matrix, 
## as well as setting and getting its inverse is defined using functions, which are components
## of a list returned as a result of makeCacheMatrix function.
## Another function cacheSolve uses the list with these functions as members to return inverse
## of a given matrix if already computed, or set an inverse if not already computed.

## The function takes a (optional) matrix as an argument and returns a list of following functions.
## A function "set" to set the wrapped matrix on the cached matrix object, "get" to get the wrapped matrix object, 
## "setInverse" function helps set the inverse component of the cached matrix object. Similarly,
## "getInverse" helps get whatever value has been set on the inverted component of this object.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(argInverse) inverse <<- argInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function takes as an input special cached matrix object (created using makeCacheMatrix function)
## It checks if an inverse has already been set on the cached matrix object. If yes, it returns
## the cached value. If no cached value is found, it compute the inverse using 'solve' function
## and set the cached value on the cached matrix object, and returns the same.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
