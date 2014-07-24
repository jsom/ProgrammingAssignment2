## Expensive calculations can be cached so that subsequent
## requests for the same calculation will be much faster.

## makeCacheMatrix is initialized with a matrix and
## provides functions to cache the result of finding
## the inverse of that matrix.
## set: sets the stored matrix x and 
## get: returns the stored matrix x
## setInverse: caches the inverse of the matrix x
## getInverse: returns the cached inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
  invMatrix = NULL
  set = function(newMatrix) {
    x <<- newMatrix
    invMatrix <<- NULL
  }
  get = function() x
  setInverse = function(newInvMatrix) invMatrix <<- newInvMatrix
  getInverse = function() invMatrix
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## cacheSolve takes a matrix wrapped in the makeCacheMatrix function.
## If the inverse matrix is not already calculated, it solves it
## and stores it in the makeCacheMatrix object and returns
## the inverse matrix.
## If the inverse matrix is already calculated, it returns the
## inverse matrix

cacheSolve <- function(x, ...) {
  invMatrix = x$getInverse()
  if(is.null(invMatrix)) {
    invMatrix = solve(x$get())
    x$setInverse(invMatrix)
  }
  invMatrix
}