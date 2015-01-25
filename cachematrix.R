
# This is a generic cache object which has not changed
# much from the original cacheVector. Infact it is generic
# and can be simply a cacheObject. The logic of what is cached
# is contained in cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  solveCache <- NULL
  get <- function() x
  # Set the Matrix, remove any old solved data
  set <- function(y) {
    x <<- y
    solveCache <<- NULL
    x
  }

  # Set the Solve
  setSolve <- function(y) {
    solveCache <<- y
    solveCache
  }

  getSolve <- function() solveCache

  list(get=get, set=set, getSolve=getSolve, setSolve=setSolve)
}


# This function contains the reposnbility of solving the inverse matrix itself
# and updating the cache object with the result. If it is already solved
# then simply return that value.
cacheSolve <- function(x, ...) {
  inverse <- x$getSolve()
  if (!is.null(inverse)) {
    message("Getting Cached Data")
    ## Return a matrix that is the inverse of 'x'
    return(inverse)
  }

  # Inverse must not yet be solved, lets solve and set it
  # Default return here from setSolve will returned the solved value
  x$setSolve(solve(x$get(), ...))
}
