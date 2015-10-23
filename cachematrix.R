# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setting <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getting <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  list(setting=setting, getting=getting, set_inv=set_inv, get_inv=get_inv)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  dat <- x$getting()
  inv <- solve(dat)
  x$set_inv(inv)
  inv
}