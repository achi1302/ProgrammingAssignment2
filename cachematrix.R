# Creates a special "matrix" object that can cache its inverse.
# Checks if the variable is a matrix and if it is squared.
# Provides methods to set/get the matrix and set/get the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  if (!is.matrix(x)) {
    stop("Input must be a matrix")
  }
  if (nrow(x) != ncol(x)) {
    stop("Matrix should be square")
  }
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# Computes the inverse of the special "matrix" created by the previous function.
# Retrieves the inverse from the cache if available; otherwise, 
# computes and caches it.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
