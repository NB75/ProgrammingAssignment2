## Two functions:
## First function to cache matrix to invert and the inverted matrix
## Second function that calculates the inverted matrix. If available in the cache, matrix won't be calculated

## Function to cache the content of a matrix and the correspondent inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## Function that tries to read value of inverted matrix from cache, if available
## If matrix is not available in cache, calculation is performed online

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
