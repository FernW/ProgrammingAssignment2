## These functions will cache the inverse of a matrix so it does not have to be
## repeatedly calculated, since this is a computationally expensive function.

## This function takes a matrix (which must be square) and calculates its
## inverse, if it has not already been calculated.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## If the inverse has already been calculated, this function simply returns it.
## If the inverse has not been calculated, this function calculates and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
