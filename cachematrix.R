## These two functions solve for the inverse of a matrix, cache the solution
## so that the solve function does not have to be run again, and return the solution
## if needed again.

## makeCacheMatrix returns a list of 4 functions, one of which solves the inverse of a matrix.

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve takes the cached matrix (or list) passed by makeCacheMatrix and returns its inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
