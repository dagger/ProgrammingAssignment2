## Functions for caching the expensive operation of solving (inverting) a 
## matrix

## Returns a list of which members are used to associate a matrix with its
## precomputed inverse. The cached response is initially NULL, and will be
## set to a value when the list is passed to cacheSolve for the first time.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Takes a list produced by makeCacheMatrix, and tries to see if it has a 
## Cached inverse. If a cached value is not present, it will calculate and 
## store one in the list for future reference, or caching.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
