## Creates 2 functions to allow caching of results of inverting a matrix

## This function creates an R object that is a list with 4 elements that
## are functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvm <- function(invm) m <<- invm
  getinvm <- function() m
  list(set = set, get = get, setinvm = setinvm, getinvm = getinvm)

}


## This function finds the inverse of a matrix and retrieves a cached value
## if it has already been calculated in order to save compute time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvm(m)
  m
  }
