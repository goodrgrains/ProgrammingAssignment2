## This set of two functions, makeCacheMatrix and cacheSolve
## allow for the function to be set with d<-makeCacheMatrix
## then create a matrix with d$set(matrix(c(a, b, c, d), nrow=2, ncol=2))
## then can find the inverse with cacheSolve(d)
## if the inverse of d is in the cache, it gets the
## cached inverse, else it calculates it

## This first function creates a list that is then
## used to set, get, and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x<<-y
    m<<-NULL
  }
  get <-function() x
  setinverse <- function(solve) m<<- solve
  getinverse <- function() m
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function checks for a cached inverse else
## calculates it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
      message("getting cached data")
      return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Returns a matrix that is the inverse of 'x'
}
