## Put comments here that give an overall description of what your
## functions do

## function 'makeCacheMatrix' set the value of the matrix then get the value
## of the matrix and set the value of the inverse of the matrix then get
## the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## function 'cacheSolve' compute the inverse of the matrix above and 
## retrieve the inverse if it already exists

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
