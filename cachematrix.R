## Put comments here that give an overall description of what your
## functions do

## Creates matrix object with cached inverse with getter, setter.
## Returns a list with results.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(entry){
    x <<- entry
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of CacheMatrix object if not already calculated

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}