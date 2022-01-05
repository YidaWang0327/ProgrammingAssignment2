## makeChacehMatrix() function creates a matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function()inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() function will computes the inverse of makeCacheMatrix if the result is not computed yet
## And if the Cache is already computed, will return the inverse of the cache directly

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}
