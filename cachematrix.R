## makeCacheMatrix creates a special matrix
## it contains functions to set and get the matrix and its inverse
## creating makecachematrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
}
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv, getInv = getInv)}

## cacheSolve helps calculate the inverse
## if it has already been calculated, then retrieve it from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv))  {
    message("getting cached data")
    return(inv)
    
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}