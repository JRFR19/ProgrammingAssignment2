makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setMatrix=setMatrix, getMatrix=getMatrix, setinverse=setinverse, getinverse=getinverse)
}


_____________________________________________________
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}