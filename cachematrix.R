## Caching the inverse of a matrix rather than compute it repeatedly
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(mtrx) {
	x <<- mtrx
	inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(set = set , get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
	message("getting cached data")
	return(x$getinverse())
  }
  
  inverse <- solve(x$get())
  x$setinverse(inverse)
  
  inverse
}
