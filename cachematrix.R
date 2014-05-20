## This function is used to store the data for matrix and it's inverse.
## It defines 4 functions, set(), get(), setinverse() and getinverse()
## to retrieve or store the data for matrix and it's inverse respectively.
## To execute the code follow the example below:
## a <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(a)
## cacheSolve(a) again to verify result is being retrieved from cache.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(z) m <<- z
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This function computes the inverse of the matrix data returned
## by makeCacheMatrix above. If the inverse has already been calculated
## and the matrix has not changed then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
  m
}